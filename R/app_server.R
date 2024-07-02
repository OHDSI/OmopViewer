#' Server Function for Shiny Application
#'
#' This function contains the server logic for a Shiny application designed to manage and
#' display data studies. It handles file uploads, data processing, and UI updates based
#' on the data state.
#'
#' @param input Standard Shiny server input parameter
#' @param output Standard Shiny server output parameter
#' @param session Standard Shiny session parameter

#' @return The function sets up a series of reactive expressions and
#'   observers to manage app state.
app_server <- function(input, output, session) {
  # Configuration for plot types and their corresponding modules
  plot_config <- data.frame(
    plot_type = c("cohort_overlap", "summarised_characteristics",
                  "summarised_large_scale_characteristics", "cohort_timing",
                  "cohort_timing_density"),
    module_ui = c("graphCohortOverlap_ui", "graphSummarisedCharacteristics_ui",
                  "graphLargeScaleCharacteristics_ui", "graphCohortTiming_ui",
                  "graphCohortTimingDensity_ui"),
    module_server = c("graphCohortOverlap_init_server", "graphSummarisedCharacteristics_init_server",
                  "graphLargeScaleCharacteristics_init_server", "graphCohortTiming_init_server",
                  "graphCohortTimingDensity_init_server"),
    stringsAsFactors = FALSE
  )


  options(shiny.maxRequestSize=30*1024^3)
  dataFolder <- "extras/data"
  summarisedResultColumns <- c(
    "result_id", "cdm_name", "group_name",
    "group_level", "strata_name", "strata_level",
    "variable_name", "variable_level", "estimate_name",
    "estimate_type", "estimate_value", "additional_name",
    "additional_level"
  )

  # Initialize the uploadData module
  uploadData_init_server("uploadData", dataFolder)

  # Monitor changes in the studies.rds file
  studies_path <- file.path(here::here(dataFolder), "studies.rds")
  studies <- shiny::reactiveFileReader(1000, session, studies_path, readRDS)

  # Function to create dynamic sidebar menu based on cohort_data


  # Refresh modules when studies.rds changes
  shiny::observe({
    # print("Studies updated")  # Debug print
    studies()
    session$userData$studySelect_upload <- studySelect_init_server("available_studies_upload")
    session$userData$studySelect <- studySelect_init_server("available_studies")
    session$userData$app_data <- dataLoad_myModuleServer("dataLoad", dataFolder)
  })

  cohort_data <- shiny::reactive({
    session$userData$app_data$loaded_data_value()
  })

  process_data <- shiny::reactive({

    datasets <- cohort_data()

    print(names(datasets))
    # List for datasets missing required columns
    missing_required_columns <- list()

    # List for datasets with all required columns
    complete_datasets <- list()

    # Iterate over each dataset to classify
    names <- names(datasets)
    print(names)

    for (i in seq_along(names)) {
      data <- datasets[[i]]
      if (all(summarisedResultColumns %in% colnames(data))) {
        complete_datasets[[length(complete_datasets) + 1]] <- data
      } else {
        missing_required_columns[[names[i]]] <- data  # Store with dataset name as key
      }
    }

    # Combine datasets with all required columns into one dataframe
    combined_data <- do.call(rbind, complete_datasets)

    # Return both results as a list
    list(complete = combined_data, incomplete = missing_required_columns)
  })


  shiny::observe({
    # # Dynamic sidebar
    output$dynamic_sidebar <- shinydashboard::renderMenu({
      sidebarMenuDynamic(process_data()$complete, process_data()$incomplete)
    })

    # Dynamic tabs
    output$dynamic_tabs_output <- shiny::renderUI({
      static_tabs <- list(
        shinydashboard::tabItem("About", aboutTab()),
        shinydashboard::tabItem("UploadData", UploadDataTab()),
        shinydashboard::tabItem("LoadData", LoadDataTab()),
        shinydashboard::tabItem("contact", contactTab())
      )

      # print(process_data()$complete)
      all_tabs <-createDynamicTabs(process_data()$complete, process_data()$incomplete, plot_config, session)


      # Combine static and dynamic tabs
      do.call(shinydashboard::tabItems, c(static_tabs, all_tabs))
    })




  })
}


