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
  plot_config <- list(
    summarised_large_scale_characteristics = list(
      name = "summarised_large_scale_characteristics",
      plotFunc = CohortCharacteristics::plotLargeScaleCharacteristics,
      uiElements = c("facet", "colour", "style", "strata", "table_filter", "var_level", "variable"),
      plotParams = list(
        facet = list(id = "lsc_plot_facet", multiple = TRUE),
        colour = list(id = "lsc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "lsc_plot_style", multiple = FALSE, choices = c("horizontal", "vertical"), selected = "horizontal"),
        splitStrata = list(id = "lsc_plot_strata", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        table_filter = list(id = "lsc_plot_table_filter", multiple = FALSE),
        variable_level = list(id = "lsc_plot_var_level", multiple = TRUE),
        variable_name = list(id = "lsc_plot_variable", multiple = TRUE)
      ),
      dataArg = "data",
      updatePickerInputIDs = list(
        variable_name = "lsc_plot_variable",
        variable_level = "lsc_plot_var_level"
      )
    ),
    cohort_overlap = list(
      name = "cohort_overlap",
      plotFunc = CohortCharacteristics::plotCohortOverlap,
      uiElements = c("facet", "uniqueCombinations"),
      plotParams = list(
        facet = list(id = "co_plot_facet", multiple = TRUE),
        uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),
      plotButtons = list(
        facet = list(id = "co_plot_facet", multiple = TRUE),
        uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),

      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "co_plot_facet"
      )
    ),
    summarised_characteristics = list(
      name = "summarised_characteristics",
      plotFunc = CohortCharacteristics::plotCharacteristics,
      uiElements = c("facet", "colour", "style", "x", "variable_name"),
      plotButtons= list(
        facet = list(id = "sc_plot_facet", multiple = TRUE),
        colour = list(id = "sc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                         choices = c("boxplot", "barplot"), selected = "barplot"),
        x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                             "variable_level", "estimate_type", "group_name",
                                                                             "group_level"),
                 selected = "variable_name"),
        variable_name = list(id = "sc_plot_variable", multiple = FALSE)
      ),
      plotParams= list(
        facet = list(id = "sc_plot_facet", multiple = TRUE),
        colour = list(id = "sc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                         choices = c("boxplot", "barplot"), selected = "barplot"),
        x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                             "variable_level", "estimate_type", "group_name",
                                                                             "group_level"),
                 selected = "variable_name")
      ),
      dataArg = "data",
      updatePickerInputIDs = list(
        variable_name = "sc_plot_variable",
        facet = "sc_plot_facet",
        colour = "sc_plot_colour"
      )
    ),
    cohort_timing = list(
      name = "cohort_timing",
      plotFunc = CohortCharacteristics::plotCohortTiming,
      uiElements = c("facet", "colour", "uniqueCombinations", "plotType"),
      plotParams = list(
        facet = list(id = "ct_plot_facet", multiple = TRUE),
        colour = list(id = "ct_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ct_plot_style", multiple = FALSE,
                        choices = c("boxplot"), selected = "boxplot")
      ),
      plotButtons = list(
        facet = list(id = "ct_plot_facet", multiple = TRUE),
        colour = list(id = "ct_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),

      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "ct_plot_facet",
        colour = "ct_plot_colour"
      )
    ),
    cohort_timing_density = list(
      name = "cohort_timing_density",
      plotFunc = CohortCharacteristics::plotCohortTiming,
      uiElements = c("facet", "colour", "uniqueCombinations", "plotType"),
      plotParams = list(
        facet = list(id = "ctd_plot_facet", multiple = TRUE),
        colour = list(id = "ctd_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ctd_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ctd_plot_style", multiple = FALSE,
                        choices = c("density"), selected = "density")
      ),
      plotButtons = list(
        facet = list(id = "ctd_plot_facet", multiple = TRUE),
        colour = list(id = "ctd_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ctd_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ctd_plot_style", multiple = FALSE,
                        choices = c("density"), selected = "density")
      ),
      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "ctd_plot_facet",
        colour = "ctd_plot_colour"
      )
    )
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

    # Check if datasets are defined and not empty
    if (!is.null(datasets) && length(datasets) > 0) {

      # Initialize lists for storing datasets
      missing_required_columns <- list()
      complete_datasets <- list()
      unique_result_ids <- numeric()

      # Iterate over each dataset to classify
      names <- names(datasets)

      for (i in seq_along(names)) {
        data <- datasets[[i]]
        if (all(summarisedResultColumns %in% colnames(data))) {
          data <- omopgenerics::newSummarisedResult(data)
          complete_datasets[[length(complete_datasets) + 1]] <- data
        } else {
          missing_required_columns[[names[i]]] <- data  # Store with dataset name as key
        }
      }

      # Combine datasets with all required columns into one dataframe
      combined_data <- do.call(omopgenerics::bind, complete_datasets) #why is this changin the result id????
      # Return both results as a list
      return(list(complete = combined_data, incomplete = missing_required_columns))

    } else {
      # Return NULL or an empty list if datasets are not defined or empty
      return(NULL)
    }
  })


  shiny::observe({
    # # Dynamic sidebar
    output$dynamic_sidebar <- shinydashboard::renderMenu({
      sidebarMenuDynamic(process_data()$complete, process_data()$incomplete)
    })

    # Dynamic tabs
    output$dynamic_tabs_output <- shiny::renderUI({
      static_tabs <- list(
        shinydashboard::tabItem("about", aboutTab()),
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


