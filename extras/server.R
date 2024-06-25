function(input, output, session) {
  # Configuration for plot types and their corresponding modules
  plot_config <- data.frame(
    plot_type = c("cohort_overlap", "summarised_characteristics",
                  "summarised_large_scale_characteristics", "cohort_timing",
                  "cohort_timing_density"),
    module_ui = c("graphCohortOverlap", "graphSummarisedCharacteristics", 
                  "graphLargeScaleCharacteristics", "graphCohortTiming",
                  "graphCohortTimingDensity"),
    stringsAsFactors = FALSE
  )
  
  
  options(shiny.maxRequestSize=30*1024^3)
  dataFolder <- "data"
  summarisedResultColumns <- c(
    "result_id", "cdm_name", "group_name",
    "group_level", "strata_name", "strata_level",
    "variable_name", "variable_level", "estimate_name",
    "estimate_type", "estimate_value", "additional_name",
    "additional_level"
  )

  # Initialize the uploadData module
  uploadData$init_server("uploadData", dataFolder)

  # Monitor changes in the studies.rds file
  studies_path <- file.path(here(dataFolder), "studies.rds")
  studies <- reactiveFileReader(1000, session, studies_path, readRDS)

  # Function to create dynamic sidebar menu based on cohort_data


  # Refresh modules when studies.rds changes
  observe({
    # print("Studies updated")  # Debug print
    studies()
    session$userData$studySelect_upload <- studySelect$init_server("available_studies_upload")
    session$userData$studySelect <- studySelect$init_server("available_studies")
    session$userData$app_data <- dataLoad$myModuleServer("dataLoad", dataFolder)
  })

  # Template modules - Simple dashboard modules
  cohort_data <- reactive({
    session$userData$app_data$loaded_data_value()
  })
  
  process_data <- reactive({
    datasets <- cohort_data()  # Assuming cohort_data() returns a named list of data frames
    
    # List for datasets missing required columns
    missing_required_columns <- list()
    # List for datasets with all required columns
    complete_datasets <- list()
    
    # Iterate over each dataset to classify
    names <- names(datasets)
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
  
  
  observe({
    # # Dynamic sidebar
    output$dynamic_sidebar <- renderMenu({
      sidebarMenuDynamic(process_data()$complete, process_data()$incomplete)
    })

    # Dynamic tabs
    output$dynamic_tabs_output <- renderUI({
      static_tabs <- list(
        tabItem("About", aboutTab),
        tabItem("UploadData", UploadDataTab),
        tabItem("LoadData", LoadDataTab),
        tabItem("contact", contactTab)
      )
      
      # print(process_data()$complete)
      all_tabs <-createDynamicTabs(process_data()$complete, process_data()$incomplete, plot_config, session)

      
      # Combine static and dynamic tabs
      do.call(tabItems, c(static_tabs, all_tabs))
    })
    


 
  })
}


