createDynamicTabs <- function(complete_data, incomplete_data, plot_config, session
) {
  # Combine complete and incomplete data for unified handling
  dynamic_tabs <- list()
  
  # Handle complete data with specific plot types
  if (!is.null(complete_data)) {
    complete_data <- complete_data |> filter(estimate_name !="result_type.x" &
                               estimate_name !="result_type.y")
    result_tables <- complete_data[complete_data$variable_name == "settings" & complete_data$estimate_name == "result_type", ]
    complete_tables_name <- unique(result_tables$estimate_value)
    
    dynamic_tabs_complete <- lapply(complete_tables_name, function(table_name) {
      local_store <- reactiveVal(list())
      
      tabName <- paste0("tab_", table_name)

      result_ids <- unique(result_tables$result_id[result_tables$estimate_value == table_name])
      table_data <- complete_data |> dplyr::filter(result_id %in% result_ids)

      
      sr <- table_data |>
        omopgenerics::newSummarisedResult()
      
      
      filterSettingFunction$init_server(
        paste0(tabName, "filter_setting_id"), reactive({settings(sr)}), global_store = local_store
      )
      
    
      table_data <- reactive({
        selected_result_id <- local_store() |> unique()
        complete_data |> 
          filter(result_id %in% selected_result_id)
      })
      
      table_data_withsetting <- reactive({
        table_data() |>
          omopgenerics::newSummarisedResult() |>
          visOmopResults::addSettings()
      })
      
      # if(table_name == "cohort_overlap"){
      #   print(table_data_withsetting())
      # }
      # # Initialize the server for filter settings
      # session$userData[[paste0(tabName, "filter_setting_input")]] <- filterSettingFunction$init_server(
      #   paste0(tabName, "filter_setting_id"), table_data_withsetting, global_store
      # )
      
      # Initialize the server for filters and tables
      session$userData[[paste0(tabName, "filter_input")]] <- filterFunction$filter_module_server(
        paste0(tabName, "filter_id"), table_data
      )
      tableFunction$init_server(
        paste0(tabName, "table_id"), dataset = table_data, 
        filter_input = session$userData[[paste0(tabName, "filter_input")]]
      )
      
      # Plotting logic
      extra_ui <- if (any(plot_config$plot_type == table_name)) {
        plot_ui_module_name <- plot_config$module_ui[plot_config$plot_type == table_name]
        plot_ui_module <- get(plot_ui_module_name)
        plot_ui_module$init_server(paste0(tabName, "_plot"), table_data, 
                                   session$userData[[paste0(tabName, "filter_input")]])
        plot_ui_module$ui(paste0(tabName, "_plot"))
      } else {
        NULL
      }

      tabItem(
        tabName = tabName,
        fluidPage(
          addSharedResources(),
          div(class = "container-fluid",
              div(class = "row",
                  div(class = "col-md-12",
                      createCard(
                        id = paste0("summarisedResult_", tabName),
                        title = table_name,
                        setting_filter_ui = filterSettingFunction$ui(paste0(tabName, "filter_setting_id")),
                        filter_ui = filterFunction$filter_module_ui(paste0(tabName, "filter_id")),
                        table_ui = tableFunction$ui(paste0(tabName, "table_id")),
                        extra_ui = extra_ui
                      )
                  )
              )
          )
        )
      )
    })
    
    dynamic_tabs <- c(dynamic_tabs, dynamic_tabs_complete)
  }
  
  # Handle incomplete data similarly but without specific plot type considerations
  if (!is.null(incomplete_data)) {
    incomplete_table_names <- names(incomplete_data)
    
    dynamic_tabs_incomplete <- lapply(incomplete_table_names, function(table_name) {
      tabName <- paste0("tab_", table_name)
      table_incomplete <- incomplete_data[[table_name]]
      # Initialize the server for filters and tables similarly to the complete data
      session$userData[[paste0(tabName, "filter_input")]] <- filterFunction$filter_module_server(
        paste0(tabName, "filter_id"), reactive({ table_incomplete })
      )
      tableFunction$init_server(
        paste0(tabName, "table_id"), dataset = reactive({ table_incomplete }), 
        filter_input = session$userData[[paste0(tabName, "filter_input")]]
      )
      
      tabItem(
        tabName = tabName,
        fluidPage(
          addSharedResources(),
          div(class = "container-fluid",
              div(class = "row",
                  div(class = "col-md-12",
                      createCard(
                        id = paste0("summarisedResult_", tabName),
                        title = table_name,
                        setting_filter_ui = NULL,
                        filter_ui = filterFunction$filter_module_ui(paste0(tabName, "filter_id")),
                        table_ui = tableFunction$ui(paste0(tabName, "table_id"))
                      )
                  )
              )
          )
        )
      )
    })
    
    dynamic_tabs <- c(dynamic_tabs, dynamic_tabs_incomplete)
  }
  
  return(dynamic_tabs)
}
