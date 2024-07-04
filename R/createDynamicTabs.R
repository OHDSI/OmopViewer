#' Create Dynamic Tabs for a Shiny Dashboard
#'
#' This function dynamically generates Shiny tabItems for a dashboard based on the supplied
#' data about complete and incomplete datasets. It configures tabs differently depending
#' on the completeness of the data and available plot configurations.
#'
#' @param complete_data A dataframe containing complete data with necessary columns.
#' @param incomplete_data A list of data frames, each representing incomplete data.
#' @param plot_config A dataframe mapping plot types to module UIs for rendering specific visualizations.
#' @param session The current Shiny session object to maintain session-specific reactive values.
#'
#' @return Returns a list of `shiny::tabItem` objects that can be added to a Shiny dashboard.
#'
createDynamicTabs <- function(complete_data, incomplete_data, plot_config, session) {
  # Combine complete and incomplete data for unified handling
  dynamic_tabs <- list()

  # Handle complete data with specific plot types
  if (!is.null(complete_data)) {
    complete_data <- complete_data |> dplyr::filter(
      .data$estimate_name != "result_type.x" &
        .data$estimate_name != "result_type.y"
    )
    result_tables <- complete_data[complete_data$variable_name == "settings" &
      complete_data$estimate_name == "result_type", ]
    complete_tables_name <- unique(result_tables$estimate_value)

    dynamic_tabs_complete <- lapply(complete_tables_name, function(table_name) {
      local_store <- shiny::reactiveVal(list())

      tabName <- paste0("tab_", table_name)

      result_ids <- unique(result_tables$result_id[result_tables$estimate_value == table_name])
      table_data <- complete_data |> dplyr::filter(.data$result_id %in% result_ids)


      sr <- table_data |>
        omopgenerics::newSummarisedResult()


      filter_setting_init_server(
        paste0(tabName, "filter_setting_id"), shiny::reactive({
          omopgenerics::settings(sr)
        }),
        global_store = local_store
      )


      table_data <- shiny::reactive({
        selected_result_id <- local_store() |> unique()
        complete_data |>
          dplyr::filter(.data$result_id %in% selected_result_id)
      })

      table_data_withsetting <- shiny::reactive({
        table_data() |>
          omopgenerics::newSummarisedResult() |>
          visOmopResults::addSettings()
      })


      # Initialize the server for filters and tables
      session$userData[[paste0(tabName, "filter_input")]] <-
        filter_module_server(
          paste0(tabName, "filter_id"), table_data
        )
      table_init_server(
        paste0(tabName, "table_id"),
        dataset = table_data,
        filter_input = session$userData[[paste0(tabName, "filter_input")]]
      )

      # Plotting logic
      extra_ui <- if (any(plot_config$plot_type == table_name)) {
        plot_module_ui_name <- plot_config$module_ui[plot_config$plot_type == table_name]
        plot_module_server_name <- plot_config$module_server[plot_config$plot_type == table_name]

        plot_ui_module <- get(plot_module_ui_name)
        plot_server_module <- get(plot_module_server_name)

        plot_server_module(
          paste0(tabName, "_plot"), table_data,
          session$userData[[paste0(tabName, "filter_input")]]
        )
        plot_ui_module(paste0(tabName, "_plot"))
      } else {
        NULL
      }

      shinydashboard::tabItem(
        tabName = tabName,
        shiny::fluidPage(
          addSharedResources(),
          shiny::div(
            class = "container-fluid",
            shiny::div(
              class = "row",
              shiny::div(
                class = "col-md-12",
                createCard(
                  id = paste0("summarisedResult_", tabName),
                  title = table_name,
                  setting_filter_ui = filter_setting_ui(paste0(tabName, "filter_setting_id")),
                  filter_ui = filter_module_ui(paste0(tabName, "filter_id")),
                  table_ui = table_ui(paste0(tabName, "table_id")),
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
  # if (!is.null(incomplete_data)) {
  #   incomplete_table_names <- names(incomplete_data)
  #
  #   dynamic_tabs_incomplete <- lapply(incomplete_table_names, function(table_name) {
  #     tabName <- paste0("tab_", table_name)
  #     table_incomplete <- incomplete_data[[table_name]]
  #     # Initialize the server for filters and tables similarly to the complete data
  #     session$userData[[paste0(tabName, "filter_input")]] <-
  #       filter_module_server(
  #       paste0(tabName, "filter_id"), shiny::reactive({ table_incomplete })
  #     )
  #     table_init_server(
  #       paste0(tabName, "table_id"), dataset = shiny::reactive({ table_incomplete }),
  #       filter_input = session$userData[[paste0(tabName, "filter_input")]]
  #     )
  #
  #     shinydashboard::tabItem(
  #       tabName = tabName,
  #       fluidPage(
  #         addSharedResources(),
  #         div(class = "container-fluid",
  #             div(class = "row",
  #                 div(class = "col-md-12",
  #                     createCard(
  #                       id = paste0("summarisedResult_", tabName),
  #                       title = table_name,
  #                       setting_filter_ui = NULL,
  #                       filter_ui = filter_module_ui(paste0(tabName, "filter_id")),
  #                       table_ui = table_ui(paste0(tabName, "table_id"))
  #                     )
  #                 )
  #             )
  #         )
  #       )
  #     )
  #   })
  #
  #   dynamic_tabs <- c(dynamic_tabs, dynamic_tabs_incomplete)
  # }

  return(dynamic_tabs)
}
