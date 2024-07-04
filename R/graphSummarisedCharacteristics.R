#' UI function for the Summarised Characteristics Graph Module
#'
#' This function creates the UI components for the summarised characteristics graph module.
#'
#' @param id A string. The namespace identifier for the module.
#'
#' @return A UI definition for summarised characteristics plot
#' @export
graphSummarisedCharacteristics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("sc_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("sc_plot_color"),
        label = "Color by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("sc_plot_style"),
        label = "Plot Style",
        choices = c("boxplot", "barplot"),
        selected = "barplot",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      shinyWidgets::pickerInput(
        ns("sc_plot_xaxis"),
        label = "X Axis",
        choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                    "variable_level", "estimate_type", "group_name",
                    "group_level"),
        selected = "variable_name",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      shinyWidgets::pickerInput(
        ns("sc_plot_variable"),
        label = "variable_name",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      )
    ),
    shiny::downloadButton(ns("sc_plot_download_png"), "Download PNG"),
    plotly::plotlyOutput(ns("sc_plot")) |>
      shinycssloaders::withSpinner()
  )
}

#' Server function for the Summarised Characteristics Graph Module
#'
#' This function initializes the server-side logic for the summarised characteristics graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @param dataset A reactive expression that returns the dataset.
#' @param filter_input A reactive expression that returns the filter input.
#'
#' @return A module server function.
#' @export
graphSummarisedCharacteristics_init_server <- function(id, dataset, filter_input) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    transformed_data <- shiny::reactive({
      df <- dataset()  # Use the reactive dataset
      df |>
        omopgenerics::newSummarisedResult() |>
        visOmopResults::addSettings()
    })

    # Reactive for handling filtered data
    filtered_data <- shiny::reactive({
      df <- transformed_data()  # Start with the transformed dataset
      flt <- filter_input()  # Get the current filters
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }
      df
    })

    shiny::observeEvent(filtered_data(), {
      shiny::req(filtered_data())
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "result_id",
                                          "variable_level", "additional_level"))]
      #### to be checked, shd not allow by these???
      shinyWidgets::updatePickerInput(session, inputId = "sc_plot_facet", choices = choices,
                        selected = choices[1])
      shinyWidgets::updatePickerInput(session, inputId = "sc_plot_color", choices = choices,
                        selected = choices[2])
      # Exclude specific unwanted values
      excluded_values <- c("Age group", "Cohort start date", "Cohort end date") #
      filtered_values <- unique(filtered_data()$variable_name)[!unique(filtered_data()$variable_name) %in% excluded_values]

      shinyWidgets::updatePickerInput(session, inputId = "sc_plot_variable", choices = filtered_values)
    })

    prepared_plot_data <- shiny::reactive({
      # Get result IDs from filtered data to include only relevant settings
      result_ids <- filtered_data() |>
        dplyr::filter(.data$variable_name == input$sc_plot_variable) |>
        dplyr::pull("result_id") |> unique()

      # Get settings for included result_ids
      inc_setting <- dataset() |>
        dplyr::filter(.data$result_id %in% result_ids,
                      .data$variable_name == "settings")

      # Combine settings with filtered data and reapply summarization
      rbind(inc_setting, filtered_data() |>
              dplyr::filter(.data$variable_name == input$sc_plot_variable) |>
              omopgenerics::newSummarisedResult()) |>
        omopgenerics::newSummarisedResult()
    })

    output$sc_plot <- plotly::renderPlotly({
      # Check if the input style is 'barplot' and handle accordingly
      # inc <- filtered_data() |>
      #   filter(variable_name == input$sc_plot_variable)
      #
      # result_ids <- filtered_data() |> pull("result_id") |> unique()
      #
      # inc_setting <- dataset() |> filter(result_id %in% result_ids) |> filter(variable_name == "settings")
      # inc_tbl <- rbind(inc_setting, filtered_data()|> omopgenerics::newSummarisedResult())
      p <- CohortCharacteristics::plotCharacteristics(data = prepared_plot_data(),
                               facet = input$sc_plot_facet,
                               colour = input$sc_plot_color,
                               plotStyle = input$sc_plot_style,
                               x = input$sc_plot_xaxis)

      # Check if the input style is 'boxplot' and handle accordingly
      p
    })

    output$sc_plot_download_png <- shiny::downloadHandler(
      filename = function() { paste0("summarised_characteristics_", Sys.Date(), ".png") },
      content = function(file) {
        inc <- filtered_data() |>
          dplyr::filter(.data$variable_name == input$sc_plot_variable) |>
          dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_type == "integer", "numeric",
                                                .data$estimate_type)) |>
          omopgenerics::newSummarisedResult()
        p <- CohortCharacteristics::plotCharacteristics(data = inc,
                                 facet = input$sc_plot_facet,
                                 colour = input$sc_plot_color,
                                 plotStyle = input$sc_plot_style,
                                 x = input$sc_plot_xaxis)
        ggplot2::ggsave(filename = file, plot = p)
      }
    )

  })
}
