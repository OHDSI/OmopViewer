#' UI function for the Cohort Timing Graph Module
#'
#' This function creates the UI components for the cohort timing graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A UI definition for Cohort Timing plot
#' @export
graphCohortTiming_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("ct_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("ct_plot_color"),
        label = "Color by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("ct_unique_comb"),
        label = "Unique Combination",
        choices = c(TRUE, FALSE),
        selected = TRUE,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
    ),
    downloadButton(ns("ct_plot_download_png"), "Download PNG"),
    plotly::plotlyOutput(ns("ct_plot")) |>
      shinycssloaders::withSpinner()
  )
}

#' Server function for the Cohort Timing Graph Module
#'
#' This function initializes the server-side logic for the cohort timing graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @param dataset A reactive expression that returns the dataset.
#' @param filter_input A reactive expression that returns the filter input.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A module server function.
#' @export
graphCohortTiming_init_server <- function(id, dataset, filter_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    transformed_data <- reactive({
      df <- dataset() # Use the reactive dataset
      df |>
        omopgenerics::newSummarisedResult() |>
        visOmopResults::addSettings()
    })

    # Reactive for handling filtered data
    filtered_data <- reactive({
      df <- transformed_data() # Start with the transformed dataset
      flt <- filter_input() # Get the current filters
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }
      df
    })


    observe({
      req(filtered_data())
      # Calculate columns with more than one unique value
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "estimate_name",
                                          "variable_level", "result_id"))]
      #### to be checked, shd not allow by these???
      shinyWidgets::updatePickerInput(session, inputId = "ct_plot_facet", choices = choices)
      shinyWidgets::updatePickerInput(session, inputId = "ct_plot_color", choices = choices)
    })

    prepared_plot_data <- reactive({
      # Get result IDs from filtered data to include only relevant settings
      result_ids <- filtered_data() |>
        # filter(variable_name == input$sc_plot_variable) |>
        pull("result_id") |> unique()

      return(dataset() |>
               visOmopResults::filterSettings(.data$result_id %in% result_ids))
    })


    output$ct_plot <- plotly::renderPlotly({

      p <- CohortCharacteristics::plotCohortTiming(
        result = prepared_plot_data(),
        facet = input$ct_plot_facet,
        colour = input$ct_plot_color,
        plotType = "boxplot",
        uniqueCombinations = as.logical(input$ct_unique_comb)
      )

      # Check if the input style is 'boxplot' and handle accordingly
      p
    })

    output$ct_plot_download_png <- downloadHandler(
      filename = function() {
        paste0("summarised_characteristics_", Sys.Date(), ".png")
      },
      content = function(file) {
        inc <- filtered_data() |>
          mutate(estimate_type = if_else(.data$estimate_type == "integer", "numeric",
                                         .data$estimate_type)) |>
          omopgenerics::newSummarisedResult()
        p <- CohortCharacteristics::plotCohortTiming(
          result = prepared_plot_data(),
          facet = input$ct_plot_facet,
          colour = input$ct_plot_color,
          plotType = "boxplot",
          uniqueCombinations = as.logical(input$ct_unique_comb)
        )
        ggplot2::ggsave(filename = file, plot = p)
      }
    )
  })
}
