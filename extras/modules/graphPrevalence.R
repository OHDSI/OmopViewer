#' UI function for the Prevalence Graph Module
#'
#' This function creates the UI components for the Prevalence graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A UI definition for Prevalence plot
#' @export
graphPrevalence_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("prv_estimates_plot_facet"),
        label = "Facet by",
        choices = c("group","cdm_name", "outcome_cohort_name",
                    "denominator_target_cohort_name", "denominator_age_group",
                    "denominator_sex", "denominator_days_prior_observation",
                    "denominator_start_date", "denominator_end_date",
                    "analysis_outcome_washout", "analysis_repeated_events",
                    "analysis_complete_database_intervals",
                    "analysis_min_cell_count", "analysis_interval",
                    "prevalence_start_date"),
        selected = c("outcome_cohort_name"),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    shiny::div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("prv_estimates_plot_colour"),
        label = "Colour by",
        choices = c("cdm_name", "outcome_cohort_name",
                    "denominator_target_cohort_name",
                    "denominator_age_group", "denominator_sex",
                    "denominator_days_prior_observation",
                    "denominator_start_date", "denominator_end_date",
                    "analysis_outcome_washout", "analysis_repeated_events",
                    "analysis_complete_database_intervals",
                    "analysis_min_cell_count", "analysis_interval",
                    "prevalence_start_date"),
        selected = "cdm_name",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    shiny::downloadButton(ns("prevalence_plot_download_png"), "Download PNG"),
    plotly::plotlyOutput(ns("prevalence_plot")) |>
      shinycssloaders::withSpinner()
  )
}
#' Server function for the Prevalence Graph Module
#'
#' This function initializes the server-side logic for the Prevalence graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @param dataset A reactive expression that returns the dataset.
#' @param filter_input A reactive expression that returns the filter input.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A module server function.
#' @export

graphPrevalence_init_server <- function(id, dataset, filter_input) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # filtered_data <- reactive({dataset()})  # Use the reactive dataset

    filtered_data <- shiny::reactive({
      df <- dataset()  # Use the reactive dataset
      flt <- filter_input()
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }

      df
    })

    output$prevalence_plot <- plotly::renderPlotly({
      inc <- filtered_data()
      class(inc) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(inc))
      p <- IncidencePrevalence::plotPrevalence(result = inc,
                         facet = input$prv_estimates_plot_facet,
                         colour = input$prv_estimates_plot_colour)
      p
    })
    output$prevalence_plot_download_png <- shiny::downloadHandler(
      filename = "prevalence.png",
      content = function(file) {
        inc <- filtered_data()
        class(inc) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(inc))
        p <- IncidencePrevalence::plotPrevalence(result = inc, colour = c("denominator_age_group", "denominator_sex", "outcome_cohort_name"))
        ggplot2::ggsave(filename = file, plot = p)
      }
    )
  })
}
