import("shiny")
import("plotly")
import("modules")
import("shinydashboard")
import("ggplot2")
import("DT")
import("utils")
import("dplyr")
import("shinyWidgets")
import("shinycssloaders")
import("IncidencePrevalence")
export("ui")
export("init_server")
CONSTS <- use("constants/constants.R")


# Module UI Function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      pickerInput(
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
    div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      pickerInput(
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
    downloadButton(ns("prevalence_plot_download_png"), "Download PNG"),
    plotlyOutput(ns("prevalence_plot")) %>% withSpinner()
  )
}

# Module Server Function
init_server <- function(id, dataset, filter_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # filtered_data <- reactive({dataset()})  # Use the reactive dataset
    
    filtered_data <- reactive({
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
    
    output$prevalence_plot <- renderPlotly({
      inc <- filtered_data()
      class(inc) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(inc))
      p <- plotPrevalence(result = inc,
                         facet = input$prv_estimates_plot_facet,
                         colour = input$prv_estimates_plot_colour)
      p
    })
    output$prevalence_plot_download_png <- downloadHandler(
      filename = "prevalence.png",
      content = function(file) {
        inc <- filtered_data()
        class(inc) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(inc))
        p <- plotprevalence(result = inc, colour = c("denominator_age_group", "denominator_sex", "outcome_cohort_name"))
        ggsave(filename = file, plot = p)
      }
    )
  })
}
