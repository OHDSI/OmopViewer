#' UI function for the Cohort Overlap Graph Module
#'
#' This function creates the UI components for the cohort overlap graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A UI definition for Cohort Overlap plot
#' @export
graphCohortOverlap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("co_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("co_unique_comb"),
        label = "Unique Combination",
        choices = c(TRUE, FALSE),
        selected = TRUE,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
    ),
    shiny::downloadButton(ns("co_plot_download_png"), "Download PNG"),
    plotly::plotlyOutput(ns("co_plot")) |>
      shinycssloaders::withSpinner()
  )
}

#' Server function for the Cohort Overlap Graph Module
#'
#' This function initializes the server-side logic for the cohort overlap graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @param dataset A reactive expression that returns the dataset.
#' @param filter_input A reactive expression that returns the filter input.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A module server function.
#' @export
graphCohortOverlap_init_server <- function(id, dataset, filter_input) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    transformed_data <- shiny::reactive({
      df <- dataset() # Use the reactive dataset
      df |>
        omopgenerics::newSummarisedResult() |>
        visOmopResults::addSettings()
    })

    # Reactive for handling filtered data
    filtered_data <- shiny::reactive({
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


    shiny::observe({
      shiny::req(filtered_data())
      # Calculate columns with more than one unique value
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "estimate_name",
                                          "variable_level", "result_id"))]
      #### to be checked, shd not allow by these???
      shinyWidgets::updatePickerInput(session, inputId = "co_plot_facet", choices = choices,
                        selected = choices[1])
    })

    prepared_plot_data <- shiny::reactive({
      # Get result IDs from filtered data to include only relevant settings
      result_ids <- filtered_data() |>
        dplyr::pull("result_id") |> unique()

      # Combine settings with filtered data and reapply summarization
      return(dataset() |>
      visOmopResults::filterSettings(.data$result_id %in% result_ids) |>
      dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_type == "integer", "numeric",
                                                   .data$estimate_type)))

    })


    output$co_plot <- plotly::renderPlotly({
      saveRDS(prepared_plot_data() , "test.rds")
      p <- CohortCharacteristics::plotCohortOverlap(
        result = prepared_plot_data() %>% omopgenerics::newSummarisedResult(),
        facet = input$co_plot_facet,
        uniqueCombinations = as.logical(input$co_unique_comb)
      )

      # Check if the input style is 'boxplot' and handle accordingly
      p
    })

    output$co_plot_download_png <- shiny::downloadHandler(
      filename = function() {
        paste0("cohort_overlap_", Sys.Date(), ".png")
      },
      content = function(file) {
        inc <- filtered_data() |>
          dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_type == "integer", "numeric",
                                                       .data$estimate_type)) |>
          omopgenerics::newSummarisedResult()
        p <- CohortCharacteristics::plotCohortTiming(
          data = prepared_plot_data(),
          facet = input$co_plot_facet,
          uniqueCombinations = as.logical(input$co_unique_comb)
        )
        ggplot2::ggsave(filename = file, plot = p)
      }
    )
  })
}
