#' UI function for the Large Scale Characteristics Graph Module
#'
#' This function creates the UI components for the large scale characteristics graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A UI definition for large scale characteristics plot
#' @export
graphLargeScaleCharacteristics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      shinyWidgets::pickerInput(
        ns("lsc_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("lsc_plot_color"),
        label = "Color by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("lsc_plot_style"),
        label = "Position",
        choices = c("horizontal", "vertical"),
        selected = "horizontal",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      shinyWidgets::pickerInput(
        ns("lsc_plot_strata"),
        label = "Split strata",
        choices = c(TRUE, FALSE),
        selected = TRUE,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      shinyWidgets::pickerInput(
        ns("lsc_plot_var_level"),
        label = "Variable level",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      shinyWidgets::pickerInput(
        ns("lsc_plot_variable"),
        label = "variable_name",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
      # pickerInput(
      #   ns("lsc_plot_add_level"),
      #   label = "Additional level",
      #   choices = NULL,
      #   selected = NULL,
      #   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #   multiple = TRUE
      # ),
      # pickerInput(
      #   ns("lsc_plot_type"),
      #   label = "Type",
      #   choices = NULL,
      #   selected = NULL,
      #   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #   multiple = TRUE
      # )
    ),
    shiny::downloadButton(ns("lsc_plot_download_png"), "Download PNG"),
    plotly::plotlyOutput(ns("lsc_plot"), width = "100%", height = "800px") |>
      shinycssloaders::withSpinner()
  )
}

#' Server function for the Large Scale Characteristics Graph Module
#'
#' This function initializes the server-side logic for the large scale characteristics graph module.
#'
#' @param id A string. The namespace identifier for the module.
#' @param dataset A reactive expression that returns the dataset.
#' @param filter_input A reactive expression that returns the filter input.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @return A module server function.
#' @export
graphLargeScaleCharacteristics_init_server <- function(id, dataset, filter_input) {
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


    shiny::observe({
      shiny::req(filtered_data())
      # Calculate columns with more than one unique value
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "estimate_name",
                                          "variable_level", "result_id",
                                          "variable_name", "group_name",
                                          "estimate_type", "additional_level"))]
      #### to be checked, shd not allow by these???
      shinyWidgets::updatePickerInput(session, inputId = "lsc_plot_facet", choices = choices,
                        selected = choices[2])

      shinyWidgets::updatePickerInput(session, inputId = "lsc_plot_color", choices = choices,
                        selected = choices[1])


      df_1 <- filtered_data() |>
        dplyr::filter(!is.na(.data$estimate_value))  # Use the reactive dataset


      filtered_values_1 <- df_1 |>
        dplyr::pull(
        "variable_level"
      ) |> unique()
      shinyWidgets::updatePickerInput(session, inputId = "lsc_plot_var_level", choices = filtered_values_1,
                        selected = filtered_values_1[1])



      filtered_values_4 <- unique(df_1$variable_name)

      shinyWidgets::updatePickerInput(session, inputId = "lsc_plot_variable", choices = filtered_values_4,
                        selected = filtered_values_4[1])

    })


    prepared_plot_data <- shiny::reactive({

      # Find relevant result IDs based on current table input
      result_ids <- filtered_data() |>
        dplyr::filter(!is.na(.data$estimate_value)) |>
        dplyr::filter(.data$variable_name %in% input$lsc_plot_variable) |>
        # filter(table_name %in% input$lsc_plot_table_filter) |>
        dplyr::filter(.data$variable_level %in% input$lsc_plot_var_level
               # additional_level %in% input$lsc_plot_add_level,type %in% input$lsc_plot_type
        ) |>
        dplyr::pull("result_id")


      # Combine and re-summarize the data only once

      return(
        dataset() |>
          dplyr::filter(!is.na(.data$estimate_value)) |>
          dplyr::filter(.data$variable_name %in% input$lsc_plot_variable) |>
          dplyr::filter(.data$variable_level %in% input$lsc_plot_var_level) |>
          omopgenerics::newSummarisedResult() |>
          visOmopResults::filterSettings(.data$result_id %in% result_ids)
        )



    })




    output$lsc_plot <- plotly::renderPlotly({

      p <- CohortCharacteristics::plotLargeScaleCharacteristics(
        data = prepared_plot_data(),
        facet = input$lsc_plot_facet,
        colorVars = input$lsc_plot_color,
        position = input$lsc_plot_style,
        splitStrata = as.logical(input$lsc_plot_strata)
      )
      p <- p +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.x = ggplot2::element_text(size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          legend.text = ggplot2::element_text(size = 8),
          strip.text = ggplot2::element_text(size = 5)  # Adjusting facet title font size

        )

      plotly::ggplotly(p)
    })

    output$lsc_plot_download_png <- shiny::downloadHandler(
      filename = function() {
        paste0("summarised_large_scale_characteristics_", Sys.Date(), ".png")
      },
      content = function(file) {
        inc <- filtered_data() |>
          omopgenerics::newSummarisedResult()
        p <- CohortCharacteristics::plotLargeScaleCharacteristics(
          data = inc,
          facet = input$lsc_plot_facet,
          colorVars = input$lsc_plot_color,
          position = input$lsc_plot_style,
          splitStrata = as.logical(input$lsc_plot_strata)
        )
        ggplot2::ggsave(filename = file, plot = p)
      }
    )
  })
}
