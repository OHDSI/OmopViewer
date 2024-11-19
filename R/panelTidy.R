
# ui ----
tidyUi <- function(tab) {
  'bslib::nav_panel(
    title = "Tidy",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        bslib::popover(
          shiny::icon("download"),
          shiny::downloadButton(outputId = "{tab}_tidy_download", label = "Download csv")
        ),
        class = "text-end"
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shinyWidgets::pickerInput(
            inputId = "{tab}_tidy_columns",
            label = "Columns",
            choices = filterValues${tab}_tidy_columns,
            selected = filterValues${tab}_tidy_columns,
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          shiny::radioButtons(
            inputId = "{tab}_tidy_pivot",
            label = "Pivot estimates/variables",
            choices = c("none", "estimates", "estimates and variables"),
            selected = "none"
          ),
          position = "right"
        ),
        DT::dataTableOutput("{tab}_tidy")
      )
    )
  )' |>
    glue::glue() |>
    as.character()
}

# server ----
tidyServer <- function(prefix, data) {
  funPrefix <- formatCamel(prefix)
  c(paste0('getTidyData', funPrefix, ' <- shiny::reactive({
      res <- ', data, ' |>
        filterData("', prefix, '", input) |>
        omopgenerics::addSettings() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id")

      # columns to eliminate
      colsEliminate <- colnames(res)
      colsEliminate <- colsEliminate[!colsEliminate %in% c(
        input$', prefix, '_tidy_columns, "variable_name", "variable_level",
        "estimate_name", "estimate_type", "estimate_value"
      )]

      # pivot
      pivot <- input$', prefix, '_tidy_pivot
      if (pivot != "none") {
        vars <- switch(
          pivot,
          "estimates" = "estimate_name",
          "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
        )
        res <- res |>
          visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
      }

      res |>
        dplyr::select(!dplyr::all_of(colsEliminate))
    })'),
    'output$[prefix]_tidy <- DT::renderDT({
      DT::datatable(
        getTidyData[funPrefix](),
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    })' |>
      glue::glue(.open = "[", .close = "]"),
    'output$[prefix]_tidy_download <- shiny::downloadHandler(
      filename = "tidy_[prefix].csv",
      content = function(file) {
        getTidyData[funPrefix]() |>
          readr::write_csv(file = file)
      }
    )' |>
      glue::glue(.open = "[", .close = "]")
  )
}
