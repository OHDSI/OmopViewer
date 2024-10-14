
# ui ----
tidyUi <- function(tab) {
  'bslib::nav_panel(
    title = "Tidy",
    bslib::card(
      full_screen = TRUE,
      {downloadTable("{tab}_tidy_download", "Download csv")},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          {selector("{tab}_tidy_columns", "Columns", "NULL", "NULL", TRUE)},
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
    glue::glue() |>
    as.character()
}

# server ----
tidyServer <- function(rt, data) {
  c(paste0('getTidyData', formatCamel(rt), ' <- shiny::reactive({
      res <- ', data, ' |>
        OmopViewer::filterData("', rt, '", input) |>
        OmopViewer::tidyData()

      # columns to eliminate
      colsEliminate <- colnames(res)
      colsEliminate <- colsEliminate[!colsEliminate %in% c(
        input$', rt, '_tidy_columns, "variable_name", "variable_level",
        "estimate_name", "estimate_type", "estimate_value"
      )]

      # pivot
      pivot <- input$', rt, '_tidy_pivot
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
    'output$[rt]_tidy <- DT::renderDT({
      DT::datatable(
        getTidyData[formatCamel(rt)](),
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    })' |>
      glue::glue(.open = "[", .close = "]"),
    'output$[rt]_tidy_download <- shiny::downloadHandler(
      filename = "tidy_[rt].csv",
      content = function(file) {
        getTidyData[formatCamel(rt)]() |>
          readr::write_csv(file = file)
      }
    )' |>
      glue::glue(.open = "[", .close = "]")
  )
}
