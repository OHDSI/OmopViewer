
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
  c('getTidyData[formatCamel(rt)] <- shiny::reactive({
      [data] |>
        filterData("[rt]", input) |>
        tidyData(
          cols = input$[rt]_tidy_columns,
          pivot = input$[rt]_tidy_pivot
        )
    })',
    'output$[rt]_tidy <- DT::renderDT({
      DT::datatable(
        getTidyData[formatCamel(rt)](),
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    })',
    'output$[rt]_tidy_download <- shiny::downloadHandler(
      filename = "tidy_[rt].csv",
      content = function(file) {
        getTidyData[formatCamel(rt)]() |>
          readr::write_csv(file = file)
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
