
# ui ----
tidyUi <- function(tab) {
  id <- paste0(tab, "_tidy_download")
  'bslib::nav_panel(
    title = "Tidy",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download csv")},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::checkboxInput(
            inputId = "{tab}_tidy_settings",
            label = "Show settings",
            value = FALSE
          ),
          shiny::checkboxInput(
            inputId = "{tab}_tidy_grouping",
            label = "Show grouping",
            value = TRUE
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
tidyServer <- function(rt, data) {
  c('getTidyData[formatCamel(rt)] <- shiny::reactive({
      [data] |>
        filterData("[rt]", input) |>
        tidyData(
          prefixSet = "set:",
          prefixGroup = "group: ",
          showSettings = input$[rt]_tidy_settings,
          showgrouping = input$[rt]_tidy_grouping,
          pivot = input$[rt]_tidy_pivot
        )
    })',
    'output$[rt]_tidy <- DT::renderDT({
      DT::datatable(getTidyData[formatCamel(rt)](), options = list(scrollX = TRUE))
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
