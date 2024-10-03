
# ui ----
downloadRawDataUi <- function() {
  'bslib::nav_item(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(
        outputId = "download_raw",
        label = "Download raw data",
        icon = shiny::icon("download")
      )
    )
  )'
}
rawUi <- function(rt) {
  id <- paste0(rt, "_raw_download")
  'bslib::nav_panel(
    title = "Raw",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download summarised_result")},
      DT::dataTableOutput("{rt}_raw")
    )
  )' |>
    glue::glue() |>
    as.character()
}

# server ----
downloadRawDataServer <- function(data) {
  '# download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      omopViewer::exportSummarisedResult([data], fileName = file)
    }
  )' |>
    glue::glue(.open = "[", .close = "]") |>
    as.character()
}
rawServer <- function(rt, data) {
  c('getRawData[formatCamel(rt)] <- shiny::reactive({
      filterData([data], "[rt]", input)
    })',
    'output$[rt]_raw <- DT::renderDT({
      DT::datatable(getRawData[formatCamel(rt)](), options = list(scrollX = TRUE))
    })',
    'output$[rt]_raw_download <- shiny::downloadHandler(
      filename = "raw_[rt].csv",
      content = function(file) {
        getRawData[formatCamel(rt)]() |>
          readr::write_csv(file = file)
          # TBR by exportSummarisedResult
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
