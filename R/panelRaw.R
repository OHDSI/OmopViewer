
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

# server ----
downloadRawDataServer <- function(data) {
  '# download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      omopgenerics::exportSummarisedResult([data], fileName = file)
    }
  )' |>
    glue::glue(.open = "[", .close = "]") |>
    as.character()
}
