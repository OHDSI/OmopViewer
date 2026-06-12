
copyReadme <- function(shiny, report, title, directory) {
  if (shiny) {
    ifShiny<- ""
    if (report) {
      ifReport <- ""
      content <- "Shiny App and Report"
    } else {
      ifReport <- NULL
      content <- "Shiny App"
    }
  } else {
    ifShiny <- NULL
    ifReport <- ""
    content <- "Report"
  }
  text <- purrr::map(omopViewerReadme, \(x) glue::glue(x, title = title)) |>
    purrr::compact() |>
    as.character()
  if (shiny && report) {
    text <- c(
      text,
      "- The `renderReport.R` file regenerates the `www/reports/report.html` and `www/reports/report.docx` files after editing `report.qmd`."
    )
  }
  writeLines(text = text, con = file.path(directory, "README.md"))
}
