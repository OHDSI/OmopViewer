
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
  writeLines(text = text, con = file.path(directory, "README.md"))
}
