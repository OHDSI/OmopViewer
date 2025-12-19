
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
  text <- as.character(purrr::map(omopViewerReadme, \(x) glue::glue(x, title = title)))
  writeLines(text = text, con = file.path(directory, "README.md"))
}
