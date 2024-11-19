summaryTab <- function(summary) {
  if (!summary) return(character())
  'bslib::nav_panel(
    title = "Summary",
    icon = shiny::icon("file-alt"),
    summaryCard(data)
  )'
}
