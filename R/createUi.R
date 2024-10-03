
createUi <- function(resultTypes, choices = list()) {
  purrr::map_chr(resultTypes, \(x) {
    c(
      'bslib::nav_panel(',
      c(panelTitle(x), panelIcon(x), panelSidebar(x, choices[[x]])) |>
        paste0(collapse = ",\n"),
      ')'
    ) |>
      paste0(collapse = "\n")
  })
}
getInfo <- function(rt, info, def) {
  x <- omopViewerTabs[[info]][omopViewerTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
}
panelTitle <- function(tab) {
  paste0('title = "', getInfo(tab, "title", formatTit(tab)), '"')
}
panelIcon <- function(tab) {
  icon <- getInfo(tab, "icon", NULL)
  if (is.null(icon)) return(NULL)
  paste0('icon = shiny::icon("', icon, '")')
}
panelSidebar <- function(tab, choic) {
  sidebar <- createSidebar(tab, choic)
  panels <- c(
    tidyUi(tab),
    formattedUi(tab, choic),
    plotsUi(tab, choic)
  ) |>
    paste0(collapse = ",\n")
  "bslib::layout_sidebar(
    {sidebar}
    bslib::navset_card_tab(
      {panels}
    )
  )" |>
    glue::glue() |>
    as.character()
}
