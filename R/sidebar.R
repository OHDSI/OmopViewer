
createSidebar <- function(tab, choic) {
  content <- c(
    getSidebarInformation(tab),
    getSidebarChoices(choic$settings, "Settings", paste0(tab, "_settings")),
    getSidebarChoices(choic$grouping, "grouping", paste0(tab, "_grouping")),
    getSidebarChoices(choic["variable_name"], "Variables", tab),
    getSidebarChoices(choic["estimate_name"], "Estimates", tab)
  ) |>
    paste0(collapse = ",\n")
  'sidebar = bslib::sidebar(
    bslib::accordion(
      {content}
    )
  ),' |>
    glue::glue() |>
    as.character()
}
getSidebarInformation <- function(tab) {
  info <- getInfo(tab, "information", "")
  'bslib::accordion_panel(
    title = "Information",
    icon = shiny::icon("info"),
    shiny::p("{info}")
  )' |>
    glue::glue() |>
    as.character()
}
getSidebarChoices <- function(choi, tit, prefix) {
  if (length(choi) == 0) return(NULL)
  selectors <- purrr::map_chr(names(choi), \(x) selector(
    paste0(prefix, "_", x), formatTit(x), 'NULL', 'NULL', TRUE
  )) |>
    paste0(collapse = ",\n")
  'bslib::accordion_panel(
    title = "{tit}",
    {selectors}
  )' |>
    glue::glue() |>
    as.character()
}
