
getPanelSidebar <- function(tab, choic) {
  content <- c(
    getSidebarInformation(tab),
    getSidebarChoices(choic$settings, "Settings", paste0(tab, "_settings")),
    getSidebarChoices(choic$groupping, "Groupping", paste0(tab, "_groupping")),
    getSidebarChoices(choic["variable_name"], "Variables", tab),
    getSidebarChoices(choic["estimate_name"], "Estimates", tab)
  ) |>
    paste0(collapse = ",\n")

  panels <- c(
    getRawPanel(tab),
    getTidyPanel(tab),
    getFormattedPanel(tab, choic),
    getPlotsPanel(tab, choic)
  ) |>
    paste0(collapse = ",\n")

  "bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      bslib::accordion(
        {content}
      )
    ),
    bslib::navset_card_tab(
      {panels}
    )
  )" |>
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
    paste0(prefix, "_", x), formatTit(x), cast(choi[[x]]), cast(choi[[x]]), TRUE
  )) |>
    paste0(collapse = ",\n")
  'bslib::accordion_panel(
    title = "{tit}",
    {selectors}
  )' |>
    glue::glue() |>
    as.character()
}
