
createUi <- function(choices = list(), panels = list()) {
  if (length(choices) == 0) return(character())

  # create a list with all the panel content
  content <- names(choices) |>
    rlang::set_names() |>
    purrr::map(\(x) panelSidebar(x, choices[[x]]))

  # group panels using the panels list
  panels |>
    purrr::imap_chr(\(x, nm) {
      if (is.list(x)) {
        c(
          'bslib::nav_menu(',
          paste0('title = ', cast(nm), ','),
          'icon = shiny::icon("list"),',
          x |>
            purrr::imap(\(xx, nam) {
              getPanel(title = nam, icon = xx, content = content[[xx]])
            }) |>
            paste0(collapse = ",\n"),
          ')'
        ) |>
          paste0(collapse = "\n")
      } else {
        getPanel(title = nm, icon = x, content = content[[x]])
      }
    }) |>
    paste0(collapse = ",\n")
}
getPanel <- function(title, icon, content) {
  c(
    'bslib::nav_panel(',
    c(
      paste0('title = ', cast(title)),
      panelIcon(icon),
      content
    ) |>
      paste0(collapse = ",\n"),
    ')'
  ) |>
    paste0(collapse = '\n')
}
getInfo <- function(rt, info, def) {
  x <- omopViewerTabs[[info]][omopViewerTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
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
