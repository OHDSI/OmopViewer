
createUiPanels <- function(panelDetails) {
  if (length(panelDetails) == 0) return(character())

  # create a list with all the panel content
  panelDetails |>
    purrr::imap(\(x, nm) {
      sidebar <- createSidebar(prefix = nm, filters = x$filters, information = x$information)
      outputPanels <- c(
        tidyUi(tab = nm), outputUi(tab = nm, choic = x$filters)
      ) |>
        paste0(collapse = ",\n")
      c(
        'bslib::nav_panel(',
        c(
          paste0('title = ', cast(x$title)),
          panelIcon(x$icon),
          "bslib::layout_sidebar(
            {sidebar}
            bslib::navset_card_tab(
              {outputPanels}
            )
          )" |>
            glue::glue() |>
            as.character()
        ) |>
          paste0(collapse = ",\n"),
        ')'
      ) |>
        paste0(collapse = '\n')
    })
}
structurePanels <- function(panels, panelStructure) {
  panelStructure |>
    purrr::imap(\(x, nm) {
      if (length(x) == 1 & is.numeric(nm)) {
        panels[[x]]
      } else {
        'bslib::nav_menu(
          title = {cast(nm)},
          {paste0(panels[x], collapse = ",\n")},
          icon = "list"
        )' |>
          glue::glue() |>
          as.character()
      }
    }) |>
    paste0(collapse = ",\n")
}
panelIcon <- function(icon) {
  if (is.null(icon)) return(character())
  paste0('icon = shiny::icon("', icon, '")')
}
