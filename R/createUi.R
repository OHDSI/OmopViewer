
createUi <- function(choices = list()) {
  n <- length(names(choices))
  x <- purrr::map_chr(names(choices), \(x) {
    nm <- names(choices[[x]])
    if (x %in% paste0("id_", 1:n)) {
      c(
        'bslib::nav_panel(',
        c(panelTitle(nm), panelIcon(nm), panelSidebar(nm, choices[[x]][[nm]])) |>
          paste0(collapse = ",\n"),
        ')'
      ) |>
        paste0(collapse = "\n")
    } else {
      c(
        'bslib::nav_menu(',
        'title = "{x}",' |> glue::glue(),
        purrr::map_chr(nm, \(y) {
          c(
            'bslib::nav_panel(',
            c(panelTitle(y), panelIcon(y), panelSidebar(y, choices[[x]][[y]])) |>
              paste0(collapse = ",\n"),
            '),'
          ) |>
            paste0(collapse = "\n")
        }),
        'icon = shiny::icon("rectangle-list")',
        ')'
      ) |>
        paste0(collapse = "\n")
    }
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
    rawUi(tab),
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
