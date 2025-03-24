
createUiPanels <- function(panelDetails) {
  if (length(panelDetails) == 0) return(list())

  # create a list with all the panel content
  panelDetails |>
    purrr::imap(\(x, nm) {
      sidebar <- createSidebar(prefix = nm, filters = x$filters)
      outputPanels <- createContent(prefix = nm, content = x$content) |>
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
  if (length(panels) == 0) return(character())
  panelStructure |>
    purrr::imap(\(x, nm) {
      if (length(x) == 1 & (is.numeric(nm) | nm == "")) {
        panels[[x]]
      } else {
        'bslib::nav_menu(
          title = {cast(nm)},
          icon = shiny::icon("list"),
          {paste0(panels[x], collapse = ",\n")}
        )' |>
          glue::glue() |>
          as.character()
      }
    }) |>
    paste0(collapse = ",\n") |>
    invisible()
}
panelIcon <- function(icon) {
  if (is.null(icon)) return(character())
  paste0('icon = shiny::icon("', icon, '")')
}
createSidebar <- function(prefix, filters) {
  paste0(
    "sidebar = bslib::sidebar(\n",
    filters |>
      purrr::imap(\(x, nm) {
        x$inputId <- paste0("'", prefix, "_", nm, "'")
        createButton(x)
      }),
    ",\nposition = 'left'\n)"
  )
}
populateValues <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(x) {
      # filter result
      res <- filterResult(result, x$result_id, x$result_type)

      # get values
      values <- res |>
        omopgenerics::splitAll() |>
        omopgenerics::addSettings() |>
        dplyr::select(!c("result_id", "estimate_type", "estimate_value")) |>
        purrr::map(unique)
      values$group <- omopgenerics::groupColumns(res)
      values$strata <- omopgenerics::strataColumns(res)
      values$additional <- omopgenerics::additionalColumns(res)
      values$settings <- omopgenerics::settingsColumns(res)

      # populate filters
      x$filters <- x$filters |>
        purrr::imap(\(x, nm) {
          if (nm %in% c("choices", "selected")) {
            x <- substitueValues(x, values)
          }
          x
        })

      x
    })
}
substitueValues <- function(x, values) {
  if (length(x) == 1 & any(stringr::str_detect(x, "\\$"))) {
    return(x)
  }
  id <- stringr::str_detect(x, "^<.*>$")
  for (k in seq_along(x)) {
    if (id[k]) {
      keyWord <- substr(x, 2, nchar(x) - 1)
      x[k] <- paste0(values[[keyWord]], collapse = "', '")
    }
  }
  paste0("c('", paste0(x, collapse = "', '"), "')")
}

