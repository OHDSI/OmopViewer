
writeUiPanels <- function(panelDetails) {
  # create a list with all the panel content
  panelDetails |>
    purrr::imap(\(x, nm) {
      if (length(x$filters) > 0) {
        sidebar <- writeSidebar(prefix = nm, filters = x$filters, position = "left") |>
          paste0(",")
      } else {
        sidebar <- ""
      }
      outputPanels <- writeContent(prefix = nm, content = x$content) |>
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
    paste0(collapse = ",\n")
}
panelIcon <- function(icon) {
  if (is.null(icon)) return(character())
  paste0('icon = shiny::icon("', icon, '")')
}
writeSidebar <- function(prefix, filters, position) {
  paste0(
    "sidebar = bslib::sidebar(\n",
    filters |>
      purrr::imap(\(x, nm) {
        if (identical(x$selected, "selected$")) {
          x$selected <- paste0("selected$", prefix, "_", nm)
        }
        if (identical(x$choices, "choices$")) {
          x$choices <- paste0("choices$", prefix, "_", nm)
        }
        createButton(x)
      }) |>
      paste0(collapse = ",\n"),
    ",\nposition = '",
    position,
    "'\n)"
  )
}
writeContent <- function(prefix, content) {
  content |>
    purrr::imap(\(x, nm) {
      id <- paste0(prefix, "_", nm)
      out <- writeOutput(ot = x$output_type, id = id)
      download <- writeDownload(do = x$download, id = id)
      if (length(x$filters) > 0) {
        sb <- writeSidebar(prefix = id, filters = x$filters, position = "right")
        res <- paste0("bslib::layout_sidebar(\n", sb, ",\n", out, "\n)")
      } else {
        res <- out
      }
      paste0(
        'bslib::nav_panel(\ntitle = "',
        x$title,
        '",\nbslib::card(\nfull_screen = TRUE,\n',
        download,
        res,
        "\n)\n)"
      )
    }) |>
    paste0(collapse = ",\n")
}
writeOutput <- function(ot, id) {
  res <- switch(ot,
                "DT" = "DT::DTOutput",
                "gt" = "gt::gt_output",
                "plot" = "shiny::plotOutput")
  paste0(res, '("', id, '")')
}
writeDownload <- function(do, id) {
  if (length(do) == 0) return("")
  paste0(
    'bslib::card_header(\nbslib::popover(\nshiny::icon("download"),\nshiny::downloadButton(outputId = "',
    id, '_download", label = "', do$label, '")\n),\nclass = "text-end"\n),\n'
  )
}
