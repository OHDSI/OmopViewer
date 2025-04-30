
# static ----
uiStatic <- function(logo,
                     title,
                     background,
                     summary,
                     theme,
                     panelDetails,
                     panelStructure) {
  # create panels
  panels <- writeUiPanels(panelDetails) |>
    structurePanels(panelStructure)

  # ui
  c(
    messageShiny(),
    "ui <- bslib::page_navbar(",
    c(
      writeTitle(title, logo),
      paste0("theme = ", theme),
      createBackground(background),
      summaryTab(summary),
      panels,
      "bslib::nav_spacer()",
      downloadRawDataUi(),
      createAbout("hds_logo.svg"),
      'bslib::nav_item(bslib::input_dark_mode(id ="dark_mode", mode = "light"))'
    ) |>
      paste0(collapse = ",\n"),
    ")"
  ) |>
    paste0(collapse = "\n") |>
    styleCode()
}
downloadRawDataUi <- function() {
  'bslib::nav_item(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(
        outputId = "download_raw",
        label = "Download raw data",
        icon = shiny::icon("download")
      )
    )
  )'
}

# dynamic ----

# functions ----
writeTitle <- function(title, logo) {
  if (is.null(logo)) {
    x <- 'title = "{title}"'
  } else {
    x <- 'title = shiny::tags$span(
      shiny::tags$img(
        src = "{logo}",
        width = "auto",
        height = "46px",
        class = "me-3",
        alt = "logo"
      ),
      "{title}"
    )'
  }
  x <- glue::glue(x) |> as.character()
  return(x)
}
writeUiPanels <- function(panelDetails) {
  # create a list with all the panel content
  panelDetails |>
    purrr::map(\(x) {
      if (length(x$filters) > 0) {
        sidebar <- writeSidebar(filters = x$filters, position = "left") |>
          paste0(",")
      } else {
        sidebar <- ""
      }
      outputPanels <- writeContent(content = x$content) |>
        paste0(collapse = ",\n")
      c(
        'bslib::nav_panel(',
        c(
          paste0('title = ', cast(x$title)),
          writeIcon(x$icon),
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
writeIcon <- function(icon) {
  if (is.null(icon)) return(character())
  paste0('icon = shiny::icon("', icon, '")')
}
writeSidebar <- function(filters, position) {
  paste0(
    "sidebar = bslib::sidebar(\n",
    paste0(writeButtons(filters), collapse = ",\n"),
    ",\nposition = '",
    position,
    "'\n)"
  )
}
writeContent <- function(content) {
  content |>
    purrr::map(\(x) {
      out <- writeOutput(ot = x$output_type, id = x$output_id)
      download <- writeDownload(do = x$download)
      if (length(x$filters) > 0) {
        sb <- writeSidebar(filters = x$filters, position = "right")
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
  paste0(outputFunction(ot), '("', id, '") |>\nshinycssloaders::withSpinner()')
}
writeDownload <- function(do) {
  if (length(do) == 0) return("")
  paste0(
    'bslib::card_header(\nbslib::popover(\n',
    paste0(
      c('shiny::icon("download")', writeButtons(do$filters), paste0(
        'shiny::downloadButton(outputId = ', cast(do$output_id), ', label = ',
        cast(do$label), ')')),
      collapse = ",\n"
    ),
    '\n),\nclass = "text-end"\n),\n'
  )
}
