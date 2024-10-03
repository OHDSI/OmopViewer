selector <- function(id, lab, cho, sel, mult) {
  'shiny::selectizeInput(
    inputId = "{id}",
    label = "{lab}",
    choices = {cho},
    selected = {sel},
    multiple = {mult},
    options = list(plugins = "remove_button")
  )' |>
    glue::glue() |>
    as.character()
}
downloadTable <- function(id, lab, type = NULL) {

  if (is.null(type)) {
    type <- ""
  } else {
    type <- paste0('{selector("', id, '_type", "File", {cast(type)}, {cast("docx")}, FALSE)},') |> glue::glue()
  }

  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      {type}
      shiny::downloadButton(outputId = "{id}", label = "{lab}")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    as.character()
}
downloadPlot <- function(outputId, id) {
  output <- omopViewerPlots$output[omopViewerPlots$plot_id == id]
  buttons <- switch(
    output,
    "ggplot2" = 'shiny::numericInput(inputId = "{outputId}_width", label = "Width", value = 15),
      shiny::numericInput(inputId = "{outputId}_height", label = "Height", value = 10),
      {selector("{outputId}_units", "Units", {cast(c("px", "cm", "inch"))}, {cast("cm")}, FALSE)},
      shiny::numericInput(inputId = "{outputId}_dpi", label = "dpi", value = 300)' |>
      glue::glue() |>
      glue::glue(),
    "grViz" = 'shiny::numericInput(inputId = "{outputId}_width", label = "Width (px)", value = 15),
      shiny::numericInput(inputId = "{outputId}_height", label = "Height (px)", value = 10)' |>
      glue::glue() |>
      glue::glue()
  )

  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      {buttons},
      shiny::downloadButton(outputId = "{outputId}", label = "Download png")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    as.character()
}
