
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
downloadTable <- function(id, lab) {
  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(outputId = "{id}", label = "{lab}")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    as.character()
}
downloadPlot <- function(outputId) {
  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      shiny::numericInput(inputId = "{outputId}_width", label = "width", value = 15),
      shiny::numericInput(inputId = "{outputId}_height", label = "height", value = 10),
      {selector("{outputId}_units", "Units", {cast(c("px", "cm", "inch"))}, {cast("cm")}, FALSE)},
      shiny::numericInput(inputId = "{outputId}_dpi", label = "dpi", value = 300),
      shiny::downloadButton(outputId = "{outputId}", label = "Download png")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    glue::glue() |>
    as.character()
}
