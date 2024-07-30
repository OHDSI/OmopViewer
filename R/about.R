#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
aboutTab <- function() {
  shiny::div(
    class = "about",
    shiny::tags$h2(shiny::tags$span("Shiny App to review PhenotypeR results")),
    shiny::tags$h4(shiny::tags$span("Here more information about the use of the app")),
    shiny::tags$img(
      src = system.file("www/images/hds_logo.svg", package = "omopViewer"),
      class = "logo-img",
      alt = "HDS Logo",
      height = "10%",
      width = "10%",
      style = "float:right"
    )
  )
}
