aboutTab <- function() {
  shiny::div(
  class = "about",
  shiny::tags$h2(
    tags$span("Shiny App to review PhenotypeR results"))  ,
  shiny::tags$h4( tags$span("Here more information about the use of the app ")),
  img(
    src = here::here("extras/www/images/hds_logo.svg"),
    class = "logo-img",
    alt = "HDS Logo",
    height = "10%",
    width = "10%",
    style="float:right"
  )
  )}

