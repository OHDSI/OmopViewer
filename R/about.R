
# INTERNAL
# About tab is defined here. This is the first tab that appears in all shiny
# apps.
aboutTab <- function() {
  title <- shiny::tagList(shiny::strong("omopViewer"), "shiny app")
  subtitle <- shiny::tagList(
    "This shiny app was generated with ",
    shiny::a(
      "omopViewer",
      href = "https://github.com/oxford-pharmacoepi/omopViewer",
      target = "_blank"),
    shiny::strong(paste0(
      "v", utils::packageVersion("omopViewer") |> as.character()))
  )
  description <- "omopViewer works only with `summarised_result` objects as
  described in omopgenerics package."
  shiny::div(
    class = "about",
    shiny::tags$h2(title),
    shiny::tags$h4(subtitle),
    shiny::tags$h5(description),
    # TO ADD omopViewer hexsticker
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
