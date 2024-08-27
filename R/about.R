
aboutTab <- function() {
  title <- 'shiny::tagList(shiny::strong("omopViewer"), "shiny app")'
  subtitle <- 'shiny::tagList(
    "This shiny app was generated with ",
    shiny::a(
      "omopViewer",
      href = "https://github.com/oxford-pharmacoepi/omopViewer",
      target = "_blank"),
    shiny::strong("v{as.character(utils::packageVersion("omopViewer"))}")
  )' |>
    glue::glue()
  description <- '"omopViewer works only with `summarised_result` objects as
  defined in omopgenerics package."'
  'shiny::div(
    class = "about",
    shiny::tags$h2({title}),
    shiny::tags$h4({subtitle}),
    shiny::tags$h5({description}),
    shiny::tags$img(
      src = system.file("www/images/hds_logo.svg", package = "omopViewer"),
      class = "logo-img",
      alt = "HDS Logo",
      height = "10%",
      width = "10%",
      style = "float:right"
    )
  )' |>
    glue::glue() |>
    styleCode() |>
    paste0(collapse = "\n")
}
