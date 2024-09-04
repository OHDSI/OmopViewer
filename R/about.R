
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
    shiny::tags$img(
      src = "hds_logo.svg",
      class = "logo-img",
      alt = "Logo",
      height = "10%",
      width = "10%",
      style = "float:right"
    ),
    shiny::tags$h2({title}),
    shiny::tags$h4({subtitle}),
    shiny::tags$h5({description})
  )' |>
    glue::glue() |>
    styleCode() |>
    paste0(collapse = "\n")
}
createAbout <- function(about = TRUE) {
  if (!about) return("")
  'bslib::nav_item(
    bslib::popover(
      shiny::icon("circle-info"),
      shiny::tags$img(
        src = "hds_logo.svg",
        class = "logo-img",
        alt = "Logo",
        height = "auto",
        width = "30%",
        style = "float:right"
      ),
      "This shiny app was generated with ",
      shiny::a(
        "omopViewer",
        href = "https://github.com/oxford-pharmacoepi/omopViewer",
        target = "_blank"
      ),
      shiny::strong("v{as.character(utils::packageVersion("omopViewer"))}")
    )
  )' |>
    glue::glue() |>
    as.character()
}
