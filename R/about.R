
createAbout <- function(logo) {
  'bslib::nav_item(
    bslib::popover(
      shiny::icon("circle-info"),
      shiny::tags$img(
        src = "{logo}",
        class = "logo-img",
        alt = "Logo",
        height = "auto",
        width = "30%",
        style = "float:right"
      ),
      "This shiny app was generated with ",
      shiny::a(
        "OmopViewer",
        href = "https://github.com/OHDSI/OmopViewer",
        target = "_blank"
      ),
      shiny::strong("v{as.character(utils::packageVersion("OmopViewer"))}")
    )
  )' |>
    glue::glue() |>
    as.character()
}
