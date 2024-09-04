
createBackground <- function(background, title, logo) {
  if (!background) return("")
  if (!is.null(logo)) {
    logoImg <- ',
    shiny::tags$img(
      src = "{logo}",
      width = "auto",
      height = "100px",
      alt = "logo",
      align = "left"
    )' |>
      glue::glue() |>
      as.character()
  } else {
    logoImg <- ""
  }
  'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
    bslib::card(
      bslib::card_header("{title} background"),
      shiny::p("You can use this section to add some background of your study"){logoImg}
    )
  )' |>
    glue::glue() |>
    as.character()
}
