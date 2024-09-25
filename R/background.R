createBackground <- function(background = NULL,
                             logo = NULL) {
  if (length(background) == 0) return("")
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

  return(
    'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
    bslib::card({fillCard(background)})
    {logoImg}
  )' |>
      glue::glue() |>
      as.character()
  )
}

fillCard <- function(background) {
  purrr::imap(background, ~ switch(.y,
                                "header" = glue::glue("bslib::card_header(shiny::markdown('{.x}'))"),
                                "title" = glue::glue("bslib::card_title(shiny::markdown('{.x}'))"),
                                "body" = glue::glue("bslib::card_body(shiny::markdown('{.x}'))"),
                                "footer" = glue::glue("bslib::card_footer(shiny::markdown('{.x}'))")
  )) |>
    unlist() |>
    paste0(collapse = ", ")
}

