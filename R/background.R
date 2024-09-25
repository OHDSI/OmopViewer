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

validateBackground <- function(background) {
  if (length(background) == 1) {
    background <- tryCatch(
      expr = {styler::style_text(background) |> suppressWarnings()},
      error = function(e) {
        cli::cli_warn(
          c("!" = "If a `bslib` code synthax was supplied in `background`, this couldn't be styled and will be ignored. The error is the following:",
            "x" = "{e$message}")
        )
        NULL
      }
    )
  } else {
    omopgenerics::assertCharacter(x = background, null = TRUE, named = TRUE)
    notAllowed <- ! names(background) %in% c("header", "title", "body", "footer")
    if (sum(notAllowed) > 0) {
      cli::cli_warn("{background[notAllowed]} {?is/are} not allowed named for `background` and will be ignored.")
      background <- background[!notAllowed]
    }
  }
  return(invisible(background))
}
