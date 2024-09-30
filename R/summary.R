createSummary <- function(sum, logo) {

  if (length(sum) == 0) {return(NULL)}

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
       title = "Summary",
       icon = shiny::icon("file-alt"),
       bslib::card(
         bslib::card_header("Summary of results"),
         {styleSummary(sum)}
         {logoImg}
      )
    )' |>
      glue::glue() |>
      as.character()
  )
}

styleSummary <- function(sum) {
  sum[1] <- gsub("(\\d+)", "**\\1**", sum[1])
  sum[-1] <- paste0(" - **", gsub(": ", ":** ", sum[-1]))
  purrr::map(sum, function(x){
    glue::glue('shiny::p(shiny::markdown("{x}"))')
  }) |> unlist() |> paste0(collapse = ", ")
}
