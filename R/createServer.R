
createServer <- function(panelDetails, data) {
  filterValues <- switch (data,
    "data" = "filterValues",
    "workingData()" = "filterValues()"
  )
  c(
    downloadRawDataServer(data),
    selectiseServer(panelDetails, filterValues),
    purrr::imap_chr(panelDetails, \(x, nm) {
      c(glue::glue("# {nm} -----"),
        glue::glue("## tidy {nm} -----"),
        tidyServer(nm, data),
        glue::glue("## output {nm} -----"),
        outputServer(nm, x$output_id, data),
        "\n"
      ) |>
        paste0(collapse = "\n")
    })
  )
}
selectiseServer <- function(panelDetails, filterValues) {
  if (length(panelDetails) == 0) return(character())
  paste(
    '# fill selectise variables ----',
    'shiny::observe({',
    'for (k in seq_along([filterValues])) {' |>
      glue::glue(.open = "[", .close = "]"),
    'shinyWidgets::updatePickerInput(',
    'session,',
    'inputId = names({filterValues})[k],' |>
      glue::glue(.open = "{", .close = "}"),
    'choices = {filterValues}[[k]],' |>
      glue::glue(.open = "{", .close = "}"),
    'selected = {filterValues}[[k]],' |>
      glue::glue(.open = "{", .close = "}"),
    ')',
    '}',
    '})',
    sep = "\n"
  )
}
