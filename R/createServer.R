
createServer <- function(resultTypes, data) {
  c(
    downloadRawDataServer(data),
    selectiseServer(resultTypes, data),
    purrr::map_chr(resultTypes, \(x) {
      c(glue::glue("# {x} -----"),
        glue::glue("## tidy {x} -----"),
        tidyServer(x, data),
        glue::glue("## output {x} -----"),
        outputServer(x, data),
        "\n"
      ) |>
        paste0(collapse = "\n")
    })
  )
}
selectiseServer <- function(resultTypes, data) {
  if (length(resultTypes) == 0) return(character())
  c(
    '# fill selectise variables ----',
    paste0('shiny::observe({
      for (k in seq_along(choices)) {
        shiny::updateSelectizeInput(
          session,
          inputId = names(choices)[k],
          choices = choices[[k]],
          selected = choices[[k]],
          server = TRUE
        )
      }
    })')
  )
}
