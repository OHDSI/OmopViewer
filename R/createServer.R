
createServer <- function(resultTypes, data) {
  purrr::map_chr(resultTypes, \(x) {
    c(glue::glue("# {x} -----"),
      glue::glue("## raw {x} -----"),
      rawServer(x, data),
      glue::glue("## tidy {x} -----"),
      tidyServer(x, data),
      glue::glue("## formatted {x} -----"),
      formattedServer(x, data),
      glue::glue("## plot {x} -----"),
      plotsServer(x, data),
      "\n"
    ) |>
      paste0(collapse = "\n")
  })
}
