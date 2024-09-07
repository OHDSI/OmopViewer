
createServer <- function(resultTypes) {
  purrr::map_chr(resultTypes, \(x) {
    c(glue::glue("# {x} -----"),
      glue::glue("## raw {x} -----"),
      rawServer(x),
      glue::glue("## tidy {x} -----"),
      tidyServer(x),
      glue::glue("## formatted {x} -----"),
      formattedServer(x),
      glue::glue("## plot {x} -----"),
      plotsServer(x),
      "\n"
    ) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n")
}
