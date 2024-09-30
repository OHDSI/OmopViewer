
createServer <- function(resultTypes, data) {
  purrr::map_chr(resultTypes, \(x) {
    c(glue::glue("# {x} -----"),
      glue::glue("## updateSelectizeInput {x} -----"),
      updateInputs(x, data),
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


updateInputs  <- function(rt, data) {
  val <- paste0(
    "updateSelectizeInput(session, ",
    '"',
    rt, "_variable_name", '"',
    ", ",
    "choices = unique(visOmopResults::filterSettings(data, result_type ==",
    '"',
    rt,
    '"',
    ")$variable_name)", ", ",
    "selected = unique(visOmopResults::filterSettings(data, result_type ==",
    '"',
    rt,
    '"',
    ")$variable_name)",
    ", ",
    "server = TRUE)"
  )

  val |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
