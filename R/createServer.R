
createServer <- function(resultTypes, data) {
  purrr::map_chr(resultTypes, \(x) {
    c(glue::glue("# {x} -----"),
      glue::glue("## update inputs {x} -----"),
      updateVariableNameInput(x, data),
      updateLargeScaleConceptIdInput(x, data),
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


updateVariableNameInput <- function(rt, data) {
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



updateLargeScaleConceptIdInput <- function(rt, data) {
  if(rt == "summarise_large_scale_characteristics"){
  val <- paste0(
    "updateSelectizeInput(session, ",
    '"',
    "summarise_large_scale_characteristics_grouping_concept_id", '"',
    ", ",
    "choices = unique(visOmopResults::filterSettings(data, result_type ==",
    '"',
    rt,
    '"',
    ")$additional_level)", ", ",
    "selected = unique(visOmopResults::filterSettings(data, result_type ==",
    '"',
    rt,
    '"',
    ")$additional_level)",
    ", ",
    "server = TRUE)"
  )

  val |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
  } else {
    invisible(NULL)
  }
}
