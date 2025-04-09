
validatePanelDetails <- function(panelDetails, result, call = parent.frame()) {
  if (length(panelDetails) == 0) {
    panelDetails <- panelDetailsFromResult(result)
  } else {
    omopgenerics::assertList(panelDetails, named = TRUE, call = call)
  }
  return(panelDetails)
}
validatePanelStructure <- function(panelStructure, panels, call = parent.frame()) {
  if (is.null(panelStructure)) {
    panelStructure <- defaultPanelStructure(panels)
  }
  if (!is.list(panelStructure)) {
    panelStructure <- as.list(panelStructure)
  }
  omopgenerics::assertList(panelStructure, call = call)
  panelStructure <- panelStructure |>
    purrr::map(\(x) {
      x <- unique(as.character(x))
      x[!is.na(x)]
    }) |>
    purrr::compact()

  present <- unique(unlist(panelStructure))

  # warn eliminated
  eliminate <- present[!present %in% panels]
  if (length(eliminate) > 0) {
    cli::cli_warn("{.var {eliminate}} removed from panelStucture as not present in `panelDetails`.")
    panelStructure <- panelStructure |>
      purrr::map(\(x) x[x %in% panels]) |>
      purrr::compact()
  }

  # inform missing
  missing <- panels[!panels %in% present]
  if (length(missing) > 0) {
    cli::cli_warn(c( "!" = "{.var {missing}} panels added to panelStucture."))
    panelStructure <- c(panelStructure, as.list(missing))
  }

  return(panelStructure)
}
