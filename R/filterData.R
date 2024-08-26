
#' This function is used to filter data in shinys that inputs are defined
#' following.
#'
#' @param result A summarised_result object.
#' @param resultType A resultType of interest to filter by.
#' @param input Input of the shiny to filter by.
#'
#' @return The filtered result.
#' @export
#'
filterData <- function(result,
                       resultType,
                       input) {
  result <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType)
  if (length(input) == 0) {
    inputs <- character()
  } else {
    inputs <- names(input)
  }

  # filter settings
  set <- omopgenerics::settings(result)
  setPrefix <- paste0(resultType, "_settings_")
  toFilter <- inputs[startsWith(inputs, setPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(setPrefix)+1, nchar(fil))
    if (nm %in% colnames(set)) {
      set <- set |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }
  result <- result |>
      dplyr::filter(.data$result_id %in% set$result_id)

  # filter groupping
  cols <- c(
    "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
    "additional_name", "additional_level")
  groupCols <- visOmopResults::groupColumns(result)
  strataCols <- visOmopResults::strataColumns(result)
  additionalCols <- visOmopResults::additionalColumns(result)
  group <- result |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct() |>
    visOmopResults::splitAll()

  groupPrefix <- paste0(resultType, "_groupping_")
  toFilter <- inputs[startsWith(inputs, groupPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(groupPrefix)+1, nchar(fil))
    if (nm %in% colnames(group)) {
      group <- group |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }
  result <- result |>
    dplyr::inner_join(
      group |>
        visOmopResults::uniteGroup(cols = groupCols) |>
        visOmopResults::uniteStrata(cols = strataCols) |>
        visOmopResults::uniteAdditional(cols = additionalCols),
      by = cols
    )

  # filter variables and estimates
  varPrefix <- paste0(resultType, "_variables_and_estimates_")
  toFilter <- inputs[startsWith(inputs, varPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(varPrefix)+1, nchar(fil))
    if (nm %in% c("variable_name", "estimate_name")) {
      result <- result |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }

  # return a summarised_result
  result <- result |>
    omopgenerics::newSummarisedResult(settings = set)

  return(result)
}
