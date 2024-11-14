
#' This function is used to filter data in shinys that inputs are defined
#' following.
#'
#' @param result A summarised_result object.
#' @param prefix Prefix for the inputs.
#' @param input Input of the shiny to filter by.
#'
#' @return The filtered result.
#' @export
#'
filterData <- function(result,
                       prefix,
                       input) {
  result <- result[[prefix]]

  if (nrow(result) == 0) return(emptySummarisedResult())

  if (length(input) == 0) inputs <- character() else inputs <- names(input)

  # subset to inputs of interest
  inputs <- inputs[startsWith(inputs, prefix)]

  # filter settings
  set <- omopgenerics::settings(result)
  setPrefix <- paste0(c(prefix, "settings_"), collapse = "_")
  toFilter <- inputs[startsWith(inputs, setPrefix)]
  nms <- substr(toFilter, nchar(setPrefix)+1, nchar(toFilter))
  for (nm in nms) {
    if (nm %in% colnames(set)) {
      set <- set |>
        dplyr::filter(as.character(.data[[nm]]) %in% input[[paste0(setPrefix, nm)]])
    }
  }
  result <- result |>
      dplyr::filter(.data$result_id %in% set$result_id)

  if (nrow(result) == 0) return(emptySummarisedResult())

  # filter grouping
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
  groupPrefix <- paste0(c(prefix, "grouping_"), collapse = "_")
  toFilter <- inputs[startsWith(inputs, groupPrefix)]
  nms <- substr(toFilter, nchar(groupPrefix)+1, nchar(toFilter))
  for (nm in nms) {
    if (nm %in% colnames(group)) {
      group <- group |>
        dplyr::filter(.data[[nm]] %in% input[[paste0(groupPrefix, nm)]])
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
  nms <- c("variable_name", "estimate_name")
  nms <- nms[paste0(prefix, "_", nms) %in% inputs]
  for (nm in nms) {
    result <- result |>
      dplyr::filter(.data[[nm]] %in% input[[paste0(prefix, "_", nm)]])
  }

  # return a summarised_result
  result <- result |>
    omopgenerics::newSummarisedResult(settings = set)

  return(result)
}
