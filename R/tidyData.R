
#' Get a tidy tibble from a `summarised_result` object.
#'
#' @param result A `summarised_result` object.
#' @param showSettings Whether to show settings columns.
#' @param showgrouping Whether to show grouping columns.
#' @param prefixSet Prefix for settings columns.
#' @param prefixGroup Prefix for grouping columns.
#' @param pivot What to pivot: "none", "estimates" or "estimates and variables".
#'
#' @return A tibble.
#' @export
#'
tidyData <- function(result,
                     showSettings = TRUE,
                     showgrouping = TRUE,
                     prefixSet = "",
                     prefixGroup = "",
                     pivot = "none") {
  # initial checks
  omopgenerics::assertClass(result, "summarised_result")
  omopgenerics::assertLogical(showSettings, length = 1)
  omopgenerics::assertLogical(showgrouping, length = 1)
  omopgenerics::assertCharacter(prefixSet, length = 1, null = TRUE)
  omopgenerics::assertCharacter(prefixGroup, length = 1, null = TRUE)
  omopgenerics::assertChoice(
    pivot,
    choices = c("none", "estimates", "estimates and variables"),
    length = 1)

  # add settings
  if (showSettings) {
    set <- omopgenerics::settings(result) |>
      dplyr::filter(.data$result_id %in% unique(result$result_id))
    groups <- set |>
      dplyr::select(c("result_id", "group", "strata", "additional")) |>
      tidyr::pivot_longer(c("group", "strata", "additional")) |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::group_by(.data$result_id) |>
      tidyr::separate_rows(value, sep = " &&& ") |>
      dplyr::pull("value") |>
      unique()
    set <- set |>
      dplyr::select(!dplyr::all_of(c("group", "strata", "additional")))
    for (col in colnames(set)) {
      if (all(is.na(set[[col]]))) {
        set <- set |>
          dplyr::select(!dplyr::all_of(col))
      }
    }
    if (ncol(set) > 1) {
      set <- set |>
        dplyr::rename_with(~ paste0(prefixSet, .x), !"result_id")
    }
    result <- set |>
      dplyr::inner_join(result, by = "result_id") |>
      dplyr::select(!"result_id")
  }

  # To be removed when visOmopResults pivotEstimates does not require
  # summarise_result class
  class(result) <- unique(c("summarised_result", class(result)))

  # pivot
  if (pivot == "estimates") {
    result <- result |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = "estimate_name")
  } else if (pivot == "estimates and variables") {
    result <- result |>
      visOmopResults::pivotEstimates(
        pivotEstimatesBy = c("variable_name", "variable_level", "estimate_name")
      )
  }

  # grouping
  if (showgrouping) {
    cols <- c(
      "cdm_name",
      visOmopResults::groupColumns(result),
      visOmopResults::strataColumns(result),
      visOmopResults::additionalColumns(result)
    )
    result <- result |>
      visOmopResults::splitAll() |>
      dplyr::relocate(dplyr::all_of(cols), .after = "cdm_name") |>
      dplyr::rename_with(~ paste0(prefixGroup, .x), dplyr::all_of(cols))
    groupsMiss <- groups[!groups %in% colnames(result)]
    if (length(groupsMiss) > 0) {
      overallCols <- "\"overall\"" |>
        rlang::parse_expr() |>
        rlang::set_names(groupsMiss)
      result <- result |>
        dplyr::mutate(!!!overallCols)
    }
  } else {
    result <- result |>
      dplyr::select(!c(
        "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
        "additional_name", "additional_level"))
  }

  return(result)
}
