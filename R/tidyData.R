
#' Get a tidy tibble from a `summarised_result` object.
#'
#' @param result A `summarised_result` object.
#' @param cols Columns to show a part from the core ones.
#' @param pivot What to pivot: "none", "estimates" or "estimates and variables".
#'
#' @return A tibble.
#' @export
#'
tidyData <- function(result,
                     cols,
                     pivot = "none") {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertCharacter(cols, null = TRUE)
  omopgenerics::assertChoice(
    pivot,
    choices = c("none", "estimates", "estimates and variables"),
    length = 1)

  sets <- omopgenerics::settings(result)
  if (!all(c("group", "strata", "additional") %in% colnames(sets))) {
    sets <- result |>
      correctSettings() |>
      omopgenerics::settings()
  }

  groupingCols <- c(
    getCols(sets$group), getCols(sets$strata), getCols(sets$additional))

  result <- result |>
    visOmopResults::addSettings() |>
    visOmopResults::splitAll()

  notPresent <- groupingCols[!groupingCols %in% colnames(result)]
  if (length(notPresent) > 0) {
    for (col in notPresent) {
      result <- result |>
        dplyr::mutate(!!col := "overall")
    }
  }
  result <- result |>
    dplyr::relocate(groupingCols, .before = "variable_name")

  # columns to eliminate
  colsEliminate <- colnames(result)
  colsEliminate <- colsEliminate[!colsEliminate %in% c(
    cols, "variable_name", "variable_level", "estimate_name", "estimate_type",
    "estimate_value"
  )]

  # pivot
  if (pivot != "none") {
    vars <- switch(
      pivot,
      "estimates" = "estimate_name",
      "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
    )
    result <- result |>
      visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
  }

  result <- result |>
    dplyr::select(!dplyr::all_of(colsEliminate))

  return(result)
}
