
#' Add the strata, group and additional columns to settings with the tidyColumns
#' (`visOmopResults::splitGroup()`, `visOmopResults::splitStrata()`,
#' `visOmopResults::splitAdditional()`) for each 'result_id'.
#'
#' @param result A `<summarised_result>` object.
#'
#' @export
#'
correctSettings <- function(result) {
  # check input
  result <- omopgenerics::validateResultArgument(result)

  set <- omopgenerics::settings(result)

  cols <- c("group", "strata", "additional")
  cols <- cols[cols %in% colnames(set)]
  if (length(cols) > 0) {
    cli::cli_warn(c("!" = "{.var {cols}} will be overwritten in settings."))
  }

  # obtain group, strata, and additional at
  x <- result |>
    dplyr::select("result_id", "strata_name", "group_name", "additional_name") |>
    dplyr::distinct()
  group <- rep("", nrow(set))
  strata <- rep("", nrow(set))
  additional <- rep("", nrow(set))
  for (k in seq_len(nrow(set))) {
    xk <- x |>
      dplyr::filter(.data$result_id == .env$set$result_id[k])
    group[k] <- visOmopResults::groupColumns(xk) |> joinCols()
    strata[k] <- visOmopResults::strataColumns(xk) |> joinCols()
    additional[k] <- visOmopResults::additionalColumns(xk) |> joinCols()
  }

  # correct settings
  set <- set |>
    dplyr::mutate(
      group = .env$group, strata = .env$strata, additional = .env$additional
    )

  return(omopgenerics::newSummarisedResult(result, settings = set))
}
joinCols <- function(x) {
  if (length(x) == 0) return(NA_character_)
  stringr::str_flatten(x, collapse = " &&& ")
}
