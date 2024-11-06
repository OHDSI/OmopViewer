
#' Prepare a list of summarised_result objects.
#'
#' @param result A summarised_result object
#' @param panelDetails A tibble with at least result_id and id as columns
#'
#' @return A list of summarised_result objects.
#' @export
#'
prepareShinyData <- function(result,
                             panelDetails = NULL) {
  # check inputs
  result <- omopgenerics::validateResultArgument(result)
  if (is.null(panelDetails)) panelDetails <- panelDetailsFromResult(result)
  omopgenerics::assertTable(panelDetails, columns = c("id", "result_id"))

  set <- omopgenerics::settings(result)

  panelDetails <- panelDetails |>
    dplyr::select("id", "result_id") |>
    dplyr::group_by(.data$id)
  if (is.character(panelDetails$result_id)) {
    panelDetails <- panelDetails |>
      dplyr::summarise(res_id = list(
        stringr::str_split(.data$result_id, "; ") |>
          unlist() |>
          unique() |>
          as.integer()
      ))
  } else {
    panelDetails <- panelDetails |>
      dplyr::summarise(res_id = list(as.integer(.data$result_id)))
  }

  x <- list()
  for (k in seq_len(nrow(panelDetails))) {
    id <- panelDetails$id[k]
    resId <- panelDetails$res_id[[k]]
    x[[id]] <- result |>
      dplyr::filter(.data$result_id %in% .env$resId) |>
      omopgenerics::newSummarisedResult(
        settings = set |>
          dplyr::filter(.data$result_id %in% .env$resId)
      )
  }

  return(x)
}
