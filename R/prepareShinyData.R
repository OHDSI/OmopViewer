
#' Prepare a list of summarised_result objects.
#'
#' @param result A summarised_result object
#' @param panelDetails A list with the panel details.
#'
#' @return A list of summarised_result objects.
#' @export
#'
prepareShinyData <- function(result,
                             panelDetails = NULL) {
  # check inputs
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertList(panelDetails, null = TRUE)
  panelDetails <- validatePanelDetails(panelDetails, result)

  prepareResult(panelDetails = panelDetails, result = result)
}
prepareResult <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(x) {
      result |>
        dplyr::filter(.data$result_id %in% .env$x$result_id) |>
        omopgenerics::newSummarisedResult(
          settings = omopgenerics::settings(result) |>
            dplyr::filter(.data$result_id %in% .env$x$result_id)
        )
    })
}
