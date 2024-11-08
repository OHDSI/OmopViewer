
#' Prepare a list of summarised_result objects.
#'
#' @param result A summarised_result object
#' @param panelDetails A list ith the panel details.
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
