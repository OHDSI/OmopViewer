
#' Get one of the default pre-built panels.
#'
#' @param panelId Name of the one of the default panels. Use `defaultPanels()`
#' to see the available default panels.
#'
#' @return A panel definition.
#' @export
#'
#' @examples
#'
#' getPanel("incidence")
#'
getPanel <- function(panelId) {
  # input check
  omopgenerics::assertChoice(panelId, choices = defaultPanels(), length = 1)

  OmopViewer::omopViewerPanels[[panelId]]
}

#' Default panels defined in the package.
#'
#' @return Names of the default panels defined in the package.
#' @export
#'
#' @examples
#'
#' defaultPanels()
#'
defaultPanels <- function() {
  names(OmopViewer::omopViewerPanels)
}

#' Obtain default panel details from a `<summarised_result>` object.
#'
#' @param result A `summarised_result` object.
#'
#' @return A list of `omop_viewer_panel` objects.
#' @export
#'
#' @examples
#'
#' panelDetailsFromResult(omopViewerResults)
#'
panelDetailsFromResult <- function(result) {
  # initial check
  result <- omopgenerics::validateResultArgument(result)

  # get result types
  resultTypes <- unique(omopgenerics::settings(result)$result_type)

  # get the panels that are contained in the data
  OmopViewer::omopViewerPanels |>
    purrr::keep(\(x) x$data$result_type %in% resultTypes)
}
