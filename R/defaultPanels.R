
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

  omopViewerPanels[[panelId]]
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
  names(omopViewerPanels)
}

#' Obtain default panel details from a `<summarised_result>` object.
#'
#' @param result A `summarised_result` object.
#'
#' @return
#' @export
#'
#' @examples
#'
#' panelDetailsFromResult(omopViewerResults)
#'
panelDetailsFromResult <- function(result) {
  definedPanels <- purrr::map(omopViewerPanels, \(x) x$result_type)
  result |>
    # input check
    omopgenerics::validateResultArgument() |>
    # settings
    omopgenerics::settings() |>
    # result_type
    dplyr::pull("result_type") |>
    unique() |>
    rlang::set_names() |>
    as.list() |>
    purrr::map(\(result_type) {
      id <- definedPanels |>
        purrr::keep(\(x) x %in% result_type) |>
        names()
      if (length(id) == 1) {
        omopViewerPanels[[id]]
      } else {
        NULL
      }
    }) |>
    purrr::compact() |>
    purrr::map(newOmopViewerPanel)
}

getValues <- function(x) {
  x |>
    purrr::map(\(x) {
      x |>
        omopgenerics::splitAll() |>
        omopgenerics::addSettings() |>
        dplyr::select(!c("result_id", "estimate_type", "estimate_value")) |>
        as.list() |>
        purrr::map(unique)
    })
}
