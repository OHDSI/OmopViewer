
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
  panels <- OmopViewer::omopViewerPanels |>
    purrr::keep(\(x) {
      if (is.null(x$data$result_type)) {
        FALSE
      } else {
        x$data$result_type %in% resultTypes
      }
    })

  # present default types
  presentResultType <- panels |>
    purrr::map_chr(\(x) x$data$result_type) |>
    unname() |>
    unique()
  defaultPanels <- resultTypes[!resultTypes %in% presentResultType] |>
    rlang::set_names() |>
    purrr::map(\(x) {
      panel <- OmopViewer::omopViewerPanels$default
      panel$title <- formatTit(x)
      panel$data$result_type <- x
      panel
    })

  panelDetails <- c(panels, defaultPanels)

  # eliminate filters with more than omopviewer.max_length
  trimFilters(panelDetails, result)
}

defaultPanelStructure <- function(panels) {
  lp <- panelStructureDefaults |>
    purrr::map(\(ps) ps[ps %in% panels]) |>
    purrr::compact()
  ln <- panels[!panels %in% unlist(lp)] |>
    as.list()
  c(lp, ln)
}
trimFilters <- function(panelDetails, result) {
  len <- getOption(x = "omopviewer.max_length", default = "100") |>
    as.integer()
  if (!is.infinite(len) & !is.na(len)) {
    if (len >= 1) {
      resultList <- purrr::map(panelDetails, \(x) x$data)
      toExclude <- prepareResult(result = result, resultList = resultList) |>
        purrr::map(\(x) {
          x |>
            omopgenerics::addSettings() |>
            omopgenerics::splitAll() |>
            dplyr::select(!c("result_id", "estimate_name", "estimate_type", "estimate_value")) |>
            purrr::map(\(x) length(unique(x))) |>
            purrr::keep(\(x) x > len) |>
            names()
        }) |>
        purrr::compact()
      for (nm in names(toExclude)) {
        panelDetails[[nm]]$exclude_filters <- unique(c(panelDetails[[nm]]$exclude_filters, toExclude[[nm]]))
      }
    }
  }

  return(panelDetails)
}
