
#' Create a `bslib::card()` object from a `<summarised_result>` object.
#'
#' @param result A `<summarised_result>` object.
#'
#' @return A `bslib::card()` object.
#' @export
#'
cardSummary <- function(result) {
  # check input
  result <- omopgenerics::validateResultArgument(result)

  # result overview
  overview <- resultOverview(result)

  # packages versions
  packages <- resultVersions(result)

  # result suppression
  suppression <- resultSuppression(result)

  # explore result settings
  sets <- omopgenerics::settings(result) |>
    dplyr::relocate(dplyr::any_of(c(
      "result_id", "result_type", "min_cell_count", "package_name",
      "package_version", "group", "strata", "additional"
    )))

  bslib::card(
    bslib::card_header("Results summary"),
    createBody(c(overview, "", packages, "", suppression, "", "### Explore settings")),
    DT::datatable(sets, options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  )
}
num <- function(x) {
  format(x, big.mark = ',')
}
resultOverview <- function(result){
  sets <- omopgenerics::settings(result)

  # extract general info
  nrows <- nrow(result)
  resultIds <- nrow(sets)
  s1 <- "- Results contain **{num(nrows)}** rows with **{resultIds}** different result_ids." |>
    glue::glue()

  # present result_types
  resultTypes <- unique(sets$result_type)
  s2 <- "- Results contain **{num(length(resultTypes))}** different result types: `{glue::glue_collapse(resultTypes, sep = '`, `', last = '` and `')}`." |>
    glue::glue()

  # present cdm names
  cdmNames <- unique(result$cdm_name)
  s3 <- "- Results contain data from **{num(length(cdmNames))}** different cdm objects: \"*{glue::glue_collapse(cdmNames, sep = '*\", \"*', last = '*\" and \"*')}*\"." |>
    glue::glue()

  c("### Result overview", s1, s2, s3)
}
resultVersions <- function(result) {
  sets <- omopgenerics::settings(result)
  x <- sets |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      result_ids = paste0(.data$result_id, collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::inner_join(
      sets |>
        dplyr::group_by(.data$package_name) |>
        dplyr::summarise(n = dplyr::n_distinct(.data$package_version)),
      by = "package_name"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$n), .data$package_name, .data$package_version
    ) |>
    dplyr::mutate(message = paste0(
      dplyr::if_else(.data$n > 1, "x", "v"),
      "- **", .data$package_name, "** ", .data$package_version,
      dplyr::if_else(.data$n > 1, paste0(": ", .data$result_ids), "")
    ))

  c("### Package versions", x$message)
}
resultSuppression <- function(result) {
  sets <- omopgenerics::settings(result)
  if (!"min_cell_count" %in% colnames(sets)) {
    sets <- dplyr::mutate(sets, "min_cell_count" = 0L)
  }
  x <- sets |>
    dplyr::select("result_id", "min_cell_count") |>
    dplyr::mutate(
      "min_cell_count" = as.integer(.data$min_cell_count),
      "min_cell_count" = dplyr::if_else(
        is.na(.data$min_cell_count) | .data$min_cell_count <= 1L,
        0L,
        .data$min_cell_count
      )
    ) |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::tally() |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(message = dplyr::if_else(
      .data$min_cell_count == 0L,
      paste0("x- **", .data$n, "** not suppressed results."),
      paste0("v- **", .data$n, "** suppressed results at minCellCount = `", .data$min_cell_count, "`.")
    ))

  c("### Result suppression", x$message)
}
createBody <- function(x) {
  purrr::map_chr(x, \(xx) {
    s <- substr(xx, 1, 2)
    tx <- substr(xx, 3, nchar(xx))
    if (s == "v-") {
      xx <- paste0('- <span style="color:green">', tx, '</span>')
    } else if (s == "x-") {
      xx <- paste0('- <span style="color:red">', tx, '</span>')
    }
    return(xx)
  }) |>
    shiny::markdown()
}

summaryTab <- function(summary) {
  if (!summary) return(character())
  'bslib::nav_panel(
    title = "Summary",
    icon = shiny::icon("file-alt"),
    omopViewer::cardSummary(data)
  )'
}
