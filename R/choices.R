
#' Get the different options that a summarised_result have.
#'
#' @param result A `<summarised_result>` object.
#' @param flatten Whether to flatten to a single list or not.
#'
#' @return A named list with the options
#' @export
#'
#' @examples
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' result <- cdm$cohort1 |>
#'   summariseCharacteristics()
#'
#' getChoices(result)
#'
getChoices <- function(result, flatten = FALSE) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertLogical(flatten, length = 1)

  # get choices
  settings <- getPossibleSettings(result)
  grouping <-getPossibleGrouping(result)
  variables <- getPossibleVariables(result)

  sets <- omopgenerics::settings(result)
  if (!all(c("group", "strata", "additional") %in% colnames(sets))) {
    sets <- result |>
      correctSettings() |>
      omopgenerics::settings()
  }

  # tidy columns
  tidyCols <- sets |>
    dplyr::group_by(.data$result_type) |>
    dplyr::group_split()
  names(tidyCols) <- purrr::map_chr(tidyCols, \(x) unique(x$result_type))
  tidyCols <- tidyCols |>
    purrr::map(\(x) {
      setCols <- x |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        purrr::map(unique)
      setCols <- names(setCols)[!is.na(setCols)]
      c("cdm_name", getCols(x$group), getCols(x$strata), getCols(x$additional), setCols)
    })

  if (flatten) {
    names(tidyCols) <- paste0(names(tidyCols), "_tidy_columns")
    choices <- c(
      correctNames(settings, "settings"),
      correctNames(grouping, "grouping"),
      correctNames(variables),
      tidyCols
    )
  } else {
    choices <- names(variables) |>
      purrr::set_names() |>
      purrr::map(\(x) list(
        settings = settings[[x]],
        grouping = grouping[[x]],
        variable_name = variables[[x]]$variable_name,
        estimate_name = variables[[x]]$estimate_name,
        tidy_columns = tidyCols[[x]]
      ))
  }

  return(choices)
}
getPossibleSettings <- function(result) {
  omopgenerics::settings(result) |>
    dplyr::select(!dplyr::any_of(c(
      "result_id", "package_name", "package_version", "min_cell_count"))) |>
    getPossibilities()
}
getPossibleGrouping <- function(result) {
  result |>
    visOmopResults::addSettings(settingsColumns = "result_type") |>
    dplyr::select(c(
      "result_type", "cdm_name", "group_name", "group_level", "strata_name",
      "strata_level", "additional_name", "additional_level")) |>
    dplyr::distinct() |>
    getPossibilities(split = TRUE)
}
getPossibleVariables <- function(result) {
  result |>
    visOmopResults::addSettings(settingsColumns = "result_type") |>
    dplyr::select(c("result_type", "variable_name", "estimate_name")) |>
    dplyr::distinct() |>
    getPossibilities()
}
getPossibilities <- function(x, split = FALSE) {
  x <- x |>
    dplyr::group_by(.data$result_type) |>
    dplyr::group_split() |>
    as.list()
  names(x) <- purrr::map_chr(x, \(x) unique(x$result_type))
  uniquePos <- function(xx) {
    xx <- xx |>
      unique() |>
      as.character() |>
      sort() # To remove when the output is constant and always equal
    xx[!is.na(xx)]
  }
  getPos <- function(xx, split = FALSE) {
    xx <- xx |>
      dplyr::select(!"result_type")
    if (split) xx <- visOmopResults::splitAll(xx)
    xx |>
      as.list() |>
      purrr::map(uniquePos) |>
      vctrs::list_drop_empty()
  }
  x <- x |>
    purrr::map(getPos, split = split)
  return(x)
}
correctNames <- function(x, prefix = "") {
  if (prefix == "") {
    sub <- "_"
  } else {
    sub <- paste0("_", prefix, "_")
  }
  x <- unlist(x, recursive = FALSE)
  names(x) <- gsub(".", sub, names(x), fixed = TRUE)
  return(x)
}
getCols <- function(x) {
  cols <- x |>
    unique() |>
    stringr::str_split(pattern = " &&& ") |>
    unlist() |>
    unique()
  cols <- cols[!is.na(cols)]
  return(cols)
}
