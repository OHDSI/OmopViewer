
getChoices <- function(result) {
  settings <- getPossibleSettings(result)
  groupping <-getPossibleGroupping(result)
  variables <- getPossibleVariables(result)
  choices <- names(variables) |>
    purrr::set_names() |>
    purrr::map(\(x) list(
      settings = settings[[x]],
      groupping = groupping[[x]],
      variable_name = variables[[x]]$variable_name,
      estimate_name = variables[[x]]$estimate_name
    ))
  return(choices)
}
getPossibleSettings <- function(result) {
  omopgenerics::settings(result) |>
    dplyr::select(!dplyr::any_of(c(
      "result_id", "package_name", "package_version", "min_cell_count"))) |>
    getPossibilities()
}
getPossibleGroupping <- function(result) {
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
