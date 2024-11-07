
getChoicesValues <- function(result, panelDetails) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  panelDetails <- validatePanelDetails(panelDetails, result)

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
    choices <- unique(c(names(settings), names(grouping), names(variables))) |>
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

addFilterNames <- function(panelDetails, result) {
  set <- omopgenerics::settings(result)
  if (!all(c("group", "strata", "additional") %in% colnames(set))) {
    set <- result |>
      correctSettings() |>
      omopgenerics::settings()
  }
  panelDetails |>
    purrr::map(\(x) {
      resId <- x$result_id
      setx <- set |>
        dplyr::filter(.data$result_id %in% .env$resId)
      settingsCols <- colnames(set)
      settingsCols <- settingsCols[!settingsCols %in% ignoreSettings]
      settingsCols <- settingsCols[purrr::map_lgl(settingsCols, \(x) {
        sum(!is.na(setx[[x]])) > 0
      })]
      if (length(settingsCols) > 0) {
        settingsCols <- paste0("settings_", settingsCols)
      }
      groupingCols <- setx |>
        dplyr::select("group", "strata", "additional") |>
        as.list() |>
        purrr::map(\(x) {
          x <- x |>
            unique() |>
            stringr::str_split(pattern = " &&& ") |>
            unlist() |>
            unique()
          x[!is.na(x)]
        }) |>
        unlist() |>
        unique()
      if (length(groupingCols) > 0) {
        groupingCols <- paste0("grouping_", groupingCols)
      }
      x$filters <- c(
        settingsCols, groupingCols, "variable_name", "estimate_name"
      )
      return(x)
    })
}
getFilterValues <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(x) {
      resId <- x$result_id
      filtersSettings <- omopgenerics::settings(result) |>
        dplyr::filter(.data$result_id %in% .env$resId) |>
        dplyr::select(!dplyr::any_of(ignoreSettings)) |>
        as.list() |>
        purrr::map(unique) |>
        prefixNames(prefix = "settings_")
      res <- result |>
        dplyr::filter(.data$result_id %in% .env$resId)
      filtersGrouping <- res |>
        dplyr::select(c(
          "cdm_name", "group_name", "group_level", "strata_name",
          "strata_level", "additional_name", "additional_level"
        )) |>
        dplyr::distinct() |>
        visOmopResults::splitAll() |>
        as.list() |>
        purrr::map(unique) |>
        prefixNames(prefix = "grouping_")
      filtersVariablesEstimates <- res |>
        dplyr::select("variable_name", "estimate_name") |>
        as.list() |>
        purrr::map(unique)
      c(filtersSettings, filtersGrouping, filtersVariablesEstimates)
    })
}
prefixNames <- function(x, prefix) {
  if (length(x) == 0) return(list())
  names(x) <- paste0(prefix, names(x))
  return(x)
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
