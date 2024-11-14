
#' Extract filter values.
#'
#' @param result A summarised_result object.
#' @param panelDetails A list with the panel details.
#'
#' @return A lit with the values to filter by.
#' @export
#'
filterValues <- function(result,
                         panelDetails = NULL) {
  # check inputs
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertList(panelDetails, null = TRUE)
  panelDetails <- validatePanelDetails(panelDetails, result) |>
    addFilterNames(result = result)

  getFilterValues(panelDetails = panelDetails, result = result)
}

addFilterNames <- function(panelDetails, result) {
  set <- omopgenerics::settings(result)
  panelDetails |>
    purrr::map(\(x) {
      resId <- x$result_id
      setx <- set |>
        dplyr::filter(.data$result_id %in% .env$resId)
      settingsCols <- colnames(setx)
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
          x[!is.na(x) & x != ""]
        }) |>
        unlist() |>
        unique()
      groupingCols <- c("cdm_name", groupingCols)
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
    purrr::imap(\(x, nm) {
      resId <- x$result_id
      filtersSettings <- omopgenerics::settings(result) |>
        dplyr::filter(.data$result_id %in% .env$resId) |>
        dplyr::select(!dplyr::any_of(ignoreSettings)) |>
        as.list() |>
        purrr::map(\(x) {
          x <- unique(x)
          x[!is.na(x)]
        }) |>
        purrr::discard(\(x) length(x) == 0) |>
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
      res <- c(filtersSettings, filtersGrouping, filtersVariablesEstimates) |>
        purrr::map(as.character)
      res <- res |>
        rlang::set_names(nm = paste0(nm, "_", names(res)))
      tidyColumns <- x$filters
      id <- startsWith(tidyColumns, "settings_") | startsWith(tidyColumns, "grouping_")
      tidyColumns[id] <- substr(tidyColumns[id], 10, nchar(tidyColumns[id]))
      res[[paste0(nm, "_tidy_columns")]] <- tidyColumns
      return(res)
    }) |>
    purrr::flatten()
}
prefixNames <- function(x, prefix) {
  if (length(x) == 0) return(list())
  names(x) <- paste0(prefix, names(x))
  return(x)
}
