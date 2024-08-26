
#' This function is used to filter data in shinys that inputs are defined
#' following.
#'
#' @param result A summarised_result object.
#' @param resultType A resultType of interest to filter by.
#' @param input Input of the shiny to filter by.
#' @param prefixSet Prefix for setting columns.
#' @param prefixGroup Prefix for groupping columns.
#' @param showSettings Whether to show settings columns.
#' @param showGroupping Whether to show groupping columns.
#' @param pivotEstimates Whether to pivot estimates.
#'
#' @return The filtered result.
#' @export
#'
filterData <- function(result,
                       resultType,
                       input,
                       prefixSet = "",
                       prefixGroup = "",
                       showSettings = TRUE,
                       showGroupping = TRUE,
                       pivotEstimates = FALSE) {
  result <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType)
  if (length(input) == 0) {
    inputs <- character()
  } else {
    inputs <- names(input)
  }

  # filter settings
  set <- omopgenerics::settings(result)
  setPrefix <- paste0(resultType, "_settings_")
  toFilter <- inputs[startsWith(inputs, setPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(setPrefix)+1, nchar(fil))
    if (nm %in% colnames(set)) {
      set <- set |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }

  if (isTRUE(showSettings)) {
    colsSet <- set |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(!"result_id") |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::pull("name") |>
      unique()
    colsSet <- colsSet[!colsSet %in% c("result_type", "package_name", "package_version")]
    result <- set |>
      dplyr::select(dplyr::all_of(c("result_id", colsSet))) |>
      dplyr::rename_with(~ paste0(prefixSet, .x), !"result_id") |>
      dplyr::inner_join(result, by = "result_id") |>
      dplyr::select(-"result_id")
  } else {
    result <- result |>
      dplyr::filter(.data$result_id %in% set$result_id)
  }

  groupCols <- c(
    "cdm_name",
    visOmopResults::groupColumns(result),
    visOmopResults::strataColumns(result),
    visOmopResults::additionalColumns(result)
  )
  result <- result |>
    visOmopResults::splitAll()

  # filter groupping
  groupPrefix <- paste0(resultType, "_groupping_")
  toFilter <- inputs[startsWith(inputs, groupPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(groupPrefix)+1, nchar(fil))
    if (nm %in% colnames(result)) {
      result <- result |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }
  if (isTRUE(showGroupping)) {
    result <- result |>
      dplyr::relocate(dplyr::all_of(groupCols), .before = "variable_name") |>
      dplyr::rename_with(~ paste0(prefixGroup, .x), dplyr::all_of(groupCols))
  } else {
    result <- result |>
      dplyr::select(!dplyr::all_of(groupCols))
  }

  # filter variables and estimates
  varPrefix <- paste0(resultType, "_variables_and_estimates_")
  toFilter <- inputs[startsWith(inputs, varPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(varPrefix)+1, nchar(fil))
    if (nm %in% c("variable_name", "estimate_name")) {
      result <- result |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }

  if (isTRUE(pivotEstimates)) {
    result <- result |>
      pivotEstimatesTemp()
  }

  return(result)
}

pivotEstimatesTemp <- function(result) {
  nameStyle <- "{estimate_name}"
  types <- result |>
    dplyr::select("estimate_type", "estimate_name") |>
    dplyr::distinct() |>
    dplyr::mutate("estimate_type" = dplyr::case_when(
      grepl("percentage|proportion", .data$estimate_type) ~ "numeric",
      "date" == .data$estimate_type ~ "Date",
      .default = .data$estimate_type
    )) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$estimate_name) |>
    dplyr::mutate("n" = dplyr::n()) |>
    dplyr::ungroup()
  notFormatted <- types |>
    dplyr::filter(.data$n > 1) |>
    dplyr::select("estimate_name") |>
    dplyr::distinct() |>
    dplyr::pull("estimate_name")
  if (length(notFormatted) > 0) {
    cli::cli_inform("The following estimates appear as character as multiple types are displayed: {notFormatted}.")
  }
  fq <- types |>
    dplyr::filter(.data$n == 1)
  fq <- paste0("as.", fq$estimate_type, "(.data$", fq$estimate_name, ")") |>
    rlang::parse_exprs() |>
    rlang::set_names(fq$estimate_name)
  result |>
    dplyr::select(-"estimate_type") |>
    tidyr::pivot_wider(
      names_from = "estimate_name", values_from = "estimate_value") |>
    dplyr::mutate(!!!fq)
}
