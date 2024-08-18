
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
  x <- result[[resultType]]
  inputs <- names(input)

  # filter settings
  set <- attr(x, "settings")
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
    set <- set |>
      dplyr::rename_with(~ paste0(prefixSet, .x), !"result_id")
    colsSet <- set |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(!"result_id") |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::pull("name") |>
      unique()
    x <- x |>
      dplyr::inner_join(
        set |>
          dplyr::select(dplyr::all_of(c("result_id", colsSet))),
        by = "result_id"
      ) |>
      dplyr::relocate(dplyr::all_of(colsSet), .after = "result_id") |>
      dplyr::select(-"result_id")
  } else {
    x <- x |>
      dplyr::filter(.data$result_id %in% set$result_id)
  }

  # filter groupping
  group <- attr(x, "groupping")
  groupPrefix <- paste0(resultType, "_groupping_")
  toFilter <- inputs[startsWith(inputs, groupPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(groupPrefix)+1, nchar(fil))
    if (nm %in% colnames(group)) {
      group <- group |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }
  if (isTRUE(showGroupping)) {
    group <- group |>
      dplyr::rename_with(~ paste0(prefixGroup, .x), !"group_id")
    colsGroup <- colnames(group)[colnames(group) != "group_id"]
    x <- x |>
      dplyr::inner_join(group, by = "group_id") |>
      dplyr::relocate(dplyr::all_of(colsGroup), .after = "group_id") |>
      dplyr::select(-"group_id")
  } else {
    x <- x |>
      dplyr::filter(.data$group_id %in% group$group_id)
  }

  # filter variables and estimates
  varPrefix <- paste0(resultType, "_variables_and_estimates_")
  toFilter <- inputs[startsWith(inputs, varPrefix)]
  for (fil in toFilter) {
    nm <- substr(fil, nchar(varPrefix)+1, nchar(fil))
    if (nm %in% c("variable_name", "estimate_name")) {
      x <- x |>
        dplyr::filter(.data[[nm]] %in% input[[fil]])
    }
  }

  if (isTRUE(pivotEstimates)) {
    x <- x |>
      pivotEstimatesTemp()
  }

  return(x)
}

#' Prepare the results for the shiny
#'
#' @description
#' Split the data by result_type, so it is easier to use inside the shiny app.
#'
#' @param result A summarised_result object.
#'
#' @return A list of summarised_result split by result_type.
#' @export
#'
prepareData <- function(result) {
  result <- omopgenerics::validateResultArguemnt(result)
  set <- omopgenerics::settings(result)
  resultType <- set$result_type |> unique()
  colsGroup <- c("cdm_name", "group_name", "group_level", "strata_name",
                 "strata_level", "additional_name", "additional_level")
  x <- list()
  for (rt in resultType) {
    sety <- set |>
      dplyr::filter(.data$result_type == .env$rt)
    y <- result |>
      dplyr::filter(.data$result_id %in% sety$result_id) |>
      dplyr::as_tibble()
    groupy <- y |>
      dplyr::select(dplyr::all_of(colsGroup)) |>
      dplyr::distinct() |>
      dplyr::mutate("group_id" = dplyr::row_number()) |>
      dplyr::relocate("group_id")
    y <- y |>
      dplyr::inner_join(groupy, by = colsGroup) |>
      dplyr::select(!dplyr::all_of(colsGroup)) |>
      dplyr::relocate("result_id", "group_id") |>
      dplyr::arrange(.data$result_id, .data$group_id)
    groupy <- groupy |>
      visOmopResults::splitAll()
    attr(y, "settings") <- sety
    attr(y, "groupping") <- groupy
    x[[rt]] <- y
  }
  return(x)
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
