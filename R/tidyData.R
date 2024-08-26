
#' Get a tidy tibble from a `summarised_result` object.
#'
#' @param result A `summarised_result` object.
#' @param showSettings Whether to show settings columns.
#' @param showGroupping Whether to show groupping columns.
#' @param prefixSet Prefix for settings columns.
#' @param prefixGroup Prefix for groupping columns.
#' @param pivotEstimates Whether to pivot estimates.
#'
#' @return A tibble.
#' @export
#'
tidyData <- function(result,
                     showSettings = TRUE,
                     showGroupping = TRUE,
                     prefixSet = "",
                     prefixGroup = "",
                     pivotEstimates = FALSE) {
  # initial checks
  omopgenerics::assertClass(result, "summarised_result")
  omopgenerics::assertLogical(showSettings, length = 1)
  omopgenerics::assertLogical(showGroupping, length = 1)
  omopgenerics::assertCharacter(prefixSet, length = 1, null = TRUE)
  omopgenerics::assertCharacter(prefixGroup, length = 1, null = TRUE)
  omopgenerics::assertLogical(pivotEstimates, length = 1)

  # add settings
  if (showSettings) {
    set <- omopgenerics::settings(result) |>
      dplyr::select(!dplyr::any_of(c(
        "result_type", "package_name", "package_version", "min_cell_count"))) |>
      dplyr::filter(.data$result_id %in% unique(result$result_id))
    for (col in colnames(set)) {
      if (all(is.na(set[[col]]))) {
        set <- set |>
          dplyr::select(!dplyr::all_of(col))
      }
    }
    if (ncol(set) > 1) {
      set <- set |>
        dplyr::rename_with(~ paste0(prefixSet, .x), !"result_id")
    }
    result <- set |>
      dplyr::inner_join(result, by = "result_id") |>
      dplyr::select(!"result_id")
  }

  # groupping
  if (showGroupping) {
    cols <- c(
      "cdm_name",
      visOmopResults::groupColumns(result),
      visOmopResults::strataColumns(result),
      visOmopResults::additionalColumns(result)
    )
    result <- result |>
      visOmopResults::splitAll() |>
      dplyr::relocate(dplyr::all_of(cols), .before = "variable_name") |>
      dplyr::rename_with(~ paste0(prefixGroup, .x), dplyr::all_of(cols))
  } else {
    result <- result |>
      dplyr::select(!c(
        "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
        "additional_name", "additional_level"))
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
