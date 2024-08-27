
#' Bind a list of data.frames into a single summarised_result object.
#'
#' @param data A data.frame or list of data.frames.
#'
#' @return A summarised_result object.
#' @export
#'
bindData <- function(data) {
  # initial check
  if (!rlang::is_bare_list(data)) data <- list(data)
  omopgenerics::assertList(data, class = "data.frame")

  empty <- emptySummarisedResult()
  if (length(data) == 0) return(empty)

  # validate summarised_result objects
  for (k in seq_along(data)) {
    nm <- names(data)[k]
    if (is.null(nm) || nm == "") nm <- paste("element", k)
    tryCatch(
      expr = {
        if (!inherits(data[[k]], "summarised_result")) {
          data[[k]] <- omopgenerics::newSummarisedResult(data[[k]])
        }
      },
      error = function(e) {
        cli::cli_inform(c(
          "!" = "{.pkg {nm}} not included in results due to:", as.character(e)))
        data[[k]] <- empty
      }
    )
  }

  data <- tryCatch(
    omopgenerics::bind(data),
    error = function(e) {
      cli::cli_inform(c(
        "!" = "Returning empty result; results could not be binded:", as.character(e)))
      return(empty)
    }
  )

  return(data)
}

#' Split a summarised_result object by a set of setting columns.
#'
#' @param data A summarised_result object.
#' @param columns Set of columns to split the summarised_result object by.
#' @param collapse Character to collapse names.
#'
#' @return A list of summarised_result objects splitted by `columns`.
#' @export
#'
splitBySettings <- function(data, columns, collapse = " & ") {
  omopgenerics::assertClass(data, "summarised_result")
  omopgenerics::assertCharacter(columns)
  omopgenerics::assertCharacter(collapse, length = 1)

  sets <- colnames(omopgenerics::settings(data))
  notPresent <- columns[!columns %in% sets]
  if (length(notPresent) > 0) {
    cli::cli_abort("columns not present in settings: {.var {notPresent}}.")
  }

  # split by selected columns
  data <- data |>
    visOmopResults::addSettings(columns = columns) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(columns))) |>
    dplyr::group_split() |>
    as.list() |>
    lapply(function(xx) {
      nm <- xx |>
        dplyr::select(dplyr::all_of(columns)) |>
        dplyr::first() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(dplyr::everything()) |>
        dplyr::pull("value") |>
        paste0(collapse = collapse)
      xx |>
        dplyr::select(!dplyr::all_of(columns)) |>
        list() |>
        rlang::set_names(nm)
    }) |>
    unlist(recursive = FALSE)

  return(data)
}
