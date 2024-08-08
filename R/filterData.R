
#' This function is used to filter data in shinys that inputs are defined
#' following.
#'
#' @param result A summarised_result object.
#' @param resultType A resultType of interest to filter by.
#' @param input Input of the shiny to filter by.
#'
#' @return The filtered result.
#' @export
#'
filterData <- function(result, resultType, input) {
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
  x <- x |>
    dplyr::filter(.data$result_id %in% set$result_id)

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
  x <- x |>
    dplyr::filter(.data$group_id %in% group$group_id)

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
      dplyr::filter(.data$result_id %in% set$result_id) |>
      dplyr::as_tibble()
    groupy <- y |>
      dplyr::select(dplyr::all_of(colsGroup)) |>
      dplyr::distinct() |>
      dplyr::mutate("group_id" = dplyr::row_number()) |>
      dplyr::relocate("group_id")
    y <- y |>
      dplyr::inner_join(groupy, by = colsGroup)
    groupy <- groupy |>
      visOmopResults::splitAll()
    attr(y, "settings") <- sety
    attr(y, "groupping") <- groupy
    x[[rt]] <- y
  }
  return(x)
}
