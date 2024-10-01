
#' Create a formatted gt table.
#'
#' @param result A tibble it must contain at least two columns: estimate_value
#' and estimate_type.
#' @param header Column names to move to header, must be columns in result.
#' @param group Column names to group by, must be columns in result.
#' @param hide Column names to hide, must be columns in result.
#'
#' @return A gt table.
#' @export
#'
visTable <- function(result,
                     header = character(),
                     group = character(),
                     hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()
  result <- result |>
    tidyData() |>
    dplyr::select(!dplyr::any_of(c(
      "package_name", "package_version", "result_type", "min_cell_count"
    )))
  omopgenerics::assertCharacter(header)
  omopgenerics::assertCharacter(group, null = TRUE)
  omopgenerics::assertCharacter(hide)
  omopgenerics::assertCharacter(c(header, group, hide), unique = TRUE)
  allCols <- c("estimate_name", "estimate_type", header, group, hide) |>
    unique()
  omopgenerics::assertTable(result, columns = allCols)

  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)) |>
    visOmopResults::formatEstimateName(estimateNameFormat = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- omopgenerics::uniqueId(exclude = colnames(result))
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::gtTable(groupColumn = group)
  return(result)
}
