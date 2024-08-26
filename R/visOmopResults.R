
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
  if (is.null(header)) header <- character()
  if (is.null(group)) group <- character()
  if (is.null(hide)) hide <- character()
  result <- result |>
    visOmopResults::splitAll()
  omopgenerics::assertCharacter(header)
  omopgenerics::assertCharacter(group)
  omopgenerics::assertCharacter(hide)
  omopgenerics::assertCharacter(c(header, group, hide), unique = TRUE)
  omopgenerics::assertTable(
    result, columns = c("estimate_name", "estimate_type", header, group, hide))

  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "medain [Q25 - Q75]" = "<median> [<q25> - <q75>]",
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
    dplyr::select(!dplyr::all_of(c("estimate_type", hide))) |>
    visOmopResults::formatHeader(header = header)
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

# warning if default plot tab
