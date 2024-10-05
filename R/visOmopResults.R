
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

  if (nrow(result) == 0) return(gt::gt(dplyr::tibble()))

  result <- result |>
    tidyData() |>
    dplyr::select(!dplyr::any_of(c(
      "package_name", "package_version", "result_type", "min_cell_count",
      "strata", "group", "additional"
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
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}

# THIS IS A REPLACEMENT FOR TIDY METHOD DEFINED IN VISOMOPRESULTS TAKES INTO
# ACCOUNT GROUP, STRATA, AND ADDITIONAL COLUMNS AND THEY ARE GENERATED IN
# OMOPGENERICS.

#' Get a tidy tibble from a `summarised_result` object.
#'
#' @param result A `summarised_result` object.
#'
#' @return A tibble.
#' @export
#'
tidyData <- function(result) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)

  # correct settings if it has not been done before
  sets <- omopgenerics::settings(result)
  if (!all(c("group", "strata", "additional") %in% colnames(sets))) {
    sets <- result |>
      correctSettings() |>
      omopgenerics::settings()
  }
  sets <- removeSettingsNa(sets)
  attr(result, "settings") <- sets

  # get grouping columns
  groupingCols <- c(
    getCols(sets$group), getCols(sets$strata), getCols(sets$additional))

  # add settings and grouping
  result <- result |>
    visOmopResults::addSettings() |>
    visOmopResults::splitAll()

  # add missing grouping
  notPresent <- groupingCols[!groupingCols %in% colnames(result)]
  if (length(notPresent) > 0) {
    for (col in notPresent) {
      result <- result |>
        dplyr::mutate(!!col := "overall")
    }
  }

  # grouping will be located before variable
  result <- result |>
    dplyr::relocate(dplyr::all_of(groupingCols), .before = "variable_name") |>
    dplyr::select(!"result_id")

  return(result)
}

removeSettingsNa <- function(x) {
  cols <- x |>
    purrr::map(unique)
  cols <- names(cols)[is.na(cols)]
  x |>
    dplyr::select(!dplyr::all_of(cols))
}
