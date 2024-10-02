#' This function adds infromation on groupping variables to the settings
#' attribute of a summaried result
#'
#' @param x A `<summarised_result>` object.
#'
#' @export
#'
correctSettings <- function(x) {
  set <- omopgenerics::settings(x) |>
    dplyr::left_join(
      x |>
        dplyr::distinct(.data$result_id, .data$group_name, .data$strata_name, .data$additional_name) |>
        dplyr::group_by(.data$result_id) |>
        dplyr::summarise(
          dplyr::across(dplyr::ends_with("name"), ~ getUniqueName(.x))
        ) |>
        dplyr::rename_with(~gsub("_name", "", .x)),
      by = "result_id"
    )
  return(omopgenerics::newSummarisedResult(x = x, settings = set))
}

getUniqueName <- function(x) {
  uniqueNames <- x |> stringr::str_split(" &&& ") |> unlist()  |> unique()
  if (all(uniqueNames == "overall")) {
    uniqueNames <- NA
  } else {
    uniqueNames <- uniqueNames[!uniqueNames %in% "overall"] |> paste0(collapse = " &&& ")
  }
  return(uniqueNames)
}
