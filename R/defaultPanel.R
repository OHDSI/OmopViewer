
panelDetailsFromResult <- function(result) {
  omopgenerics::settings(result) |>
    dplyr::group_by(.data$result_type) |>
    dplyr::summarise(result_id = paste0(.data$result_id, collapse = "; ")) |>
    dplyr::left_join(
      omopViewerTabs |>
        dplyr::left_join(
          omopViewerOutput |>
            dplyr::group_by(.data$result_tab_id) |>
            dplyr::summarise(output_id = paste0(.data$output_id, collapse = "; ")),
          by = "result_tab_id"
        ) |>
        dplyr::select("result_type", "title", "icon", "output_id"),
      by = "result_type"
    ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      title = dplyr::coalesce(.data$title, formatTit(.data$result_type)),
      output_id = dplyr::coalesce(.data$output_id, "0")
    ) |>
    dplyr::select("id", "result_id", "title", "icon", "output_id")
}
panelStructureFromResult <- function(result) {
  x <- omopgenerics::settings(result) |>
    dplyr::select("result_type") |>
    dplyr::distinct() |>
    dplyr::left_join(
      omopViewerTabs |>
        dplyr::select("result_type", "title"),
      by = "result_type"
    ) |>
    dplyr::mutate(name = dplyr::if_else(
      is.na(.data$title), formatTit(.data$result_type), .data$title
    ))
  x$result_type |>
    rlang::set_names(x$name) |>
    as.list()
}
