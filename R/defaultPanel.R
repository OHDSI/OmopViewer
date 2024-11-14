
panelDetailsFromResult <- function(result) {
  set <- omopgenerics::settings(result)
  panelDetailsFromSet(set)
}
panelDetailsFromSet <- function(set) {
  set$result_type |>
    unique() |>
    rlang::set_names() |>
    purrr::map(\(x) panelDetailsFromResultType(resultType = x, set = set))
}
panelDetailsFromResultType <- function(resultType, set) {
  id <- omopViewerTabs$result_type == resultType
  tabId <- omopViewerTabs$result_tab_id[id]
  outputId <- omopViewerOutput$output_id[omopViewerOutput$result_tab_id == tabId]
  icon <- omopViewerTabs$icon[id]
  title <- omopViewerTabs$title[id]
  if (length(outputId) == 0) outputId <- 0L
  if (length(title) == 0) title <- formatTit(resultType)
  list(
    result_type = resultType,
    result_id = set$result_id[set$result_type == resultType],
    output_id = outputId,
    icon = icon,
    title = title,
    information = omopViewerTabs$information[id]
  )
}
panelStructureFromResult <- function(result) {
  as.list(omopgenerics::settings(result)$result_type)
}
completePanelDetails <- function(panelDetails, result) {
  set <- omopgenerics::settings(result)
  panelDetails <- panelDetails |>
    purrr::imap(\(x, nm) {
      if (!is.list(x)) {
        cli::cli_inform("Element {.var {nm}} eliminated from {.pkg panelDetails} as it is not a list.")
        return(NULL)
      } else if (!any(c("result_type", "result_id") %in% names(x))) {
        cli::cli_inform("Element {.var {nm}} eliminated from {.pkg panelDetails}, {.var result_type} or {.var result_id} must be provided.")
        return(NULL)
      } else {
        if (!"result_type" %in% names(x)) {
          x$result_type <- unique(set$result_type[set$result_id == x$result_id])
        }
        def <- panelDetailsFromResultType(resultType = x$result_type, set = set)
        cols <- c("result_id", "output_id", "icon", "title", "information")
        for (col in cols) {
          if (!col %in% names(x)) x[[col]] <- def[[col]]
        }
        x <- x[c("result_type", "result_id", "output_id", "icon", "title", "information")]
        return(x)
      }
    }) |>
    purrr::discard(is.null) |>
    # correct types
    purrr::map(\(x) {
      x$result_type <- as.character(x$result_type)
      x$result_id <- as.integer(x$result_id)
      x$output_id <- as.integer(x$output_id)
      x$icon <- as.character(x$icon)
      x$title <- as.character(x$title)
      x$information <- as.character(x$information)
      return(x)
    })

  # add elements that may be missing
  resId <- panelDetails |>
    purrr::map(\(x) x$result_id) |>
    unlist() |>
    unique()
  set <- omopgenerics::settings(result) |>
    dplyr::filter(!.data$result_id %in% .env$resId)
  if (nrow(set) > 0) {
    extra <- panelDetailsFromSet(set = set)
    cli::cli_inform("{.var {names(extra)}} added to {.pkg panelDetails}, if you do not want them in your shiny please remove from results.")
    panelDetails <- c(panelDetails, extra)
  }

  return(panelDetails)
}
