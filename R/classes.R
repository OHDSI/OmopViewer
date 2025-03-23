
#' @export
newOmopViewerPanel <- function(x) {
  omopgenerics::assertList(x)
  class(x) <- "omopviewer_panel"
  x
}

#' @export
print.omopviewer_panel <- function(x, ...) {
  tit <- getTitle(x)
  icon <- getIcon(x)
  resultId <- getResultId(x)
  filters <- getFilters(x)
  content <- getContent(x)
  cli::cli_inform(c(
    "{.pkg {tit}} (OmopViewer panel)",
    "*" = "{.strong icon:} {icon}",
    "*" = "{.strong result ids:} {resultId}",
    "*" = "{.strong filters:} {filters}",
    "*" = paste0("{.strong content:} ", content)
  ))
  invisible(x)
}

getTitle <- function(x) {
  x$title %||% "-"
}
getIcon <- function(x) {
  x$icon %||% "-"
}
getResultId <- function(x) {
  ri <- x$result_id
  if (length(ri) == 0) {
    paste0("-no result_id-")
  } else {
    paste0(ri, collapse = ", ")
  }
}
getFilters <- function(x) {
  fi <- x$filters
  if (length(fi) == 0) {
    "-no filters-"
  } else {
    paste0(length(fi), " filters")
  }
}
getContent <- function(x) {
  co <- x$content
  if (length(co) == 0) {
    "-no content panels-"
  } else {
    co |>
      purrr::map_chr(\(x) {
        tit <- getTitle(x)
        type <- getType(x)
        paste0("{.pkg ", tit, "} ({.emph ", type, "})")
      }) |>
      paste0(collapse = "; ")
  }
}
getType <- function(x) {
  ot <- x$output_type
  if (length(ot) == 1) {
    ot
  } else {
    "-"
  }
}
