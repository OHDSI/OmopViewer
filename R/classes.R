
newOmopViewerPanel <- function(x) {
  omopgenerics::assertList(x)
  class(x) <- "omopviewer_panel"
  x
}

#' @export
print.omopviewer_panel <- function(x, ...) {
  tit <- getTitle(x)
  icon <- getIcon(x)
  data <- getData(x)
  filters <- getFilters(x)
  content <- getContent(x)
  cli::cli_inform(c(
    "{.pkg {tit}} (OmopViewer panel)",
    "*" = "{.strong icon:} {icon}",
    "*" = "{.strong data:} {data}",
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
getData <- function(x) {
  x <- x$data
  data <- character()
  if (length(x$result_type) > 0) {
    data <- c(data, paste0("result_type: <", paste0(x$result_type, collapse = ">, <"), ">"))
  }
  if (length(x$result_id) > 0) {
    data <- c(data, paste0("result_id: ", paste0(x$result_id, collapse = ", ")))
  }
  if (length(data) == 0) {
    data <- paste0("-no data-")
  } else {
    data <- paste0(data, collapse = "; ")
  }
  return(data)
}
getFilters <- function(x) {
  fi <- length(x$filters)
  af <- length(x$automatic_filters)
  if (af > 0) {
    if (fi > 0) {
      glue::glue("{fi} filters + {af} automatic filters")
    } else {
      glue::glue("{af} automatic filters")
    }
  } else {
    if (fi > 0) {
      glue::glue("{fi} filters")
    } else {
      "-no filters-"
    }
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
