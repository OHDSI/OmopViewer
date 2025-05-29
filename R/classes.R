
newOmopViewerPanel <- function(x) {
  omopgenerics::assertList(x)
  class(x) <- "omopviewer_panel"
  x
}

#' @export
print.omopviewer_panel <- function(x, ...) {
  # extract data
  tit <- getTitle(x)
  icon <- getIcon(x)
  data <- getData(x)
  filters <- getFilters(x)
  content <- getContent(x)

  # create message
  mes <- c(
    paste0("\033[34m", tit,"\033[0m (OmopViewer panel)\n"),
    "\u2022 ", paste0("\033[1m icon:\033[0m ", icon, "\n"),
    "\u2022 ", paste0("\033[1m data:\033[0m ", data, "\n"),
    "\u2022 ", paste0("\033[1m filters:\033[0m ", filters, "\n"),
    "\u2022 ", paste0("\033[1m content:\033[0m ", content, "\n")
  )

  # print
  cat(mes, sep = "")

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
        paste0("\033[1;34m", tit, "\033[0m (\033[3m", type, "\033[0m)")
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
