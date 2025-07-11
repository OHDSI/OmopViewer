writeButtons <- function(filters) {
  # cast input id
  filters <- purrr::map(filters, \(x) {
    x$inputId <- cast(x$inputId)
    x$input_id <- cast(x$input_id)
    x
  })

  # create bucket list button
  bucketList <- filters |>
    purrr::keep(\(x) x$button_type == "rank_list")
  if (length(bucketList) > 0) {
    # TODO consider how to add header to the button
    # TODO consider possibility to have multiple bucket lists
    bucketList <- list(
      bucket_list = list(
        button_type = "bucket_list",
        rank = bucketList
      )
    )
  }

  # create buttons
  c(bucketList, filters) |>
    purrr::keep(\(x) x$button_type != "rank_list") |>
    purrr::map(writeButton)
}
writeButton <- function(x) {
  # write the button
  if (x$button_type == "pickerInput") {
    x$label <- cast(x$label)
    x$choices <- cast(x$choices)
    x$selected <- cast(x$selected)
    x$options = 'list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")'
    if (rlang::is_installed("shinyWidgets")) {
      args <- names(formals(shinyWidgets::pickerInput))
      args <- args[args %in% names(x)]
    } else {
      x$button_type <- NULL
      x$input_id <- NULL
      args <- names(x)
    }
    button <- paste0(
      "shinyWidgets::pickerInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "checkbox") {
    x$label <- cast(x$label)
    if (rlang::is_installed("shiny")) {
      args <- names(formals(shiny::checkboxInput))
      args <- args[args %in% names(x)]
    } else {
      x$button_type <- NULL
      x$input_id <- NULL
      args <- names(x)
    }
    button <- paste0(
      "shiny::checkboxInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "numericInput") {
    x$label <- cast(x$label)
    if (rlang::is_installed("shiny")) {
      args <- names(formals(shiny::numericInput))
      args <- args[args %in% names(x)]
    } else {
      x$button_type <- NULL
      x$input_id <- NULL
      args <- names(x)
    }
    button <- paste0(
      "shiny::numericInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "bucket_list") {
    x$header <- cast(x$header)
    if (rlang::is_installed("sortable")) {
      args <- names(formals(sortable::bucket_list))
      args <- args[args %in% names(x)]
    } else {
      x$button_type <- NULL
      x$inputId <- NULL
      args <- names(x)
    }
    dots <- x$rank |>
      purrr::map_chr(\(rnk) {
        paste0(
          "sortable::add_rank_list(\ntext=", cast(rnk$text), ",\nlabels=",
          cast(rnk$labels), ",\ninput_id=", rnk$input_id, "\n)"
        )
      }) |>
      paste0(collapse = ",\n")
    button <- paste0(
      "sortable::bucket_list(\n",
      c(purrr::imap(x[args], \(x, nm) paste(nm, "=", x)), dots) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "materialSwitch") {
    x$label <- cast(x$label)
    if (rlang::is_installed("shinyWidgets")) {
      args <- names(formals(shinyWidgets::materialSwitch))
      args <- args[args %in% names(x)]
    } else {
      x$button_type <- NULL
      x$input_id <- NULL
      args <- names(x)
    }
    button <- paste0(
      "shinyWidgets::materialSwitch(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "Toggle.shinyInput") {
    x$input_id <- NULL
    x$button_type <- NULL
    x$label <- cast(x$label)
    x$onText <- cast(x$onText)
    x$offText <- cast(x$offText)
    button <- paste0(
      "shiny.fluent::Toggle.shinyInput(\n",
      purrr::imap(x, \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else {
    cli::cli_abort("unexpected button_type")
  }

  return(button)
}
