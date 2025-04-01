createButton <- function(x) {
  # write the button
  if (x$button_type == "pickerInput") {
    x$label <- cast(x$label)
    x$choices <- cast(x$choices)
    x$selected <- cast(x$selected)
    x$options = 'list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")'
    args <- names(formals(shinyWidgets::pickerInput))
    args <- args[args %in% names(x)]
    button <- paste0(
      "shinyWidgets::pickerInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "checkbox") {
    x$label <- cast(x$label)
    args <- names(formals(shiny::checkboxInput))
    args <- args[args %in% names(x)]
    button <- paste0(
      "shiny::checkboxInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "numericInput") {
    x$label <- cast(x$label)
    args <- names(formals(shiny::numericInput))
    args <- args[args %in% names(x)]
    button <- paste0(
      "shiny::numericInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else {
    cli::cli_abort("unexpected button_type")
  }

  return(button)
}
