createButton <- function(x) {
  # write the button
  if (x$button_type == "pickerInput") {
    x$options = 'list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")'
    args <- names(formals(shinyWidgets::pickerInput))
    args <- args[args %in% names(x)]
    if (identical(x$selected, "selected$")) {
      x$selected <- paste0("selected$", x$inputId)
    }
    if (identical(x$choices, "choices$")) {
      x$choices <- paste0("choices$", x$inputId)
    }
    button <- paste0(
      "shinyWidgets::pickerInput(\n",
      purrr::imap(x[args], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "checkbox") {
    args <- names(formals(shiny::checkboxInput))
    button <- paste0(
      "shiny::checkboxInput(\n",
      purrr::imap(x[names(args)], \(x, nm) paste(nm, "=", x)) |>
        paste0(collapse = ",\n"),
      "\n)"
    )
  } else if (x$button_type == "") {

  }

  return(button)
}
