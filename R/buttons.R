createButton <- function(x) {
  # write the button
  if (x$button_type == "pickerInput") {
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
