createButton <- function(x, defaults, prefix) {
  arg <- unique(x$argument)
  args <- list(
    "inputId" = paste0('"', prefix, "_", arg, '"'),
    "label" = paste0('"', arg, '"')
  )
  type <- x$value[x$name == "type"]
  def <- x$value[x$name == "default"]
  if (length(def) == 0) {
    def <- tryCatch(rlang::eval_tidy(defaults[[arg]]), error = function(e) NULL)
    if (!lifecycle::is_present(def)) def <- NULL
  }
  def <- cast(def)
  if (type == "selector") {
    multiple <- x$value[x$name == "multiple"]
    choices <- x$value[x$name == "options"] |>
      stringr::str_split_1(", ") |>
      cast()
    args <- c(args, list(selected = def, multiple = multiple, choices = choices))
  } else if (type == "check") {
    args <- c(args, list(value = def))
  }
  button(type, args)
}
button <- function(type, args) {
  x <- purrr::imap(args, \(x, nm) paste0(nm, " = ", x))
  if (type == "selector") {
    x <- c(x, 'options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")')
  }
  fun <- switch (
    type,
    "selector" = 'shinyWidgets::pickerInput(',
    "check" = 'shiny::checkboxInput('
  )
  paste0(c(fun, paste0(x, collapse = ",\n"), ")"), collapse = "\n")
}
selector <- function(id, lab, cho, sel, mult) {
  'shinyWidgets::pickerInput(
    inputId = "{id}",
    label = "{lab}",
    choices = {cho},
    selected = {sel},
    multiple = {mult},
    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  )' |>
    glue::glue() |>
    as.character()
}
