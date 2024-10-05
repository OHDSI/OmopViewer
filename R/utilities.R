validateSummary <- function(summary, result){
  if (summary) {
    sum <- utils::capture.output(summary(result), type = "message")
  } else {
    sum <- NULL
  }
  return(sum)
}

validateDirectory <- function(directory) {
  # create directory if it does not exit
  if (!dir.exists(directory)) {
    cli::cli_inform(c("i" = "Provided directory does not exist, it will be created."))
    dir.create(path = directory, recursive = TRUE)
    cli::cli_inform(c("v" = "directory created: {.pkg {directory}}"))

  } else if (file.exists(file.path(directory, "shiny"))) {
    # ask overwrite shiny
    overwrite <- "1"  # overwrite if non-interactive
    if (rlang::is_interactive()) {
      cli::cli_inform(c(
        "!" = "A {.strong shiny} folder already exists in the provided directory. Enter choice 1 or 2:",
        " " = "1) Overwrite",
        " " = "2) Cancel"
      ))
      overwrite <- readline()
      while (!overwrite %in% c("1", "2")) {
        cli::cli_inform(c("x" = "Invalid input. Please choose 1 to overwrite or 2 to cancel:"))
        overwrite <- readline()
      }
    }
    if (overwrite == "2") {
      return(TRUE)
    } else {
      cli::cli_inform(c("i" = "{.strong shiny} folder will be overwritten."))
    }
  }
  return(directory)
}

validatePanels <- function(panels, choices) {
  omopgenerics::assertList(panels)
  # clean list elements
  panelsClean <- purrr::map(panels, function(x) {
    x[x %in% names(choices)]
  }) |>
    purrr::compact()
  panelOrder <- purrr::flatten_chr(panelsClean)
  # check duplicates
  if (length(unique(panelOrder)) != length(panelOrder)){
    cli::cli_abort("`panels` cannot have duplicate results")
  }
  out <- setdiff(purrr::flatten_chr(panels), panelOrder)
  if (length(out) > 0) {
    cli::cli_warn("{.strong {out}} in `panels` not found in results")
  }
  # if not specified, append remaining results
  panelsClean <- c(panelsClean, as.list(setdiff(names(choices), panelOrder)))
  # add names
  if (length(panelsClean) > 0) {
    if (is.null(names(panelsClean))) {
      names(panelsClean) <- paste0("id_", 1:length(panelsClean))
    } else {
      names(panelsClean)[names(panelsClean) == ""] <- paste0("id_", 1:sum(names(panelsClean) == ""))
    }
  }
  # get choices
  choices <- purrr::map(panelsClean, function(x, y = choices) {y[x]})
  resultOrder <- panelsClean |> unlist() |> unname()
  return(list(choices = choices, result_order = resultOrder))
}
