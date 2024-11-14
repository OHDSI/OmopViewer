
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
      unlink(file.path(directory, "shiny"), recursive = TRUE)
      cli::cli_inform(c("v" = "Prior {.strong shiny} folder deleted."))
    }
  }
  return(directory)
}
validateBackground <- function(background, logo, call = parent.frame()) {
  msg <- "'background' must be either TRUE/FALSE or a path to an existing `.md` file."
  if (is.logical(background)) {
    omopgenerics::assertLogical(background, length = 1, call = call, msg = msg)
    if (background) {
      background <- defaultBackground(logo = logo)
    } else {
      background <- NULL
    }
  } else if (is.character(background)) {
    omopgenerics::assertCharacter(background, length = 1, call = call, msg = msg)
    if (file.exists(background)) {
      background <- readLines(background)
    } else {
      cli::cli_abort(message = "background file ({.path {background}}) does not exist.", call = call)
    }
  } else {
    cli::cli_abort(message = msg, call = call)
  }
  return(background)
}
validatePanelDetails <- function(panelDetails, result, call = parent.frame()) {
  if (length(panelDetails) == 0) {
    panelDetails <- panelDetailsFromResult(result)
  } else {
    omopgenerics::assertList(panelDetails, named = TRUE, call = call)
    panelDetails <- completePanelDetails(panelDetails, result)
  }
  return(panelDetails)
}
validatePanelStructure <- function(panelStructure, panelDetails, result, call = parent.frame()) {
  if (length(panelStructure) == 0) {
    panelStructure <- as.list(names(panelDetails))
  } else {
    omopgenerics::assertList(panelStructure, call = call)
    panelStructure <- purrr::map(panelStructure, as.character)
    present <- unlist(panelStructure)

    if (length(present) != length(unique(present))) {
      cli::cli_abort("panel identifiers in {.var panelStructure} must be unique.", call = call)
    }

    all <- names(panelDetails)

    # warn eliminated
    eliminate <- present[!present %in% all]
    if (length(eliminate) > 0) {
      cli::cli_warn("{.var {eliminate}} removed from panelStucture as not present in data.")
      panelStructure <- panelStructure |>
        purrr::map(\(x) x[x %in% all]) |>
        purrr::discard(\(x) length(x) == 0)
    }

    # inform missing
    missing <- all[!all %in% present]
    if (length(missing) > 0) {
      cli::cli_inform("{.var {missing}} panels added to panelStucture.")
      panelStructure <- c(panelStructure, as.list(missing))
    }
  }
  return(panelStructure)
}
