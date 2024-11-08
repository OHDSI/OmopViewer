
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
      cli::cli_inform(c("i" = "{.strong shiny} folder will be overwritten. Prior shiny folder deleted."))
      unlink(file.path(directory, "shiny"), recursive = TRUE)
    }
  }
  return(directory)
}
validatePanels <- function(panels, resultTypes, call = parent.frame()) {
  omopgenerics::assertList(panels, call = call)

  # elements must be characters
  if (length(panels) > 0 & !is.character(unlist(panels))) {
    cli::cli_abort("{.var panels} should be a character list.", call = call)
  }

  # elements must be unique TO CONSIDER REMOVING
  allPanels <- purrr::flatten_chr(panels)
  uniquePanels <- unique(allPanels)
  if (length(allPanels) != length(uniquePanels)) {
    cli::cli_abort("{.var panels} can not have repeated elements.", call = call)
  }

  # eliminate not present panels
  panels <- panels |>
    purrr::map(\(x) purrr::keep(x, \(y) y %in% resultTypes)) |>
    purrr::compact()
  notPresent <- allPanels[!allPanels %in% resultTypes]
  if (length(notPresent) > 0) {
    cli::cli_warn("{.var {notPresent}} eliminated from panels as not present in results.")
  }

  # add panels not in choices
  panels <- c(panels, as.list(resultTypes[!resultTypes %in% allPanels]))

  # correct lengths
  panels <- panels |>
    purrr::map(\(x) if (length(x) > 1) as.list(x) else x)

  # correct names of panels and title
  panels <- panelNames(panels)

  return(panels)
}
panelNames <- function(x) {
  # correct names of menus
  if (is.null(names(x))) names(x) <- rep("", length(x))
  names(x) <- names(x) |>
    purrr::imap_chr(\(nm, k) {
      if (nm != "") return(nm)
      if (length(x[[k]]) == 1) { # if single panel it is not a menu
        return(getTitle(x[[k]]))
      } else {
        return("unnamed")
      }
    })
  # correct names of panels if not provided the name is the default one
  x <- x |>
    purrr::map(\(xx) {
      if (is.list(xx)) {
        if (is.null(names(xx))) names(xx) <- rep("", length(xx))
        names(xx) <- names(xx) |>
          purrr::imap_chr(\(nm, k) if (nm != "") nm else getTitle(xx[[k]]))
      }
      return(xx)
    })
  return(x)
}
getTitle <- function(x) {
  getInfo(x, "title", formatTit(x))
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
    present <- unique(unlist(panelStructure))
    all <- names(panelDetails)
    missing <- all[!all %in% present]
    if (length(missing) > 0) {
      cli::cli_inform("{.var {missing}} panels added to panelStrcuture.")
      panelStructure <- c(panelStructure, as.list(missing))
    }
  }
  return(panelStructure)
}
