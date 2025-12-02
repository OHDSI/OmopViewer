
#' Export and launch a static shiny specific to the provided results.
#'
#' @param result A summarised_result object.
#' @param directory Directory to create the shiny.
#' @param logo Name of a logo or path to a logo. If NULL no logo is included.
#' Only svg format allowed for the moment.
#' @param title title of the shiny
#' @param background Whether to include a background panel. Background panel
#' content will be controlled from the generated background.md file.
#' @param summary Whether to include a panel with a summary of content in the
#' `result`.
#' @param report Whether to include a quarto report.
#' @param panelDetails A named list to provide details for each one of the
#' panels, such as: result_id, result_type, title, icon, filters and content.
#' By default it is created using the `panelDetailsFromResult()` function.
#' @param panelStructure A named list of panel identifiers to organise them in
#' drop-down menus. Identifiers names are the ones used in `panelDetails`. By
#' default one panel per each `panelDetails` element is created.
#' @param theme Specify the theme for the Shiny application. You can either
#' select a predefined theme provided by the package (e.g., `"theme1"`), or
#' define a custom theme using `bslib::bs_theme()`. If using a custom theme, it
#' must be provided as a character string (e.g.,
#' `"bslib::bs_theme(bg = 'white', fg = 'black')"`).
#' @param template Path to a template `.docx` document to be used for the
#' report.
#' @param updateButtons Whether to include update buttons for visualisations.
#' @param includeOneChoiceFilters Whether to include filter buttons for filters
#' with just one choice.
#' @param open Whether to open the shiny app project.
#'
#' @return The shiny app will be created in directory.
#'
#' @export
#'
#' @examples
#' exportStaticApp(
#'   result = omopgenerics::emptySummarisedResult(),
#'   directory = tempdir()
#' )
#'
exportStaticApp <- function(result,
                            directory,
                            logo = "ohdsi",
                            title = "",
                            background = TRUE,
                            summary = TRUE,
                            report = FALSE,
                            panelDetails = panelDetailsFromResult(result),
                            panelStructure = NULL,
                            theme = NULL,
                            template = NULL,
                            updateButtons = TRUE,
                            includeOneChoiceFilters = TRUE,
                            open = rlang::is_interactive()) {
  # input check
  result <- omopgenerics::validateResultArgument(result)

  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertLogical(open, length = 1)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertLogical(summary, length = 1)
  omopgenerics::assertLogical(report, length = 1)
  omopgenerics::assertCharacter(theme, length = 1, null = TRUE)
  omopgenerics::assertLogical(updateButtons, length = 1)
  omopgenerics::assertLogical(includeOneChoiceFilters, length = 1)
  theme <- validateTheme(theme = theme)
  template <- validateTemplate(template = template, theme = theme)
  panelDetails <- validatePanelDetails(panelDetails, result, includeOneChoiceFilters)
  panelStructure <- validatePanelStructure(panelStructure, names(panelDetails))

  # processing data
  processingData(panelDetails)

  # create shiny
  directory <- createDirectory(directory, "shiny")
  if (isTRUE(directory)) {
    "{.strong shiny} folder will not be overwritten. Stopping process." |>
      rlang::set_names("i") |>
      cli::cli_inform()
    return(invisible())
  }

  # preprocess file
  preprocess <- preprocessData(panelDetails)

  # copy the logos to the shiny folder
  logo <- copyLogos(logo, directory)

  # background
  background <- validateBackground(background, logo)

  # populate options of panelDetails
  panelDetails <- populatePanelDetailsOptions(panelDetails, result)

  # create ui
  ui <- uiStatic(
    logo = logo,
    title = title,
    background = background,
    summary = summary,
    panelDetails = panelDetails,
    panelStructure = panelStructure,
    updateButtons = updateButtons
  )

  # create server
  server <- serverStatic(panelDetails, summary, updateButtons)

  # functions to copy
  functions <- readLines(system.file("functions.R", package = "OmopViewer"))

  # create global
  global <- createGlobal(c(ui, server, preprocess, functions))

  # write files in the corresponding directory
  if (!is.null(background)) {
    writeLines(background, con = file.path(directory, "background.md"))
  }
  writeLines(ui, con = file.path(directory, "ui.R"))
  writeLines(server, con = file.path(directory, "server.R"))
  writeLines(global, con = file.path(directory, "global.R"))
  writeLines(functions, con = file.path(directory, "functions.R"))
  writeLines(omopViewerProj, con = file.path(directory, "shiny.Rproj"))

  # export theme
  exportTheme(theme = theme, directory = directory)

  # export data
  exportData(result = result, preprocess = preprocess, directory = directory)

  # export report
  if (report) {
    # create report
    report <- createReport(panelDetails, title, !is.null(template))

    # export files
    exportReport(report = report, directory = directory)
    exportTemplate(template = template, directory = directory)
  }

  cli::cli_inform(c("v" = "Shiny created in: {.pkg {directory}}"))

  # open shiny
  if (open) {
    cli::cli_inform(c("i" = "Launching shiny"))
    usethis::proj_activate(directory)
  }

  return(invisible())
}

# utilities ----
messageShiny <- function() {
  c(
    paste0(
      "# Generated by OmopViewer ",
      as.character(utils::packageVersion("OmopViewer"))
    ),
    "# Be careful editing this file",
    ""
  )
}
copyLogos <- function(logo, directory) {
  # Create 'www' directory if it doesn't exist
  dir.create(file.path(directory, "www"), showWarnings = FALSE)

  # HDS logo must be copied always as it is needed for about tab
  hdsLogo <- logoPath("hds")
  to <- file.path(directory, "www", "hds_logo.svg")
  file.copy(from = hdsLogo, to = to, overwrite = TRUE)

  if (is.null(logo)) {
    return(NULL)
  }

  # search for standard logo naming:
  logo <- logoPath(logo)

  # copy the logo if exists
  if (file.exists(logo)) {
    nm <- basename(logo)
    to <- file.path(directory, "www", nm)
    if (logo != hdsLogo) {
      file.copy(from = logo, to = to, overwrite = TRUE)
    }
    return(nm)
  } else {
    cli::cli_warn(c("!" = "Logo couldn't be found."))
    return(NULL)
  }
}
logoPath <- function(logo) {
  lowLogo <- stringr::str_to_lower(logo)
  # add more logoKeywords in data-raw/internalData
  if (lowLogo %in% logoKeywords) {
    system.file(file.path("logos", paste0(lowLogo, "_logo.svg")), package = "OmopViewer")
  } else {
    logo
  }
}
formatTit <- function(x) {
  x |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    stringr::str_to_sentence()
}
formatCamel <- function(x) {
  x |>
    snakecase::to_any_case(case = "upper_camel", numerals = "asis")
}
formatSnake <- function(x) {
  x |>
    snakecase::to_any_case(case = "snake", numerals = "asis")
}
cast <- function(x) {
  if (is.character(x)) {
    if (length(x) == 0) {
      x <- "character()"
    } else if (length(x) == 1) {
      if (!stringr::str_detect(x, "\"") & !stringr::str_detect(x, "\\$")) {
        x <- paste0("\"", x, "\"")
      }
    } else {
      x <- paste0('c("', paste0(x, collapse = '", "'), '")')
    }
  } else if (is.null(x)) {
    x <- "NULL"
  } else {
    x <- deparse(x)
  }
  return(x)
}
subs <- function(x, pat, subst) {
  id <- which(x == pat)
  if (length(id) == 1) {
    n <- length(x)
    if (id == 1) {
      x <- c(subst, x[-1])
    } else if (id == n) {
      x <- c(x[-n], subst)
    } else {
      x <- c(x[1:(id - 1)], subst, x[(id + 1):n])
    }
  }
  return(x)
}

# processing data ----
processingData <- function(panelDetails) {
  cli::cli_inform(c("i" = "Processing data"))
  if (length(panelDetails) == 0) {
    c("!" = "No panels identified, generated shiny will be empty.") |>
      cli::cli_inform()
  } else {
    c("v" = "Data processed: {length(panelDetails)} panel{?s} idenfied: {.var {names(panelDetails)}}.") |>
      cli::cli_inform()
  }
  invisible(NULL)
}

# create directory ----
createDirectory <- function(directory, project) {
  directoryProject <- file.path(directory, project)
  # create directory if it does not exit
  if (!dir.exists(directory)) {
    cli::cli_inform(c("i" = "Provided directory does not exist, it will be created."))
    dir.create(path = directory, recursive = TRUE)
    cli::cli_inform(c("v" = "directory created: {.pkg {directory}}"))
  } else if (file.exists(directoryProject)) {
    # ask overwrite shiny
    overwrite <- "1"  # overwrite if non-interactive
    if (rlang::is_interactive()) {
      cli::cli_inform(c(
        "!" = "A {.strong {project}} folder already exists in the provided directory. Enter choice 1 or 2:",
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
      cli::cli_inform(c("i" = "{.strong {project}} folder will be overwritten."))
      unlink(directoryProject, recursive = TRUE)
      cli::cli_inform(c("v" = "Prior {.strong {project}} folder deleted."))
    }
  }
  dir.create(path = directoryProject, showWarnings = FALSE)
  cli::cli_inform(c("i" = "Creating `{project}` from provided data"))
  directoryProject
}

# preprocess file ----
preprocessData <- function(panelDetails) {
  resultList <- purrr::map(panelDetails, \(x) x$data)
  c(
    "# shiny is prepared to work with this resultList:",
    paste0("resultList <- ", writeResultList(resultList)),
    omopViewerPreprocess
  ) |>
    styleCode()
}
writeResultList <- function(resultList) {
  #paste0(deparse(resultList), collapse = "")
  if (length(resultList) == 0) return("list()")
  paste0(
    "list(\n",
    purrr::imap_chr(resultList, \(x, nm) {
      paste0(
        nm,
        " = list(",
        paste0(purrr::imap(x, \(xx, nx) paste0(nx, " = ", cast(xx))), collapse = ", "),
        ")"
      )
    }) |>
      paste0(collapse = ",\n"),
    "\n)"
  )
}

# create global ----
createGlobal <- function(code) {
  libraries <- detectPackages(c(code, omopViewerGlobal))
  libraryStatementsList <- paste0("library(", libraries, ")")

  c(messageShiny(), libraryStatementsList, "", omopViewerGlobal) |>
    styleCode()
}
detectPackages <- function(code) {
  code |>
    stringr::str_extract_all(pattern = "\\b([a-zA-Z0-9\\.]+)::") |>
    unlist() |>
    stringr::str_replace("::", "") |>
    unique() |>
    sort() |>
    checkInstalledPackages()
}
checkInstalledPackages <- function(x) {
  notInstalled <- purrr::keep(x, \(x) !rlang::is_installed(x))
  if (length(notInstalled) > 0) {
    cli::cli_warn(c(
      "!" = "{length(notInstalled)} package{?s} {?is/are} not installed: {.pkg {notInstalled}}."
    ))
    install <- paste0(notInstalled, collapse = '", "')
    cli::cli_inform(c("i" = '{.run install.packages(c("{install}"))}'))
  }
  return(x)
}
