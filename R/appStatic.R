
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
#' @param panelStructure A named list to organise the different panels in
#' dropdown menus.
#' @param panelDetails A named list to provide details for each one of the
#' panels, such as: result_id, result_type, title, icon, output_id, ...
#' @param open Whether to open the shiny app project.
#' @param theme Assign a theme to the shiny app using bslib::bs_theme().
#' @param panels deprecated.
#'
#' @return The shiny app will be created in directory.
#'
#' @export
#'
#' @examples
#' exportStaticApp(
#'   result = emptySummarisedResult(),
#'   directory = tempdir(),
#'   theme = "bslib::bs_theme(bg = '#bb0a1e', fg = '#0000ff')"
#' )
#'
exportStaticApp <- function(result,
                            directory,
                            logo = "ohdsi",
                            title = "",
                            background = TRUE,
                            summary = TRUE,
                            panelStructure = NULL,
                            panelDetails = NULL,
                            theme = NULL,
                            open = rlang::is_interactive(),
                            panels = lifecycle::deprecated()) {
  # input check
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertLogical(open, length = 1)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertLogical(summary, length = 1)
  omopgenerics::assertCharacter(theme, length = 1, null = TRUE)
  directory <- validateDirectory(directory)
  if (isTRUE(directory)) {
    return(cli::cli_inform(c("i" = "{.strong shiny} folder will not be overwritten. Stopping process.")))
  }
  if (lifecycle::is_present(panels)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "exportStaticApp(panels= )",
      with = "exportStaticApp(panelStructure= )"
    )
    if (missing(panelStructure)) panelStructure <- panels
  }
  panelDetails <- validatePanelDetails(panelDetails, result)
  panelStructure <- validatePanelStructure(panelStructure, panelDetails, result)

  # processing data
  cli::cli_inform(c("i" = "Processing data"))
  if (length(panelDetails) == 0) {
    c("!" = "No panels identified, generated shiny will be empty.") |>
      cli::cli_inform()
  } else {
    c("v" = "Data processed: {length(panelDetails)} panel{?s} idenfied: {.var {names(panelDetails)}}.") |>
      cli::cli_inform()
  }

  # create shiny
  directory <- file.path(directory, "shiny")
  dir.create(path = directory, showWarnings = FALSE)
  cli::cli_inform(c("i" = "Creating shiny from provided data"))

  # copy the logos to the shiny folder
  logo <- copyLogos(logo, directory)

  # background
  background <- validateBackground(background, logo)

  # add filter buttons
  panelDetails <- panelDetails |>
    addFilterNames(result)

  # create ui
  ui <- c(
    messageShiny(),
    uiStatic(
      logo = logo,
      title = title,
      summary = summary,
      background = !is.null(background),
      theme = theme,
      panelStructure = panelStructure,
      panelDetails = panelDetails
    )
  )

  # create server
  server <- c(
    messageShiny(),
    serverStatic(panelDetails = panelDetails)
  )

  # check installed libraries
  libraries <- c(
    detectPackages(ui),
    detectPackages(server),
    detectPackages(omopViewerGlobal)
  ) |>
    unique() |>
    sort()
  checkInstalledPackages(libraries)

  # create global
  libraryStatementsList <- paste0("library(", libraries, ")")
  global <- c(messageShiny(), libraryStatementsList, "", omopViewerGlobal) |>
    styleCode()

  # write files in the corresponding directory
  dir.create(file.path(directory, "data"), showWarnings = FALSE)
  if (!is.null(background)) {
    writeLines(background, con = file.path(directory, "background.md"))
  }
  writeLines(ui, con = file.path(directory, "ui.R"))
  writeLines(server, con = file.path(directory, "server.R"))
  writeLines(global, con = file.path(directory, "global.R"))
  writeLines(omopViewerProj, con = file.path(directory, "shiny.Rproj"))
  exportSummarisedResult(
    result,
    minCellCount = 0,
    fileName = "results.csv",
    path = file.path(directory, "data")
  )

  cli::cli_inform(c("v" = "Shiny created in: {.pkg {directory}}"))

  # open shiny
  if (open) {
    cli::cli_inform(c("i" = "Launching shiny"))
    usethis::proj_activate(directory)
  }

  return(invisible())
}

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

# ui ----
uiStatic <- function(logo,
                     title,
                     background,
                     summary,
                     theme,
                     panelDetails,
                     panelStructure) {
  # Create the bslib::bs_theme() call, or use NULL if not provided
  theme_setting <- if (!is.null(theme)) {
    paste0("theme = ", theme, ",")
  } else {
    ""
  }

  # create panels
  panels <- createUiPanels(panelDetails) |>
    structurePanels(panelStructure)

  # ui
  c(
    "ui <- bslib::page_navbar(",
    theme_setting,
    c(
      pageTitle(title, logo),
      createBackground(background),
      summaryTab(summary),
      panels,
      "bslib::nav_spacer()",
      downloadRawDataUi(),
      createAbout("hds_logo.svg"),
      'bslib::nav_item(bslib::input_dark_mode(id ="dark_mode", mode = "light"))'
    ) |>
      paste0(collapse = ",\n"),
    ")"
  ) |>
    paste0(collapse = "\n") |>
    styleCode()
}

pageTitle <- function(title, logo) {
  if (is.null(logo)) {
    x <- 'title = "{title}"'
  } else {
    x <- 'title = shiny::tags$span(
      shiny::tags$img(
        src = "{logo}",
        width = "auto",
        height = "46px",
        class = "me-3",
        alt = "logo"
      ),
      "{title}"
    )'
  }
  x <- glue::glue(x) |> as.character()
  return(x)
}

# server ----
serverStatic <- function(panelDetails) {
  paste0(
    c(
      "server <- function(input, output, session) {",
      createServer(panelDetails, data = "data"),
      "}"
    ),
    collapse = "\n"
  ) |>
    styleCode()
}

# utilities ----
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
      x <- paste0('"', x, '"')
    } else {
      x <- paste0('c("', paste0(x, collapse = '", "'), '")')
    }
  } else if (is.null(x)) {
    x <- "NULL"
  } else if (is.call(x)) {
    x <- deparse(x)
  } else {
    x <- paste0("c(", paste0(x, collapse = ", "), ")")
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
