
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
#' @param panels List specifying order of results. Each panel is determined
#' by the available result types in the result object. Panels for any available
#' results not specified will be included after the specified result tabs.
#' @param open Whether to open the shiny app project.
#' @param theme Specify the theme for the Shiny application. You can either select a predefined
#' theme provided by the package (e.g., `"theme1"`), or define a custom theme using `bslib::bs_theme()`.
#' If using a custom theme, it must be provided as a character string (e.g., `"bslib::bs_theme(bg = 'white', fg = 'black')"`).
#' The custom theme allows for advanced customization of the app's appearance, including colours, fonts, and other styling options.
#' @param colourPalette A character vector of two colours to customize the Shiny app's appearance.
#' This argument can be used in place of the theme argument, and allows users to choose
#' custom colours for certain aspects of the shiny. The first colour defines the
#' primary theme colour, which is applied to key interactive components
#' such as buttons, links, and labels, emphasizing important actions and elements.
#' The second colour is used for the header bar at the top of the Shiny app.
#
#'
#' @return The shiny app will be created in directory.
#'
#' @export
#'
#' @examples {
#' exportStaticApp(
#'   result = emptySummarisedResult(),
#'   theme = "theme1"
#' )
#' }
#'
exportStaticApp <- function(result,
                            logo = "ohdsi",
                            title = "",
                            background = TRUE,
                            summary = TRUE,
                            directory = getwd(),
                            panels = list(),
                            theme = NULL,
                            colourPalette = NULL,
                            open = rlang::is_interactive()) {
  # input check
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertLogical(open, length = 1)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertLogical(summary, length = 1)
  omopgenerics::assertCharacter(theme, length = 1, null = TRUE)
  omopgenerics::assertCharacter(colourPalette, length = 2, null = TRUE)
  omopgenerics::assertList(panels)
  omopgenerics::assertLogical(background, length = 1)
  sum <- validateSummary(summary, result)
  directory <- validateDirectory(directory)
  if (isTRUE(directory)) {
    return(cli::cli_inform(c("i" = "{.strong shiny} folder will not be overwritten. Stopping process.")))
  }

  if (!is.null(theme) && !is.null(colourPalette)) {
    cli::cli_abort("Cannot use theme and colourPalette argument at the same time.")
  }

  # processing data
  cli::cli_inform(c("i" = "Processing data"))
  resType <- omopgenerics::settings(result)[["result_type"]] |> unique()
  mes <- "Data processed: {length(resType)} result type{?s} idenfied"
  if (length(resType) == 0) {
    resType <- character()
    mes <- c("!" = paste0(mes, "."))
  } else {
    mes <- c("v" = paste0(mes, ": {.var {resType}}."))
  }
  cli::cli_inform(mes)
  if (length(resType) == 0) {
    cli::cli_inform(c("!" = "No result_type(s) found, the generated shiny will be empty."))
  }

  choices <- getChoices(result)
  panels <- purrr::flatten_chr(panels)
  if (any(isFALSE(panels %in% names(choices)))) {
    cli::cli_warn("'{setdiff(panels, names(choices))}' not found in results")
  }
  # if not specified, append remaining results
  panels <- c(
    intersect(panels, names(choices)),
    setdiff(names(choices), panels)
  )
  choices <- choices[panels]

  # create shiny
  directory <- file.path(directory, "shiny")
  dir.create(path = directory, showWarnings = FALSE)
  cli::cli_inform(c("i" = "Creating shiny from provided data"))

  # copy the logos to the shiny folder
  logo <- copyLogos(logo, directory)

  # create ui
  ui <- c(
    messageShiny(),
    uiStatic(
      choices = choices,
      logo = logo,
      title = title,
      summary = sum,
      background = background,
      theme = theme,
      colourPalette = colourPalette
    )
  )

  # create server
  server <- c(messageShiny(), serverStatic(resultTypes = names(choices)))

  # check installed libraries
  libraries <- c(
    detectPackages(ui), detectPackages(server), detectPackages(omopViewerGlobal)
  ) |>
    unique() |>
    sort()
  checkInstalledPackages(libraries)

  # create global
  libraryStatementsList <- paste0("library(", libraries, ")")
  global <- c(messageShiny(), libraryStatementsList, "", omopViewerGlobal) |>
    styleCode()

  # write files in the corresponding directory
  dir.create(paste0(directory, "/data"), showWarnings = FALSE)
  if (background) {
    writeLines(defaultBackground(logo), con = file.path(directory, "background.md"))
  }
  writeLines(ui, con = paste0(directory, "/ui.R"))
  writeLines(server, con = paste0(directory, "/server.R"))
  writeLines(global, con = paste0(directory, "/global.R"))
  writeLines(omopViewerProj, con = paste0(directory, "/shiny.Rproj"))
  omopgenerics::exportSummarisedResult(
    result,
    minCellCount = 0,
    fileName = "results.csv",
    path = paste0(directory, "/data")
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
      "# Generated by omopViewer ",
      as.character(utils::packageVersion("omopViewer"))
    ),
    "# Be careful editing this file",
    ""
  )
}
copyLogos <- function(logo, directory) {
  # Create 'www' directory if it doesn't exist
  dir.create(paste0(directory, "/www"), showWarnings = FALSE)

  # HDS logo must be copied always as it is needed for about tab
  hdsLogo <- logoPath("hds")
  to <- paste0(directory, "/www/hds_logo.svg")
  file.copy(from = hdsLogo, to = to, overwrite = TRUE)

  if (is.null(logo)) {
    return(NULL)
  }

  # search for standard logo naming:
  logo <- logoPath(logo)

  # copy the logo if exists
  if (file.exists(logo)) {
    nm <- basename(logo)
    to <- paste0(directory, "/www/", nm)
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
    system.file(paste0("/logos/", lowLogo, "_logo.svg"), package = "omopViewer")
  } else {
    logo
  }
}



# ui ----
uiStatic <- function(choices = list(),
                     logo = NULL,
                     title = "",
                     background = TRUE,
                     summary = NULL,
                     theme = NULL,
                     colourPalette = NULL) {
  # initial checks
  omopgenerics::assertList(choices, named = TRUE)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertCharacter(theme, length = 1, null = TRUE)
  omopgenerics::assertCharacter(colourPalette, length = 2, null = TRUE)

  get_theme <- function(theme) {
  if (is.null(theme)) {
    return(NULL)
  }
  if (theme == "theme1") {
    theme <- "bslib::bs_theme(bootswatch = 'sandstone',
    primary = '#605ca8',
    bg = 'white',
    fg = 'black',
    success = '#3B9AB2',
    base_font = font_google('Space Mono'),
    code_font = font_google('Space Mono'))"

    theme <- gsub("\n    ", "", theme)
    return(theme)
  } else if(grepl("bslib::bs_theme", theme)) {
    theme = theme
  } else {
    cli::cli_warn(c("!" = "Custom theme must follow the format 'bslib::bs_theme(...)'.The theme argument has set to NULL."))
    theme = NULL
  }
  }

  theme <- get_theme(theme)

  getColourPalette <- function(colourPalette) {
    if (is.null(colourPalette)) {
      return(NULL)
    }
    if (!is.null(colourPalette)) {
      cp <- sprintf(
        "bslib::bs_theme(
    version = 5,
    preset = 'sandstone',
    bg = '%s',
    fg = '%s',
    primary = '%s',
    success = '%s',
    base_font = font_google('Space Mono'),
    code_font = font_google('Space Mono')
  )",
        "white", # bg
        "black", # fg
        colourPalette[1], # primary
        colourPalette[2] # success
      )
      cp <- gsub("\n", "", cp)
      return(cp)
    }
  }

  cp <- getColourPalette(colourPalette)

  if(is.null(theme) && !is.null(colourPalette)) {
    theme = cp
  } else if (!is.null(theme) && is.null(colourPalette)){
    theme = theme
  } else if(is.null(theme) && is.null(colourPalette)){
    theme = NULL
  }

  c(
    paste0(
      "ui <- bslib::page_navbar(",
      if (!is.null(theme)){
        paste0("theme=", theme, ",")}
    ),
    c(
      pageTitle(title, logo),
      createBackground(background),
      createSummary(summary, logo),
      createUi(names(choices), choices),
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
serverStatic <- function(resultTypes = character()) {
  # initial checks
  omopgenerics::assertCharacter(resultTypes, unique = TRUE)

  paste0(
    c(
      "server <- function(input, output, session) {",
      createServer(resultTypes, data = "data"),
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
