
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
                            panelDetails = panelDetailsFromResult(result),
                            panelStructure = as.list(names(panelDetails)),
                            theme = NULL,
                            open = rlang::is_interactive()) {
  # input check
  result <- omopgenerics::validateResultArgument(result)

  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertLogical(open, length = 1)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertLogical(summary, length = 1)
  omopgenerics::assertCharacter(theme, length = 1, null = TRUE)
  theme <- validateTheme(theme)
  panelDetails <- validatePanelDetails(panelDetails, result)
  panelStructure <- validatePanelStructure(panelStructure, names(panelDetails))

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
  directory <- validateDirectory(directory)
  if (isTRUE(directory)) {
    return(cli::cli_inform(c("i" = "{.strong shiny} folder will not be overwritten. Stopping process.")))
  }
  directory <- file.path(directory, "shiny")
  dir.create(path = directory, showWarnings = FALSE)
  cli::cli_inform(c("i" = "Creating shiny from provided data"))

  # preprocess file
  resultList <- panelDetails |>
    purrr::map(\(x) {
      list(result_id = x$data$result_id, result_type = x$data$result_type) |>
        purrr::compact()
    })
  preprocess <- c(
    "# shiny is prepared to work with this resultList:",
    paste0("resultList <- ", writeResultList(resultList)),
    omopViewerPreprocess
  ) |>
    styleCode()

  # copy the logos to the shiny folder
  logo <- copyLogos(logo, directory)

  # background
  background <- validateBackground(background, logo)

  # populate options of panelDetails
  panelDetails <- panelDetails |>
    # populate inputId and outputId names
    populateIds() |>
    # get filter and render function name
    populateFunctionNames() |>
    # populate in filte the prefix and the name of the function
    populateRender()

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
  ) |>
    styleCode()

  # create server
  server <- c(
    messageShiny(),
    serverStatic(panelDetails = panelDetails)
  ) |>
    paste0(collapse = "\n") |>
    styleCode()

  # functions to copy
  functions <- readLines(system.file("functions.R", package = "OmopViewer"))

  # check installed libraries
  libraries <- c(
    detectPackages(ui),
    detectPackages(server),
    detectPackages(omopViewerGlobal),
    detectPackages(preprocess),
    detectPackages(functions)
  ) |>
    unique() |>
    sort()
  checkInstalledPackages(libraries)

  # create global
  libraryStatementsList <- paste0("library(", libraries, ")")
  global <- c(messageShiny(), libraryStatementsList, "", omopViewerGlobal) |>
    styleCode()

  # write files in the corresponding directory
  if (!is.null(background)) {
    writeLines(background, con = file.path(directory, "background.md"))
  }
  writeLines(ui, con = file.path(directory, "ui.R"))
  writeLines(server, con = file.path(directory, "server.R"))
  writeLines(global, con = file.path(directory, "global.R"))
  writeLines(functions, con = file.path(directory, "functions.R"))
  writeLines(omopViewerProj, con = file.path(directory, "shiny.Rproj"))

  # export data
  dataPath <- file.path(directory, "data")
  dir.create(dataPath, showWarnings = FALSE)
  omopgenerics::exportSummarisedResult(
    result, minCellCount = 0, fileName = "results.csv", path = dataPath
  )
  writeLines(preprocess, con = file.path(dataPath, "preprocess.R"))

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
addFilterNames <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(x) {
      filters <- omopgenerics::settings(result) |>
        dplyr::filter(.data$result_id %in% .env$x$result_id) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_version", "package_name",
          "min_cell_count"
        ))) |>
        purrr::imap(\(x, nm) {
          if (nm %in% c("group", "strata", "additional")) {
            x <- x |>
              stringr::str_split(pattern = " &&& ") |>
              unlist() |>
              unique()
            glue::glue("grouping_{x[x != \"\"]}") |>
              as.character()
          } else if (sum(!is.na(x)) > 0){
            paste0("settings_", nm)
          } else {
            NULL
          }
        }) |>
        purrr::compact() |>
        unname() |>
        unlist()
      if (any(result$result_id %in% x$result_id)) {
        filters <- c(
          "grouping_cdm_name", "variable_name", "estimate_name", filters
        )
      }
      if (length(filters) == 0) filters <- character()
      x$filter_name <- filters
      x
    })
}

# ui ----
uiStatic <- function(logo,
                     title,
                     background,
                     summary,
                     theme,
                     panelDetails,
                     panelStructure) {
  # theme
  theme_setting <- paste0("theme = ", theme, ",")

  # create panels
  panels <- writeUiPanels(panelDetails) |>
    structurePanels(panelStructure)

  # ui
  c(
    "ui <- bslib::page_navbar(",
    c(
      pageTitle(title, logo),
      pageTheme(theme),
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
    paste0(collapse = "\n")
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
  )
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
writeResultList <- function(resultList) {
  #paste0(deparse(resultList), collapse = "")
  if (length(resultList) == 0) return("list()")
  paste0(
    "list(\n",
    purrr::imap_chr(resultList, \(x, nm) {
      rt <- x$result_type
      ri <- x$result_id
      if (is.null(rt)) {
        if (is.null(ri)) {
          res <- "list()"
        } else {
          res <- paste0("list(result_id = c(", paste0(ri, collapse = "L, "), "L))")
        }
      } else {
        if (is.null(ri)) {
          res <- paste0("list(result_type = ", cast(rt), ")")
        } else {
          res <- paste0("list(\nresult_type = ", cast(rt), ",\nresult_id = c(", paste0(ri, collapse = "L, "), "L)\n)")
        }
      }
      paste0(cast(nm), " = ", res)
    }) |>
      paste0(collapse = ",\n"),
    "\n)"
  )
}
populateValues <- function(panelDetails, result) {
  res <- panelDetails |>
    purrr::map(\(x) {
      # filter result
      res <- filterResult(result, x$data$result_id, x$data$result_type)

      # get values
      values <- res |>
        omopgenerics::splitAll() |>
        omopgenerics::addSettings() |>
        dplyr::select(!c("result_id", "estimate_type", "estimate_value")) |>
        purrr::map(unique)
      values$group <- omopgenerics::groupColumns(res)
      values$strata <- omopgenerics::strataColumns(res)
      values$additional <- omopgenerics::additionalColumns(res)
      values$settings <- omopgenerics::settingsColumns(res)

      # create automatic filters
      automaticFilters <- unique(x$automatic_filters) |>
        purrr::map(\(x) {
          if (!x %in% c("group", "strata", "additional", "settings")) {
            rlang::set_names("main", x)
          } else {
            vals <- values[[x]]
            rlang::set_names(rep(x, length(vals)), vals)
          }
        }) |>
        purrr::flatten_chr()
      # exclude filters
      automaticFilters <- automaticFilters[
        !names(automaticFilters) %in% as.character(unique(x$exclude_filters))
      ]
      # create filters
      automaticFilters <- automaticFilters |>
        purrr::imap(\(x, nm) {
          list(
            button_type = "pickerInput",
            label = cast(formatTit(nm)),
            column = nm,
            column_type = x,
            choices = "choices$",
            selected = "selected$",
            multiple = TRUE
          )
        })
      x$filters <- c(x$filters, automaticFilters)

      # populate filters
      x$filters <- substituteFilters(x$filters, values)

      # populate filters of content
      x$content <- x$content |>
        purrr::map(\(cont) {
          cont$filters <- substituteFilters(cont$filters, values)
          cont
        })

      x
    })
}
substituteFilters <- function(filters, values) {
  filters |>
    purrr::map(\(filt) {
      purrr::imap(filt, \(x, nm) {
        if (nm %in% c("choices", "selected")) {
          x <- substituteValues(x, values)
        }
        x
      })
    })
}
substituteValues <- function(x, values) {
  if (length(x) == 1 & any(stringr::str_detect(x, "\\$"))) {
    return(x)
  }
  id <- stringr::str_detect(x, "^<.*>$")
  for (k in which(id)) {
    keyWord <- substr(x[k], 2, nchar(x[k]) - 1)
    x[k] <- paste0(values[[keyWord]], collapse = "\", \"")
  }
  x <- x[nchar(x) > 0]
  paste0("c(\"", paste0(x, collapse = "\", \""), "\")")
}
populateIds <- function(panelDetails) {
  panelDetails |>
    purrr::imap(\(x, nmp) {
      x$filters <- x$filters |>
        purrr::imap(\(x, nmf) addInputId(x, paste0(nmp, "_", nmf)))
      x$content <- x$content |>
        purrr::imap(\(cont, nmc) {
          if (!"output_id" %in% names(cont)) {
            cont$output_id <- paste0(nm, "_", nmc)
          }
          cont$filters <- cont$filters |>
            purrr::imap(\(x, nmf) addInputId(x, paste0(nmp, "_", nmc, "_", nmf)))
          if ("download" %in% names(cont) & !"output_id" %in% names(con$download)) {
            cont$download$output_id <- paste0(nm, "_", nmc, "_download")
          }
          cont
        })
      x
    })
}
addInputId <- function(x, def) {
  if ("inputId" %in% names(x)) {
    x$input_id <- x$inputId
  } else if ("input_id" %in% names(x)) {
    x$inputId <- x$input_id
  } else {
    x$input_id <- def
    x$inputId <- def
  }
  x
}
populateFunctionNames <- function(panelDetails) {
  panelDetails |>
    # filter function name
    purrr::imap(\(x, nm) {
      if ("filters" %in% names(x) & !"filter_function_name" %in% names(x)) {
        x$filter_function_name <- paste0("get", formatCamel(paste0(nm, "_data")))
      }
      x
    }) |>
    # render function name
    purrr::imap(\(x, nmp) {
      x$content <- x$content |>
        purrr::imap(\(cont, nmc) {
          if (!"render_content_name" %in% names(cont)) {
            cont$render_content_name <- paste0("get", formatCamel(paste0(
              nmp, "_", nmc
            )))
          }
          cont
        })
      x
    })
}
