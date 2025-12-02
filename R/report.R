
exportReport <- function(result,
                         directory,
                         title = "",
                         theme = NULL,
                         template = NULL,
                         panelDetails = panelDetailsFromResult(result),
                         open = rlang::is_interactive()) {
  # input validation
  result <- omopgenerics::validateResultArgument(result = result)
  theme <- validateTheme(theme)
  omopgenerics::assertLogical(open, length = 1)
  omopgenerics::assertCharacter(title, length = 1)
  template <- validateTemplate(template = template, theme = theme)
  omopgenerics::assertList(panelDetails)
  omopgenerics::assertCharacter(directory, length = 1)
  directory <- createDirectory(directory, "report")
  if (isTRUE(directory)) {
    cli::cli_inform(c(i = "{.strong report} folder will not be overwritten. Stopping process."))
    return(invisible())
  }

  # preprocess
  preprocess <- preprocessData(panelDetails)

  # create report
  report <- createReport(panelDetails, title, !is.null(template))

  # export files
  exportReport(report = report, directory = directory)
  exportTheme(theme = theme, directory = directory)
  exportData(result = result, preprocess = preprocess, directory = directory)
  exportTemplate(template = template, directory = directory)
  writeLines(text = "", con = file.path(directory, "functions.R"))

  cli::cli_inform(c("v" = "Report created in: {.pkg {directory}}"))

  # open report
  if (open) {
    cli::cli_inform(c("i" = "Opening report project"))
    usethis::proj_activate(directory)
  }

  invisible()
}

validateTemplate <- function(template, theme, call = parent.frame()) {
  if (is.null(template) & !is.null(theme$defaults$visOmopResults$template)) {
    template <- theme$defaults$visOmopResults$template
  }
  omopgenerics::assertCharacter(template, length = 1, call = call, null = TRUE)
  if (!is.null(template)) {
    if (startsWith(x = template, prefix = "system.file")) {
      template <- rlang::eval_tidy(rlang::parse_expr(template))
    }
    if (!file.exists(template)) {
      cli::cli_warn(c("!" = "`template` ignored as file does not exist."))
      template <- NULL
    }
  }
  return(template)
}
createReport <- function(panelDetails, title, useTemplate) {
  # get code
  code <- panelDetails |>
    purrr::imap(\(x, nm) {
      report <- x$report
      if (is.null(report)) return(report)
      fig <- 1
      tab <- 1
      for (k in seq_along(report)) {
        if (names(report)[k] != "title") {
          if (report[[k]]$type == "table") {
            report[[k]] <- paste0(
              ':::{custom-style="Caption"}\n**Table ', tab, ':** ',
              report[[k]]$caption, '\n:::\n\n```{r}\n', report[[k]]$content, '\n```'
            )
            tab <- tab + 1
          } else {
            report[[k]] <- paste0(
              ':::{custom-style="Caption"}\n**Figure ', fig, ':** ',
              report[[k]]$caption, '\n:::\n\n```{r}\n', report[[k]]$content, '\n```'
            )
            fig <- fig + 1
          }
        } else {
          report[[k]] <- paste0("## ", report[[k]])
        }
      }
      paste0(report, collapse = "\n\n") |>
        stringr::str_replace_all(
          pattern = "<data>",
          replacement = paste0("data[[\"", nm, "\"]]")
        )
    }) |>
    purrr::compact() |>
    paste0(collapse = "\n\n")

  # get packages
  packages <- sort(unique(c("visOmopResults", detectPackages(code = code))))
  packages <- paste0("library(", packages, ")", collapse = "\n")

  if (useTemplate) {
    template <- "reference-doc: template.docx\n    fig-cap-location: top"
  } else {
    template <- "fig-cap-location: top"
  }

  # create report
  reportTemplate |>
    stringr::str_replace_all(pattern = "<title>", replacement = title) |>
    stringr::str_replace_all(pattern = "<packages>", replacement = packages) |>
    stringr::str_replace_all(pattern = "<code>", replacement = code) |>
    stringr::str_replace_all(pattern = "<template>", replacement = template)
}
exportReport <- function(report, directory) {
  writeLines(text = report, con = file.path(directory, "report.qmd"))
}
exportTemplate <- function(template, directory) {
  if (!is.null(template)) {
    file.copy(from = template, to = file.path(directory, "template.docx"))
  }
}
exportTheme <- function(theme, directory) {
  yaml::write_yaml(x = theme, file = file.path(directory, "_brand.yml"))
}
exportData <- function(result, preprocess, directory) {
  # export rawData
  dataPath <- file.path(directory, "rawData")
  dir.create(dataPath, showWarnings = FALSE)
  omopgenerics::exportSummarisedResult(
    result = result,
    minCellCount = 0,
    fileName = "results.csv",
    path = dataPath,
    logFile = NULL,
    logSqlPath = NULL
  )
  writeLines(preprocess, con = file.path(dataPath, "preprocess.R"))
  cont <- c(
    "# raw data", "",
    "This folder contains the raw `.csv` files that feed the shiny App.",
    "This folder is ignored when deploying"
  )
  writeLines(cont, con = file.path(dataPath, "README.md"))

  # export data
  dataPath <- file.path(directory, "data")
  dir.create(dataPath, showWarnings = FALSE)
  cont <- c("# data", "", "This folder contains the `.RData` file that feeds the shiny App and report.")
  writeLines(cont, con = file.path(dataPath, "README.md"))

  # rscignore
  writeLines(rscignore, con = file.path(directory, ".rscignore"))

  invisible()
}
