
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

  # add readme
  copyReadme(shiny = FALSE, report = TRUE, title = title, directory = directory)

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
    template <- paste0(
      "reference-doc: template.docx\n",
      "    fig-cap-location: top\n",
      "  html:\n",
      "    toc: true\n",
      "    embed-resources: true"
    )
  } else {
    template <- paste0(
      "fig-cap-location: top\n",
      "  html:\n",
      "    toc: true\n",
      "    embed-resources: true"
    )
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

#' Render a generated OmopViewer report
#'
#' Render the `report.qmd` file in a generated OmopViewer app or report
#' directory and copy the outputs to `www/reports`.
#'
#' @param directory Directory containing `report.qmd`.
#' @param formats Character vector of formats to render. Supported values are
#'   `"html"` and `"docx"`.
#' @param quiet Whether to suppress Quarto output.
#'
#' @return Invisibly returns report metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' renderReport("shiny")
#' }
renderReport <- function(directory = getwd(), formats = c("html", "docx"), quiet = FALSE) {
  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertCharacter(formats)
  formats <- unique(formats)
  unsupported <- setdiff(formats, c("html", "docx"))
  if (length(unsupported) > 0) {
    cli::cli_abort("Unsupported report format{?s}: {.val {unsupported}}.")
  }
  directory <- normalizePath(directory, mustWork = TRUE)
  renderReportFiles(directory = directory, formats = formats, quiet = quiet)
}
renderReportFiles <- function(directory, formats, quiet) {
  reportFile <- file.path(directory, "report.qmd")
  if (!file.exists(reportFile)) {
    cli::cli_abort("No {.file report.qmd} file was found in {.path {directory}}.")
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    cli::cli_abort("The {.pkg quarto} R package is required to render reports.")
  }
  if (!quarto::quarto_available()) {
    cli::cli_abort(c(
      "The Quarto CLI is required to render reports.",
      "i" = "Install Quarto locally, then run {.file renderReport.R} again."
    ))
  }

  reportsDirectory <- file.path(directory, "www", "reports")
  dir.create(reportsDirectory, recursive = TRUE, showWarnings = FALSE)

  old <- setwd(directory)
  on.exit(setwd(old), add = TRUE)

  dataFile <- file.path("data", "studyData.RData")
  preprocessFile <- file.path("rawData", "preprocess.R")
  if (!file.exists(dataFile)) {
    if (!file.exists(preprocessFile)) {
      cli::cli_abort("No processed data or preprocessing script was found.")
    }
    source(preprocessFile, local = TRUE)
  }

  outputNames <- stats::setNames(paste0("report.", formats), formats)
  outputFiles <- stats::setNames(file.path("www", "reports", outputNames), formats)
  rootArtifacts <- unique(c(outputNames, paste0(tools::file_path_sans_ext(outputNames), "_files")))
  reportArtifacts <- unique(c(
    outputFiles,
    file.path("www", "reports", paste0(tools::file_path_sans_ext(outputNames), "_files")),
    file.path("www", "reports", "report_metadata.rds")
  ))
  unlink(c(rootArtifacts, reportArtifacts), recursive = TRUE, force = TRUE)

  for (fmt in formats) {
    rendered <- quarto::quarto_render(
      input = "report.qmd",
      output_format = fmt,
      output_file = outputNames[[fmt]],
      quiet = quiet
    )
    renderedFile <- outputNames[[fmt]]
    if (length(rendered) > 0 && file.exists(rendered)) {
      renderedFile <- rendered
    }
    if (!file.exists(renderedFile)) {
      cli::cli_abort("Quarto did not create {.file {outputNames[[fmt]]}}.")
    }
    file.copy(renderedFile, outputFiles[[fmt]], overwrite = TRUE)
    if (!file.exists(outputFiles[[fmt]])) {
      cli::cli_abort("Quarto did not create {.file {outputFiles[[fmt]]}}.")
    }
    if (!identical(normalizePath(renderedFile), normalizePath(outputFiles[[fmt]]))) {
      unlink(renderedFile, recursive = TRUE)
    }
  }

  metadata <- createReportMetadata(formats, outputFiles)
  saveRDS(metadata, file = file.path("www", "reports", "report_metadata.rds"))
  invisible(metadata)
}
createReportMetadata <- function(formats, outputFiles) {
  md5File <- function(path) {
    if (file.exists(path)) {
      unname(tools::md5sum(path))
    } else {
      NA_character_
    }
  }

  list(
    rendered_at = Sys.time(),
    formats = formats,
    files = outputFiles,
    report_qmd_md5 = md5File("report.qmd"),
    data_md5 = md5File(file.path("data", "studyData.RData")),
    output_md5 = stats::setNames(
      vapply(outputFiles, md5File, character(1)),
      names(outputFiles)
    ),
    quarto_version = tryCatch(
      as.character(quarto::quarto_version()),
      error = function(e) NA_character_
    )
  )
}
exportRenderReportScript <- function(directory) {
  writeLines(
    text = renderReportScript(),
    con = file.path(directory, "renderReport.R")
  )
}
renderReportScript <- function() {
  c(
    "# Regenerate the pre-rendered report files for this Shiny app.",
    "# Run from the generated Shiny app directory before publishing.",
    "",
    "reportDirectory <- function() {",
    "  if (requireNamespace(\"here\", quietly = TRUE)) {",
    "    here::here()",
    "  } else {",
    "    getwd()",
    "  }",
    "}",
    "",
    "renderReport <- function(directory = reportDirectory(), formats = c(\"html\", \"docx\"), quiet = FALSE) {",
    "  formats <- unique(formats)",
    "  unsupported <- setdiff(formats, c(\"html\", \"docx\"))",
    "  if (length(unsupported) > 0) {",
    "    stop(\"Unsupported report format: \", paste(unsupported, collapse = \", \"), call. = FALSE)",
    "  }",
    "  directory <- normalizePath(directory, mustWork = TRUE)",
    "  reportFile <- file.path(directory, \"report.qmd\")",
    "  if (!file.exists(reportFile)) {",
    "    stop(\"No report.qmd file was found in \", directory, call. = FALSE)",
    "  }",
    "  if (!requireNamespace(\"quarto\", quietly = TRUE)) {",
    "    stop(\"The quarto R package is required to render reports.\", call. = FALSE)",
    "  }",
    "  if (!quarto::quarto_available()) {",
    "    stop(\"The Quarto CLI is required to render reports.\", call. = FALSE)",
    "  }",
    "",
    "  reportsDirectory <- file.path(directory, \"www\", \"reports\")",
    "  dir.create(reportsDirectory, recursive = TRUE, showWarnings = FALSE)",
    "",
    "  old <- setwd(directory)",
    "  on.exit(setwd(old), add = TRUE)",
    "",
    "  dataFile <- file.path(\"data\", \"studyData.RData\")",
    "  preprocessFile <- file.path(\"rawData\", \"preprocess.R\")",
    "  if (!file.exists(dataFile)) {",
    "    if (!file.exists(preprocessFile)) {",
    "      stop(\"No processed data or preprocessing script was found.\", call. = FALSE)",
    "    }",
    "    source(preprocessFile, local = TRUE)",
    "  }",
    "",
    "  outputNames <- stats::setNames(paste0(\"report.\", formats), formats)",
    "  outputFiles <- stats::setNames(file.path(\"www\", \"reports\", outputNames), formats)",
    "  rootArtifacts <- unique(c(outputNames, paste0(tools::file_path_sans_ext(outputNames), \"_files\")))",
    "  reportArtifacts <- unique(c(",
    "    outputFiles,",
    "    file.path(\"www\", \"reports\", paste0(tools::file_path_sans_ext(outputNames), \"_files\")),",
    "    file.path(\"www\", \"reports\", \"report_metadata.rds\")",
    "  ))",
    "  unlink(c(rootArtifacts, reportArtifacts), recursive = TRUE, force = TRUE)",
    "",
    "  for (fmt in formats) {",
    "    rendered <- quarto::quarto_render(",
    "      input = \"report.qmd\",",
    "      output_format = fmt,",
    "      output_file = outputNames[[fmt]],",
    "      quiet = quiet",
    "    )",
    "    renderedFile <- outputNames[[fmt]]",
    "    if (length(rendered) > 0 && file.exists(rendered)) {",
    "      renderedFile <- rendered",
    "    }",
    "    if (!file.exists(renderedFile)) {",
    "      stop(\"Quarto did not create \", outputNames[[fmt]], call. = FALSE)",
    "    }",
    "    file.copy(renderedFile, outputFiles[[fmt]], overwrite = TRUE)",
    "    if (!file.exists(outputFiles[[fmt]])) {",
    "      stop(\"Quarto did not create \", outputFiles[[fmt]], call. = FALSE)",
    "    }",
    "    if (!identical(normalizePath(renderedFile), normalizePath(outputFiles[[fmt]]))) {",
    "      unlink(renderedFile, recursive = TRUE)",
    "    }",
    "  }",
    "",
    "  md5File <- function(path) {",
    "    if (file.exists(path)) {",
    "      unname(tools::md5sum(path))",
    "    } else {",
    "      NA_character_",
    "    }",
    "  }",
    "  metadata <- list(",
    "    rendered_at = Sys.time(),",
    "    formats = formats,",
    "    files = outputFiles,",
    "    report_qmd_md5 = md5File(\"report.qmd\"),",
    "    data_md5 = md5File(file.path(\"data\", \"studyData.RData\")),",
    "    output_md5 = stats::setNames(",
    "      vapply(outputFiles, md5File, character(1)),",
    "      names(outputFiles)",
    "    ),",
    "    quarto_version = tryCatch(",
    "      as.character(quarto::quarto_version()),",
    "      error = function(e) NA_character_",
    "    )",
    "  )",
    "  saveRDS(metadata, file = file.path(\"www\", \"reports\", \"report_metadata.rds\"))",
    "  message(\"Report files written to \", normalizePath(\"www/reports\", mustWork = FALSE))",
    "  invisible(metadata)",
    "}",
    "",
    "renderReport()"
  )
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
