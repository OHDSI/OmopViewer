
# static ----
serverStatic <- function(panelDetails, summary, updateButtons, report) {
  paste0(
    c(
      messageShiny(),
      "server <- function(input, output, session) {",
      createSummaryServer(summary, data = "data"),
      createServer(panelDetails, data = "data", updateButtons = updateButtons),
      createReportServer(report),
      "}"
    ),
    collapse = "\n"
  ) |>
    styleCode()
}

# dynamic ----

# functions ----
createSummaryServer <- function(summary, data) {
  if (summary) {
    c("# summary ----",
      "output$summary_cdm_name <- shinyTree::renderTree(summaryCdmName({data}))",
      "output$summary_packages <- shinyTree::renderTree(summaryPackages({data}))",
      "output$summary_min_cell_count <- shinyTree::renderTree(summaryMinCellCount({data}))",
      "output$summary_panels <- shinyTree::renderTree(summaryPanels({data}))") |>
      purrr::map_chr(\(x) glue::glue(x, data = data))
  } else {
    character()
  }
}
createServer <- function(panelDetails, data, updateButtons) {
  if (updateButtons & length(panelDetails) > 0) {
    updateButtonsStart <- paste0(
      "# update buttons ----\nupdateButtons <- shiny::reactiveValues(\n",
      paste0(names(panelDetails), " = FALSE", collapse = ",\n"),
      "\n)\n"
    )
  } else {
    updateButtonsStart <- character()
  }
  c(
    downloadRawDataServer(data),
    updateButtonsStart,
    purrr::imap_chr(panelDetails, \(x, nm) {
      c("",
        paste0("# ", nm, " -----"),
        writeUpdateDataMessage(nm = nm, filters = x$filters, updateButtons = updateButtons),
        writeFilterData(x = x, nm = nm, data = data, updateButtons = updateButtons),
        writeContentServer(content = x$content, data = data)
      ) |>
        paste0(collapse = "\n")
    })
  )
}
downloadRawDataServer <- function(data) {
  '# download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      [data] |>
        omopgenerics::bind() |>
        omopgenerics::exportSummarisedResult(
          fileName = file, logFile = NULL, logSqlPath = NULL
        )
    }
  )' |>
    glue::glue(.open = "[", .close = "]") |>
    as.character()
}
createReportServer <- function(report) {
  if (!report) return(character())
  '# report ----
  report_path <- function(extension) {
    file.path("www", "reports", paste0("report.", extension))
  }
  report_url <- function(extension) {
    paste0("reports/report.", extension)
  }
  report_metadata <- function() {
    metadata_file <- file.path("www", "reports", "report_metadata.rds")
    if (!file.exists(metadata_file)) {
      return(NULL)
    }
    tryCatch(readRDS(metadata_file), error = function(e) NULL)
  }
  report_is_stale <- function(metadata) {
    if (is.null(metadata)) {
      return(FALSE)
    }
    stale <- FALSE
    if (!is.null(metadata$report_qmd_md5) && file.exists("report.qmd")) {
      stale <- stale || !identical(
        unname(tools::md5sum("report.qmd")),
        metadata$report_qmd_md5
      )
    }
    data_file <- file.path("data", "studyData.RData")
    if (!is.null(metadata$data_md5) && file.exists(data_file)) {
      stale <- stale || !identical(
        unname(tools::md5sum(data_file)),
        metadata$data_md5
      )
    }
    stale
  }

  output$report_status <- shiny::renderUI({
    has_html <- file.exists(report_path("html"))
    has_docx <- file.exists(report_path("docx"))
    if (!has_html && !has_docx) {
      return(shiny::div(
        class = "alert alert-warning mb-3",
        "Report files have not been rendered. Run renderReport.R locally before publishing."
      ))
    }
    if (!has_html || !has_docx) {
      return(shiny::div(
        class = "alert alert-warning mb-3",
        "One or more report files are missing. Run renderReport.R locally before publishing."
      ))
    }

    metadata <- report_metadata()
    if (is.null(metadata)) {
      return(shiny::div(
        class = "alert alert-warning mb-3",
        "Report files are available, but metadata is missing. Run renderReport.R to refresh them before publishing."
      ))
    }

    rendered_at <- metadata$rendered_at
    rendered_at <- if (is.null(rendered_at)) {
      "unknown"
    } else {
      format(rendered_at, "%Y-%m-%d %H:%M")
    }
    status <- paste("Report generated:", rendered_at)
    if (report_is_stale(metadata)) {
      shiny::div(
        class = "alert alert-warning mb-3",
        status,
        shiny::br(),
        "report.qmd or the processed data has changed. Run renderReport.R locally to refresh the report files."
      )
    } else {
      shiny::div(class = "alert alert-success mb-3", status)
    }
  })

  output$report_preview <- shiny::renderUI({
    if (!file.exists(report_path("html"))) {
      return(shiny::div(class = "text-muted", "HTML preview is not available."))
    }
    shiny::tags$iframe(
      src = report_url("html"),
      width = "100%",
      height = "800px",
      style = "border: 1px solid #ddd; border-radius: 4px; background: white;"
    )
  })

  output$download_report_docx <- shiny::downloadHandler(
    filename = "report.docx",
    content = function(file) {
      path <- report_path("docx")
      if (!file.exists(path)) {
        stop("report.docx has not been rendered. Run renderReport.R locally before publishing.")
      }
      file.copy(path, file, overwrite = TRUE)
    }
  )
  output$download_report_html <- shiny::downloadHandler(
    filename = "report.html",
    content = function(file) {
      path <- report_path("html")
      if (!file.exists(path)) {
        stop("report.html has not been rendered. Run renderReport.R locally before publishing.")
      }
      file.copy(path, file, overwrite = TRUE)
    }
  )'
}
writeUpdateDataMessage <- function(nm, filters, updateButtons) {
  if (length(filters) == 0 || !updateButtons) return(character())
  inputs <- c(
    paste0("shiny::observe({updateButtons$", nm, " <- TRUE}) |>"),
    "shiny::bindEvent(",
    c(paste0("input$", nm, "_", names(filters)), "ignoreInit = TRUE") |>
      paste0(collapse = ",\n"),
    ")"
  ) |>
    paste0(collapse = "\n")
  update <- paste0(
  "shiny::observeEvent(updateButtons$", nm, ", {
  if (updateButtons$", nm, " == TRUE) {
    output$update_message_", nm, " <- shiny::renderUI(updateMessage) # defined in functions.R
  } else {
    output$update_message_", nm, " <- shiny::renderUI(NULL)
  }
  })\n"
  )
  silence <- paste0(
    "shiny::observeEvent(input$update_", nm, ", {updateButtons$", nm,
    " <- FALSE})"
  )
  paste0(
    "## update message if filter is changed\n", inputs, "\n",  update, silence,
    "\n"
  )
}
writeFilterData <- function(x, nm, data, updateButtons) {
  # join by filter function
  filtersToApply <- list()
  filters <- x$filters
  for (k in seq_along(filters)) {
    type <- filters[[k]]$column_type
    filtersToApply[[type]] <- c(
      filtersToApply[[type]],
      rlang::set_names(x = filters[[k]]$inputId, nm = filters[[k]]$column)
    )
  }
  # filter functions
  filtersText <- c(
    paste0(data, "[['", nm, "']]"),
    filtersToApply |>
      purrr::imap_chr(\(x, nm) {
        fun <- switch(nm,
                      "main" = 'dplyr::filter(',
                      "group" = 'omopgenerics::filterGroup(',
                      "strata" = 'omopgenerics::filterStrata(',
                      "additional" = 'omopgenerics::filterAdditional(',
                      "settings" = 'omopgenerics::filterSettings(')
        paste0(fun, paste0(".data$", names(x), " %in% input$", x, collapse = ",\n"), ")")
      })
  ) |>
    paste0(collapse = " |>\n")
  if (updateButtons) {
    x <- paste0(
      "## get ", nm, " data\n", x$filter_function,
      " <- shiny::eventReactive(input$update_", nm, ", {\n", filtersText, "\n})"
    )
  } else {
    x <- paste0(
      "## get ", nm, " data\n", x$filter_function, " <- shiny::reactive({\n",
      filtersText, "\n})"
    )
  }
  x
}
writeContentServer <- function(content, data) {
  purrr::map_chr(content, \(cont) {
    c(writeOutputServer(cont), cont$observe, writeDownloadServer(cont)) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n")
}
writeOutputServer <- function(content) {
  paste0(
    content$reactive_function, " <- shiny::reactive({\n", content$reactive, "\n})\n",
    "output$", content$output_id, " <- ", renderFunction(content$output_type),
    "({\n", content$render, "\n})"
  )
}
outputFunction <- function(outputType) {
  switch(outputType,
         "DT" = "DT::DTOutput",
         "gt" = "gt::gt_output",
         "plot" = "shiny::plotOutput",
         "grViz" = "DiagrammeR::grVizOutput",
         "plotly" = "plotly::plotlyOutput",
         "ui" = "shiny::uiOutput",
         "reactable" = "reactable::reactableOutput")
}
renderFunction <- function(outputType) {
  switch(outputType,
         "DT" = "DT::renderDT",
         "gt" = "gt::render_gt",
         "plot" = "shiny::renderPlot",
         "grViz" = "DiagrammeR::renderGrViz",
         "plotly" = "plotly::renderPlotly",
         "ui" = "shiny::renderUI",
         "reactable" = "reactable::renderReactable")
}
writeDownloadServer <- function(content) {
  download <- content$download
  if (length(download$render) == 0)  {
    return(character())
  }
  paste0(
    "output$", download$output_id, " <- shiny::downloadHandler(\nfilename = ",
    cast(download$filename), ",\ncontent = function(file) {\n", download$render,
    "\n}\n)"
  )
}
