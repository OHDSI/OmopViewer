
# static ----
serverStatic <- function(panelDetails, summary) {
  paste0(
    c(
      messageShiny(),
      "server <- function(input, output, session) {",
      createSummaryServer(summary, data = "data"),
      createServer(panelDetails, data = "data"),
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
createServer <- function(panelDetails, data) {
  c(
    downloadRawDataServer(data),
    purrr::imap_chr(panelDetails, \(x, nm) {
      c(glue::glue("# {nm} -----"),
        writeFilterData(x = x, nm = nm, data = data),
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
        omopgenerics::exportSummarisedResult(fileName = file)
    }
  )' |>
    glue::glue(.open = "[", .close = "]") |>
    as.character()
}
writeFilterData <- function(x, nm, data) {
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
  paste0(
    "## get ", nm, " data\n", x$filter_function, " <- shiny::reactive({\n", filtersText, "\n})"
  )
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
    content$render_function, " <- shiny::reactive({\n", content$render,
    "\n})\noutput$", content$output_id, " <- ", renderFunction(content$output_type),
    "({\n", content$render_function, "()\n})"
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
  if (length(download) == 0) return(character())
  paste0(
    "output$", download$output_id, " <- shiny::downloadHandler(\nfilename = ",
    cast(download$filename), ",\ncontent = function(file) {\n", download$render,
    "\n}\n)"
  )
}
