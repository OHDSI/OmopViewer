
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
    "## get ", nm, " data\n", x$filter_function_name, " <- shiny::reactive({\n", filtersText, "\n})"
  )
}
writeContentServer <- function(content, data) {
  purrr::map_chr(content, \(cont) {
    c(writeOutputServer(cont), writeDownloadServer(cont)) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n")
}
writeOutputServer <- function(content) {
  paste0(
    "output$", content$output_id, " <- ", renderFunction(content$output_type),
    "({\n", content$render_content, "\n})"
  )
}
outputFunction <- function(outputType) {
  switch(outputType,
         "DT" = "DT::DTOutput",
         "gt" = "gt::gt_output",
         "plot" = "shiny::plotOutput")
}
renderFunction <- function(outputType) {
  switch(outputType,
         "DT" = "DT::renderDT",
         "gt" = "gt::render_gt",
         "plot" = "shiny::renderPlot")
}
writeDownloadServer <- function(content) {
  download <- content$download
  if (length(download) == 0) return(character())
  paste0(
    "output$", download$output_id, " <- shiny::downloadHandler(\nfilename = \"",
    download$filename, "\",\ncontent = function(file) {\n", download$render,
    "\n}\n)"
  )
}
