
createServer <- function(panelDetails, data) {
  c(
    downloadRawDataServer(data),
    purrr::imap_chr(panelDetails, \(x, nm) {
      c(glue::glue("# {nm} -----"),
        filterData(filters = x$filters, nm = nm, data = data),
        "\n"
      ) |>
        paste0(collapse = "\n")
    })
  )
}
filterData <- function(filters, nm, data = data) {
  funName <- snakecase::to_lower_camel_case(paste0("get_", nm, "_data"))
  filtersToApply <- list()
  for (k in seq_along(filters)) {
    type <- filters[[k]]$column_type
    filtersToApply[[type]] <- c(
      filtersToApply[[type]],
      rlang::set_names(x = filters[[k]]$column, nm = filters[[k]]$inputId)
    )
  }
  filtersText <- purrr::imap(filtersToApply, \(x, nm) {
    fun <- switch(nm,
                  "main" = 'dplyr::filter(',
                  "group" = 'omopgenerics::filterGroup(',
                  "strata" = 'omopgenerics::filterStrata(',
                  "additional" = 'omopgenerics::filterAdditional(',
                  "settings" = 'omopgenerics::filterSettings(')
    paste0(".data$", names(x), " %in% input$" x)
  }) |>
    paste0(collapse = " ")
  paste0(
    "## get ", nm, " data\n", funName, "() <- shiny::reactive({\n", data, "[['",
    nm, "']]", filtersText, "\n})"
  )
}
getDataName <- function(panelName) {
  snakecase::to_cam
}
getFunctionName <- function(panel)
