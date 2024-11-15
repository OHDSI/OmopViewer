
createServer <- function(panelDetails, data) {
  c(
    downloadRawDataServer(data),
    purrr::imap_chr(panelDetails, \(x, nm) {
      c(glue::glue("# {nm} -----"),
        glue::glue("## tidy {nm} -----"),
        tidyServer(nm, data),
        glue::glue("## output {nm} -----"),
        outputServer(nm, x$output_id, data),
        "\n"
      ) |>
        paste0(collapse = "\n")
    })
  )
}
