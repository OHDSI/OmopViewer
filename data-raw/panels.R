# to search icons see: https://fontawesome.com/search?ic=free
# predefined content ----
## tidy content ----
tidyContent <- list(
  title = "Tidy",
  output_type = "DT",
  render_content = "tidyDT(x, input$columns, input$pivot_estimates)",
  filters = list(
    columns = list(
      button_type = "pickerInput",
      label = "\"Columns\"",
      choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
      selected = c("cdm_name", "<group>", "<strata>", "<additional>"),
      multiple = FALSE
    ),
    pivot_estimates = list(
      button_type = "checkbox",
      label = "\"Pivot estimates\"",
      value = TRUE
    )
  ),
  download = list(
    label = "Download csv",
    render = "",
    filename = ""
  )
)
## table content ----
## plot incidence content ----

# predefined panels ----
## incidence ----
incidencePanel <- list(
  title = "Incidence",
  icon = "chart-line",
  data = list(result_type = "incidence"),
  filters = list(
    cdm_name = list(
      button_type = "pickerInput",
      column = "cdm_name",
      column_type = "main",
      choices = "choices$",
      selected = "selected$",
      multiple = TRUE
    )
  ),
  content = list(tidy = tidyContent)
)
## prevalence ----
prevalencePanel <- list(
  title = "Prevalence",
  icon = "chart-column",
  data = list(result_type = "prevalence"),
  filters = list(),
  content = list(tidy = tidyContent)
)
## all panels ----
omopViewerPanels <- list(
  incidence = incidencePanel,
  prevalence = prevalencePanel
) |>
  purrr::map(\(x) newOmopViewerPanel(x))

# exported panels ----
usethis::use_data(omopViewerPanels, overwrite = TRUE, internal = FALSE)
