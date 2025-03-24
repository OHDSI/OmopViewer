# to search icons see: https://fontawesome.com/search?ic=free
# predefined content ----
## tidy content ----
tidyContent <- list(
  title = "Tidy",
  output_type = "DT",
  render_content = "tidyDT(x, input$columns, input$pivot_estimates)",
  sidebar = list(
    columns = list(
      button_type = "pickerInput",
      label = "Columns",
      choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
      selected = c("cdm_name", "<group>", "<strata>", "<additional>"),
      multiple = FALSE
    ),
    pivot_estimates = list(
      button_type = "checkbox",
      label = "Pivot estimates",
      value = TRUE
    )
  )
)
## table content ----
## plot incidence content ----

# predefined panels ----
## incidence ----
incidencePanel <- list(
  result_type = "incidence",
  title = "Incidence",
  icon = "chart-line",
  filters = list(
    cdm_name = list(
      button_type = "pickerInput",
      column = "cdm_name",
      choices = "choices$",
      selected = "selected$",
      multiple = TRUE
    )
  ),
  content = list(tidy = tidyContent)
)
## prevalence ----
prevalencePanel <- list(
  result_type = "prevalence",
  title = "Prevalence",
  icon = "chart-column",
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
