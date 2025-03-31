# to search icons see: https://fontawesome.com/search?ic=free
# predefined content ----
## tidy content ----
tidyContent <- list(
  title = "Tidy",
  output_type = "DT",
  render_content = "tidyDT(<filtered_data>, input$columns, input$pivot_estimates)",
  filters = list(
    columns = list(
      button_type = "pickerInput",
      label = "Columns",
      choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>", "variable_name", "variable_level"),
      selected = c("cdm_name", "<group>", "<strata>", "<additional>", "variable_name", "variable_level"),
      multiple = TRUE
    ),
    pivot_estimates = list(
      button_type = "checkbox",
      label = "Pivot estimates",
      value = TRUE
    )
  ),
  download = list(
    label = "Download csv",
    render = "<filtered_data> |>
    omopgenerics::tidy() |>
    readr::write_csv(file = file)",
    filename = "tidy_results.csv"
  )
)
## table content ----
## plot incidence content ----

# predefined filters ----
cdmFilter <- list(
  button_type = "pickerInput",
  label = "CDM name",
  column = "cdm_name",
  column_type = "main",
  choices = "choices$",
  selected = "selected$",
  multiple = TRUE
)

# predefined panels ----
## incidence ----
incidencePanel <- list(
  title = "Incidence",
  icon = "chart-line",
  data = list(result_type = "incidence"),
  automatic_filters = c("group", "strata", "additional", "settings", "variable_name", "estimate_name"),
  exclude_filters = "denominator_cohort_name",
  filters = list(cdm_name = cdmFilter),
  content = list(
    tidy = tidyContent,
    table = list(
      title = "table Incidence",
      output_type = "gt",
      render_content = "<filtered_data> |>
      IncidencePrevalence::plotIncidence(
      x = input$x,
      facet = input$facet,
      colour = input$colour
      )",
      filters = list(
        x = list(
          button_type = "pickerInput",
          label = "x axis",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("incidence_start_date"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("outcome_cohort_name"),
          multiple = TRUE
        )
      ),
      download = list(
        label = "Download table",
        filters = list(
          format = list(
            button_type = "pickerInput",
            label = "Format",
            choices = c("docx", "png", "pdf", "html"),
            selected = "docx",
            multiple = FALSE
          )
        ),
        render = "gt::gtsave(<render_content>, file)",
        filename = "paste0(\"table_incidence.\", input$download_format)"
      )
    )
  )
)
## prevalence ----
prevalencePanel <- list(
  title = "Prevalence",
  icon = "chart-column",
  data = list(result_type = "prevalence"),
  automatic_filters = c("group", "strata", "additional", "settings", "variable_name", "estimate_name"),
  exclude_filters = "denominator_cohort_name",
  filters = list(cdm_name = cdmFilter),
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
