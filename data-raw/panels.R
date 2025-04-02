# to search icons see: https://fontawesome.com/search?ic=free
# predefined content ----
## tidy content ----
tidyContent <- list(
  title = "Tidy",
  output_type = "DT",
  render = "tidyDT(<filtered_data>, input$columns, input$pivot_estimates)",
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

## download prebuilt ----
downloadPlot <- function(filename) {
  list(
    label = "Download plot",
    filters = list(
      width = list(
        button_type = "numericInput",
        label = "Width",
        value = 15
      ),
      height = list(
        button_type = "numericInput",
        label = "Height",
        value = 15
      ),
      units = list(
        button_type = "pickerInput",
        label = "Units",
        selected = "cm",
        choices = c("px", "cm", "inch"),
        multiple = FALSE
      ),
      dpi = list(
        button_type = "numericInput",
        label = "DPI",
        value = 300
      )
    ),
    render = "plt <- <rendered_data>
        ggplot2::ggsave(
          filename = file,
          plot = plt,
          width = as.numeric(input$width),
          height = as.numeric(input$height),
          units = input$units,
          dpi = as.numeric(input$dpi)
        )",
    filename = filename
  )
}
downloadGtTable <- function(filename) {
  list(
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
    render = "gt::gtsave(<rendered_data>, file)",
    filename = paste0("paste0(\"", filename, ".\", input$format)")
  )
}

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
      title = "Table Incidence",
      output_type = "gt",
      render = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tableIncidence(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = omopgenerics::settingsColumns(res)
      )",
      filters = list(
        none = list(
          button_type = "rank_list",
          text = "None",
          labels = c("<strata>", "incidence_start_date", "incidence_end_date", "denominator_age_group", "denominator_sex")
        ),
        header = list(
          button_type = "rank_list",
          text = "Header",
          labels = "estimate_name"
        ),
        group_column = list(
          button_type = "rank_list",
          text = "Group columns",
          labels = c("cdm_name", "outcome_cohort_name")
        ),
        hide = list(
          button_type = "rank_list",
          text = "Hide",
          labels = c("denominator_cohort_name", "analysis_interval", "analysis_censor_cohort_name", "analysis_complete_database_intervals", "analysis_outcome_washout", "analysis_repeated_events", "denominator_days_prior_observation", "denominator_end_date", "denominator_requirements_at_entry", "denominator_start_date", "denominator_target_cohort_name", "denominator_time_at_risk")
        )
      ),
      download = downloadGtTable("table_incidence")
    ),
    plot = list(
      title = "Plot Incidence",
      output_type = "plot",
      render = "<filtered_data> |>
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
      download = downloadPlot("plot_incidence.png")
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
  content = list(
    tidy = tidyContent,
    table = list(
      title = "Table Prevalence",
      output_type = "gt",
      render = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tablePrevalence(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = omopgenerics::settingsColumns(res)
      )",
      filters = list(
        none = list(
          button_type = "rank_list",
          text = "None",
          labels = c("<strata>", "prevalence_start_date", "prevalence_end_date", "denominator_age_group", "denominator_sex")
        ),
        header = list(
          button_type = "rank_list",
          text = "Header",
          labels = "estimate_name"
        ),
        group_column = list(
          button_type = "rank_list",
          text = "Group columns",
          labels = c("cdm_name", "outcome_cohort_name")
        ),
        hide = list(
          button_type = "rank_list",
          text = "Hide",
          labels = c("denominator_cohort_name", "analysis_interval", "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_type", "denominator_days_prior_observation", "denominator_end_date", "denominator_requirements_at_entry", "denominator_start_date", "denominator_target_cohort_name", "denominator_time_at_risk")
        )
      ),
      download = downloadGtTable("table_prevalence")
    ),
    plot = list(
      title = "Plot Prevalence",
      output_type = "plot",
      render = "<filtered_data> |>
      IncidencePrevalence::plotPrevalence(
      x = input$x,
      facet = input$facet,
      colour = input$colour
      )",
      filters = list(
        x = list(
          button_type = "pickerInput",
          label = "x axis",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("prevalence_start_date"),
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
      download = downloadPlot("plot_prevalence.png")
    )
  )
)
## all panels ----
omopViewerPanels <- list(
  incidence = incidencePanel,
  prevalence = prevalencePanel
) |>
  purrr::map(\(x) newOmopViewerPanel(x))

# exported panels ----
usethis::use_data(omopViewerPanels, overwrite = TRUE, internal = FALSE)
