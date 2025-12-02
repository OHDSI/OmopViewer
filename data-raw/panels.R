# to search icons see: https://fontawesome.com/search?ic=free
# predefined content ----
## tidy content ----
tidyContent <- list(
  title = "Tidy",
  output_type = "DT",
  reactive = "tidyDT(<filtered_data>, input$columns, input$pivot_estimates)",
  render = "<reactive_data>",
  filters = list(
    columns = list(
      button_type = "pickerInput",
      label = "Columns",
      choices = c(
        "cdm_name",
        "<group>",
        "<strata>",
        "<additional>",
        "<settings>",
        "variable_name",
        "variable_level"
      ),
      selected = c(
        "cdm_name",
        "<group>",
        "<strata>",
        "<additional>",
        "variable_name",
        "variable_level"
      ),
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
downloadPlot <- function(filename, type = "ggplot2") {
  if (type == "ggplot2") {
    fun <- "ggplot2::ggsave"
  } else if (type == "plotly") {
    fun <- "plotly::save_image"
  }
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
    render = paste0(fun, "(
      filename = file,
      plot = <reactive_data>,
      width = as.numeric(input$width),
      height = as.numeric(input$height),
      units = input$units,
      dpi = as.numeric(input$dpi)
    )"),
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
    render = "gt::gtsave(<reactive_data>, file)",
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
rankTableButton <- function(none = character(),
                            header = character(),
                            groupColumn = character(),
                            hide = character()) {
  list(
    none = list(
      button_type = "rank_list",
      text = "None",
      labels = none
    ),
    header = list(
      button_type = "rank_list",
      text = "Header",
      labels = header
    ),
    group_column = list(
      button_type = "rank_list",
      text = "Group columns",
      labels = groupColumn
    ),
    hide = list(
      button_type = "rank_list",
      text = "Hide",
      labels = hide
    )
  )
}

# predefined panels ----
## incidence ----
incidencePanel <- list(
  title = "Incidence",
  icon = "chart-line",
  data = list(result_type = "incidence"),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  exclude_filters = c("denominator_cohort_name", "incidence_end_date"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Incidence",
      output_type = "gt",
      reactive = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tableIncidence(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = omopgenerics::settingsColumns(res)
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<strata>",
          "incidence_start_date",
          "incidence_end_date",
          "denominator_age_group",
          "denominator_sex"
        ),
        header = "estimate_name",
        groupColumn = c("cdm_name", "outcome_cohort_name"),
        hide = c(
          "denominator_cohort_name",
          "analysis_interval",
          "analysis_censor_cohort_name",
          "analysis_complete_database_intervals",
          "analysis_outcome_washout",
          "analysis_repeated_events",
          "denominator_days_prior_observation",
          "denominator_end_date",
          "denominator_requirements_at_entry",
          "denominator_start_date",
          "denominator_target_cohort_name",
          "denominator_time_at_risk"
        )
      ),
      download = downloadGtTable("table_incidence")
    ),
    plot = list(
      title = "Plot Incidence",
      output_type = "ui",
      reactive = "<filtered_data> |>
      IncidencePrevalence::plotIncidence(
      x = input$x,
      facet = input$facet,
      colour = input$colour,
      ribbon = input$ribbon,
      line = input$line
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        x = list(
          button_type = "pickerInput",
          label = "x axis",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("incidence_start_date"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("outcome_cohort_name"),
          multiple = TRUE
        ),
        ribbon = list(
          button_type = "materialSwitch",
          label = "Ribbon",
          value = TRUE
        ),
        line = list(
          button_type = "materialSwitch",
          label = "Line",
          value = TRUE
        )
      ),
      download = downloadPlot("plot_incidence.png")
    ),
    plot_population = list(
      title = "Plot population",
      output_type = "ui",
      reactive = "<filtered_data> |>
      IncidencePrevalence::plotIncidencePopulation(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
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
      )
    )
  )
)
## prevalence ----
prevalencePanel <- list(
  title = "Prevalence",
  icon = "chart-column",
  data = list(result_type = "prevalence"),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  exclude_filters = "denominator_cohort_name",
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Prevalence",
      output_type = "gt",
      reactive = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tablePrevalence(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = omopgenerics::settingsColumns(res)
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<strata>",
          "prevalence_start_date",
          "prevalence_end_date",
          "denominator_age_group",
          "denominator_sex"
        ),
        header = "estimate_name",
        groupColumn = c("cdm_name", "outcome_cohort_name"),
        hide = c(
          "denominator_cohort_name",
          "analysis_interval",
          "analysis_complete_database_intervals",
          "analysis_full_contribution",
          "analysis_type",
          "denominator_days_prior_observation",
          "denominator_end_date",
          "denominator_requirements_at_entry",
          "denominator_start_date",
          "denominator_target_cohort_name",
          "denominator_time_at_risk"
        )
      ),
      download = downloadGtTable("table_prevalence")
    ),
    plot = list(
      title = "Plot Prevalence",
      output_type = "ui",
      reactive = "<filtered_data> |>
      IncidencePrevalence::plotPrevalence(
      x = input$x,
      facet = input$facet,
      colour = input$colour,
      ribbon = input$ribbon,
      line = input$line
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        x = list(
          button_type = "pickerInput",
          label = "x axis",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("prevalence_start_date"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("outcome_cohort_name"),
          multiple = TRUE
        ),
        ribbon = list(
          button_type = "materialSwitch",
          label = "Ribbon",
          value = TRUE
        ),
        line = list(
          button_type = "materialSwitch",
          label = "Line",
          value = TRUE
        )
      ),
      download = downloadPlot("plot_prevalence.png")
    ),
    plot_population = list(
      title = "Plot population",
      output_type = "ui",
      reactive = "<filtered_data> |>
      IncidencePrevalence::plotPrevalencePopulation(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
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
      )
    )
  )
)
## incidence attrition ----
incidenceAttritionPanel <- list(
  title = "Incidence Attrition",
  icon = "layer-group",
  data = list(result_type = "incidence_attrition"),
  automatic_filters = c("outcome_cohort_name", "strata", "settings", "variable_name"),
  exclude_filters = "reason",
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Incidence Attrition",
      output_type = "gt",
      reactive = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tableIncidenceAttrition(
      header = input$header,
      settingsColumn = omopgenerics::settingsColumns(res),
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("reason"),
        header = "variable_name",
        groupColumn = c("cdm_name", "outcome_cohort_name"),
        hide = c(
          "denominator_cohort_name",
          "estimate_name",
          "reason_id",
          "variable_level",
          "analysis_censor_cohort_name",
          "analysis_complete_database_intervals",
          "analysis_outcome_washout",
          "analysis_repeated_events",
          "denominator_age_group",
          "denominator_sex",
          "denominator_days_prior_observation",
          "denominator_end_date",
          "denominator_requirements_at_entry",
          "denominator_start_date",
          "denominator_target_cohort_name",
          "denominator_time_at_risk"
        )
      ),
      download = downloadGtTable("table_incidence_attrition")
    )
  )
)
## prevalence attrition ----
prevalenceAttritionPanel <- list(
  title = "Prevalence Attrition",
  icon = "layer-group",
  data = list(result_type = "prevalence_attrition"),
  automatic_filters = c("outcome_cohort_name", "strata", "settings", "variable_name"),
  exclude_filters = "reason",
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Prevalence Attrition",
      output_type = "gt",
      reactive = "res <- <filtered_data>
      res |>
      IncidencePrevalence::tablePrevalenceAttrition(
      header = input$header,
      settingsColumn = omopgenerics::settingsColumns(res),
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("<strata>"),
        header = "variable_name",
        groupColumn = c("cdm_name", "outcome_cohort_name"),
        hide = c(
          "denominator_cohort_name",
          "estimate_name",
          "reason_id",
          "variable_level",
          "analysis_interval",
          "analysis_complete_database_intervals",
          "analysis_full_contribution",
          "analysis_type",
          "denominator_age_group",
          "denominator_sex",
          "denominator_days_prior_observation",
          "denominator_end_date",
          "denominator_requirements_at_entry",
          "denominator_start_date",
          "denominator_target_cohort_name",
          "denominator_time_at_risk"
        )
      ),
      download = downloadGtTable("table_prevalence_attrition")
    )
  )
)
## summarise cohort overlap ----
cohortOverlapPanel <- list(
  title = "Cohort Overlap",
  icon = "circle-half-stroke",
  data = list(result_type = "summarise_cohort_overlap"),
  automatic_filters = c(
    "group",
    "strata",
    "variable_name",
    "estimate_name",
    "settings"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Overlap",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableCohortOverlap(
      uniqueCombinations = input$unique_combinations,
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = c(
        list(
          unique_combinations = list(
            button_type = "checkbox",
            label = "Unique combinations",
            value = TRUE
          )
        ),
        rankTableButton(
          none = c(
            "cohort_name_reference",
            "cohort_name_comparator",
            "<strata>",
            "estimate_name"
          ),
          header = "variable_name",
          groupColumn = "cdm_name",
          hide = c("variable_level", "overlap_by")
        )
      ),
      download = downloadGtTable("table_overlap")
    ),
    plot = list(
      title = "Plot Overlap",
      output_type = "ui",
      reactive = "<filtered_data> |>
      CohortCharacteristics::plotCohortOverlap(
      facet = input$facet
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name_reference",
            "cohort_name_comparator",
            "<strata>",
            "<settings>"
          ),
          selected = c("cdm_name", "cohort_name_reference"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_overlap.png")
    )
  )
)
## summarise cohort count ----
cohortCountPanel <- list(
  title = "Cohort Count",
  icon = "users",
  data = list(result_type = "summarise_cohort_count"),
  automatic_filters = c("group", "strata", "variable_name", "settings"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Counts",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableCohortCount(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("<strata>", "variable_name", "estimate_name"),
        groupColumn = "cdm_name",
        header = "cohort_name",
        hide = c("variable_level", "table_name")
      ),
      download = downloadGtTable("table_count")
    ),
    plot = list(
      title = "Plot Counts",
      output_type = "ui",
      reactive = "<filtered_data> |>
      CohortCharacteristics::plotCohortCount(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "cohort_name", "<strata>", "<settings>"),
          selected = c("cdm_name", "<strata>"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "cohort_name", "<strata>", "<settings>"),
          selected = c("cohort_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_count.png")
    )
  )
)
## summarise cohort attrition ----
cohortAttritionPanel <- list(
  title = "Cohort Attrition",
  icon = "layer-group",
  data = list(result_type = "summarise_cohort_attrition"),
  automatic_filters = c("cohort_name", "variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Attrition",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableCohortAttrition(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("reason"),
        groupColumn = c("cdm_name", "cohort_name"),
        header = "variable_name",
        hide = c("variable_level", "reason_id", "estimate_name", "<settings>")
      ),
      download = downloadGtTable("table_attrition")
    ),
    diagram = list(
      title = "Diagram",
      output_type = "grViz",
      reactive = "<filtered_data> |>
      CohortCharacteristics::plotCohortAttrition(
      show = input$show
      )",
      render = "<reactive_data>",
      filters = list(
        show = list(
          button_type = "pickerInput",
          label = "Show",
          choices = c("subjects", "records"),
          selected = c("subjects", "records"),
          multiple = TRUE
        )
      ),
      download = list(
        label = "Download Diagram",
        filters = list(
          width = list(
            button_type = "numericInput",
            label = "Width (px)",
            value = 2000
          )
        ),
        render = "svg <- DiagrammeRsvg::export_svg(<reactive_data>)
        rsvg::rsvg_png(charToRaw(svg), file, width = input$width)",
        filename = "attrition_diagram.png"
      )
    )
  )
)
## summarise cohort timing ----
cohortTimingPanel <- list(
  title = "Cohort Timing",
  icon = "chart-simple",
  data = list(result_type = "summarise_cohort_timing"),
  automatic_filters = c("group", "strata"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Timing",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableCohortTiming(
      timeScale = input$time_scale,
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = c(
        list(
          time_scale = list(
            button_type = "pickerInput",
            label = "Time scale",
            choices = c("days", "years"),
            selected = c("days"),
            multiple = FALSE
          )
        ),
        rankTableButton(
          none = c("<group>"),
          groupColumn = c("cdm_name"),
          header = "<strata>",
          hide = c("variable_level", "<settings>")
        )
      ),
      download = downloadGtTable("table_timing")
    ),
    plot = list(
      title = "Plot Timing",
      output_type = "ui",
      reactive = "<filtered_data> |>
      CohortCharacteristics::plotCohortTiming(
      plotType = input$plot_type,
      timeScale = input$time_scale,
      uniqueCombinations = input$unique_combinations,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("boxplot", "densityplot"),
          selected = c("boxplot"),
          multiple = FALSE
        ),
        time_scale = list(
          button_type = "pickerInput",
          label = "Time scale",
          choices = c("days", "years"),
          selected = c("days"),
          multiple = FALSE
        ),
        unique_combinations = list(
          button_type = "checkbox",
          label = "Unique combinations",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "<group>", "<strata>"),
          selected = c("cdm_name", "cohort_name_reference"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "<group>", "<strata>"),
          selected = c("cohort_name_comparator"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_timing.png")
    )
  )
)
## summarise characteristics ----
characteristicsPanel <- list(
  title = "Cohort Characteristics",
  icon = "users-gear",
  data = list(result_type = "summarise_characteristics"),
  automatic_filters = c("cohort_name", "strata", "variable_name", "estimate_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Characteristics",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableCharacteristics(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<strata>",
          "variable_name",
          "variable_level",
          "estimate_name"
        ),
        groupColumn = character(),
        header = c("cdm_name", "cohort_name"),
        hide = c("<additional>", "<settings>")
      ),
      download = downloadGtTable("table_characteristics")
    ),
    plot = list(
      title = "Plot Characteristics",
      output_type = "ui",
      reactive = "<filtered_data> |>
      CohortCharacteristics::plotCharacteristics(
      plotType = input$plot_type,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("boxplot", "barplot", "scatterplot"),
          selected = c("boxplot"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "cohort_name", "<strata>"),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "cohort_name", "<strata>"),
          selected = c("cohort_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_characteristics.png")
    )
  )
)
## summarise omop snapshot ----
snapshotPanel <- list(
  title = "Snapshot",
  icon = "camera",
  data = list(result_type = "summarise_omop_snapshot"),
  automatic_filters = c("variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Snapshot",
      output_type = "gt",
      reactive = "<filtered_data> |>
      OmopSketch::tableOmopSnapshot()",
      render = "<reactive_data>",
      download = downloadGtTable("table_snapshot")
    )
  ),
  report = list(
    title = "Database metadata",
    list(
      caption = "Database metadata.",
      type = "table",
      content = "OmopSketch::tableOmopSnapshot(<data>)"
    )
  )
)
## summarise observation period ----
observationPeriodPanel <- list(
  title = "Observation period Summary",
  icon = "eye",
  data = list(result_type = "summarise_observation_period"),
  automatic_filters = c(
    "observation_period_ordinal",
    "strata",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Observation period",
      output_type = "gt",
      reactive = "<filtered_data> |>
      OmopSketch::tableObservationPeriod()",
      render = "<reactive_data>",
      download = downloadGtTable("table_obsevation_period")
    ),
    plot = list(
      title = "Plot Observation period",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::plotObservationPeriod(
      variableName = input$variable,
      plotType = input$plot_type,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        variable = list(
          button_type = "pickerInput",
          label = "Variable",
          choices = c(
            "Number subjects",
            "Records per person",
            "Duration in days",
            "Days to next observation period"
          ),
          selected = c("Number subjects"),
          multiple = FALSE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("boxplot", "barplot", "densityplot"),
          selected = c("barplot"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices =  c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("observation_period_ordinal"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_observation_period.png")
    )
  )
)
## summarise clinical records ----
clinicalRecordsPanel <- list(
  title = "Clinical Tables Summary",
  icon = "bars-staggered",
  data = list(result_type = "summarise_clinical_records"),
  automatic_filters = c(
    "group",
    "strata",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Clinical records",
      output_type = "gt",
      reactive = "<filtered_data> |>
      OmopSketch::tableClinicalRecords()",
      render = "<reactive_data>",
      download = downloadGtTable("table_clinical_records")
    )
  )
)
## summarise person ----
personPanel <- list(
  title = "Person Table Summary",
  icon = "person",
  data = list(result_type = "summarise_person"),
  automatic_filters = c(
    "group",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Person",
      output_type = "gt",
      reactive = "<filtered_data> |>
      OmopSketch::tablePerson()",
      render = "<reactive_data>",
      download = downloadGtTable("table_person")
    ),
    plot = list(
      title = "Plot Person",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::plotPerson(
      variableName = input$variable
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        variable = list(
          button_type = "pickerInput",
          label = "Variable",
          choices =  c(
            "Number subjects",
            "Number subjects not in observation",
            "Sex",
            "Sex source",
            "Race",
            "Race source",
            "Ethnicity",
            "Ethnicity source",
            "Year of birth",
            "Month of birth",
            "Day of birth",
            "Location",
            "Provider",
            "Care site"
          ),
          selected = c("Number subjects"),
          multiple = FALSE
        )
      ),
      download = downloadPlot("plot_person.png")
    )
  )
)
## summarise record count ----
recordCountPanel <- list(
  title = "Record count",
  icon = "signal",
  data = list(result_type = "summarise_record_count"),
  automatic_filters = c(
    "group",
    "strata",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    tidy = tidyContent,
    plot = list(
      title = "Plot record count",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::plotRecordCount(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("omop_table"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_record_count.png")
    )
  )
)
## summarise missing data ----
missingPanel <- list(
  title = "Missing data",
  icon = "circle-exclamation",
  data = list(result_type = "summarise_missing_data"),
  automatic_filters = c(
    "group",
    "strata",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Missing data",
      output_type = "gt",
      reactive = "<filtered_data> |>
      OmopSketch::tableMissingData()",
      render = "<reactive_data>",
      download = downloadGtTable("table_missing_data")
    )
  )
)
## summarise in observation ----
inObservationPanel <- list(
  title = "In Observation",
  icon = "explosion",
  data = list(result_type = "summarise_in_observation"),
  automatic_filters = c(
    "group",
    "strata",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    tidy = tidyContent,
    plot = list(
      title = "Plot in observation",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::plotInObservation(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = character(),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_in_observation.png")
    )
  )
)
## summarise trend ----
trendPanel <- list(
  title = "Trends",
  icon = "arrow-trend-up",
  data = list(result_type = "summarise_trend"),
  automatic_filters = c(
    "group",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Trends",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      OmopSketch::tableTrend(type = 'reactable')",
      render = "<reactive_data>",
      download = list(
        label = "Download csv",
        render = "<reactive_data> |>
    readr::write_csv(file = file)",
        filename = "trends.csv"
      )
    ),
    plot = list(
      title = "Plot Trends",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::plotTrend(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cdm_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = character(),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_trend.png")
    )
  )
)
conceptCountsPanel <- list(
  title = "Concept Counts",
  icon = "database",
  data = list(result_type = "summarise_concept_id_counts"),
  automatic_filters = c(
    "group",
    "strata",
    "strata",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    tidy = list(
      title = "Table Concept Counts",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      OmopSketch::tableConceptIdCounts(display = input$display)",
      render = "<reactive_data>",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        display = list(
          button_type = "pickerInput",
          label = "Display",
          choices = c(
            "overall",
            "standard",
            "source",
            "missing standard",
            "missing source"
          ),
          selected = "overall",
          multiple = FALSE
        )
      ),
      download = list(
        label = "Download csv",
        render = "<reactive_data> |>
    readr::write_csv(file = file)",
        filename = "concept_id_counts.csv"
      )
    ),
    table = list(
      title = "Top Concept Counts",
      output_type = "ui",
      reactive = "<filtered_data> |>
      OmopSketch::tableTopConceptCounts(top = as.numeric(input$top))",
      render = "<reactive_data>",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        top = list(
          button_type = "pickerInput",
          label = "Top",
          choices = c(
            5L,
            10L,
            50L,
            100L
          ),
          selected = 10L,
          multiple = FALSE
        )
      ),
      download = downloadGtTable("table_top_concept_counts")
    )
  )
)
## orphan code use ----
orphanCodesPanel <- list(
  title = "Orphan codes",
  icon = "magnifying-glass-arrow-right",
  data = list(result_type = "orphan_code_use"),
  automatic_filters = c(
    "codelist_name",
    "domain_id",
    "standard_concept",
    "vocabulary_id"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_react = list(
      title = "Table Orphan codes (reactable)",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableOrphanCodes(
        type = \"reactable\",
        header = \"cdm_name\"
      )",
      render = "<reactive_data>"
    ),
    table_gt = list(
      title = "Table Orphan codes (gt)",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableOrphanCodes(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "codelist_name",
          "domain_id",
          "variable_name",
          "variable_level",
          "standard_concept",
          "vocabulary_id"
        ),
        groupColumn = character(),
        header = c("cdm_name", "estimate_name"),
        hide = character()
      ),
      download = downloadGtTable("table_orphan_codes")
    )
  )
)
## cohort code use ----
cohortCodeUsePanel <- list(
  title = "Cohort code use",
  icon = "chart-column",
  data = list(result_type = "cohort_code_use"),
  automatic_filters = c("group", "strata", "variable_name", "estimate_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_react = list(
      title = "Table Cohort code use (reactable)",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableCohortCodeUse(
        type = \"reactable\",
        header = \"cdm_name\"
      )",
      render = "<reactive_data>"
    ),
    table_gt = list(
      title = "Table Cohort code use (gt)",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableCohortCodeUse(
      timing = TRUE,
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<group>",
          "<strata>",
          "<additional>",
          "variable_name",
          "variable_level"
        ),
        groupColumn = character(),
        header = c("cdm_name", "estimate_name"),
        hide = character()
      ),
      download = downloadGtTable("table_cohort_code_use")
    )
  )
)
## code use ----
codeUsePanel <- list(
  title = "Code use",
  icon = "chart-column",
  data = list(result_type = "code_use"),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_react = list(
      title = "Table Code use (reactable)",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableCodeUse(
        type = \"reactable\",
        header = \"cdm_name\"
      )",
      render = "<reactive_data>"
    ),
    table_gt = list(
      title = "Table Code use (gt)",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableCodeUse(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<group>",
          "<strata>",
          "<additional>",
          "<settings>",
          "variable_name"
        ),
        groupColumn = character(),
        header = c("cdm_name", "estimate_name"),
        hide = character()
      ),
      download = downloadGtTable("table_code_use")
    )
  )
)
## achilles code use ----
achillesCodeUsePanel <- list(
  title = "Achilles code use",
  icon = "chart-column",
  data = list(result_type = "achilles_code_use"),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_react = list(
      title = "Table Achilles code use (reactable)",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableAchillesCodeUse(
        type = \"reactable\",
        header = \"cdm_name\"
      )",
      render = "<reactive_data>"
    ),
    table_gt = list(
      title = "Table Achilles code use (gt)",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableAchillesCodeUse(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<group>",
          "<strata>",
          "<additional>",
          "<settings>",
          "variable_name"
        ),
        groupColumn = character(),
        header = c("cdm_name", "estimate_name"),
        hide = character()
      ),
      download = downloadGtTable("table_achilles_code_use")
    )
  )
)
## unmapped codes ----
unmappedPanel <- list(
  title = "Unmapped codes",
  icon = "chart-column",
  data = list(result_type = "unmapped_codes"),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "settings",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_react = list(
      title = "Table Unmapped codes (reactable)",
      output_type = "reactable",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableUnmappedCodes(
        type = \"reactable\",
        header = \"cdm_name\"
      )",
      render = "<reactive_data>"
    ),
    table_gt = list(
      title = "Table Unmapped codes (gt)",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CodelistGenerator::tableUnmappedCodes(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "<group>",
          "<strata>",
          "<additional>",
          "<settings>",
          "variable_name"
        ),
        groupColumn = character(),
        header = c("cdm_name", "estimate_name"),
        hide = character()
      ),
      download = downloadGtTable("table_unmapped_codes")
    )
  )
)
## dose coverage ----
doseCoveragePanel <- list(
  title = "Dose coverage",
  icon = "pills",
  data = list(result_type = "summarise_dose_coverage"),
  automatic_filters = c(
    "ingredient_name",
    "unit",
    "route",
    "pattern_id",
    "variable_name",
    "estimate_name"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Dose coverage",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableDoseCoverage(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("unit", "route", "pattern_id"),
        header = c("variable_name", "estimate_name"),
        groupColumn = c("cdm_name", "ingredient_name"),
        hide = c("variable_level", "<settings>")
      ),
      download = downloadGtTable("table_dose_coverage")
    )
  )
)
## proportion of patients covered ----
ppcPanel <- list(
  title = "Proportion of patients covered",
  icon = "chart-gantt",
  data = list(result_type = "summarise_proportion_of_patients_covered"),
  automatic_filters = c("cohort_name", "strata", "estimate_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table PPC",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableProportionOfPatientsCovered(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("time", "estimate_name"),
        header = c("cohort_name", "<strata>"),
        groupColumn = c("cdm_name"),
        hide = c("variable_name", "variable_level", "<settings>")
      ),
      download = downloadGtTable("table_ppc")
    ),
    plot = list(
      title = "Plot PPC",
      output_type = "ui",
      reactive = "<filtered_data> |>
      DrugUtilisation::plotProportionOfPatientsCovered(
      ribbon = input$ribbon,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        ribbon = list(
          button_type = "checkbox",
          label = "Ribbon",
          value = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "cohort_name", "<strata>"),
          selected = c("cohort_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "cohort_name", "<strata>"),
          selected = c("<strata>"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_ppc.png")
    )
  )
)
## summarise drug restart ----
drugRestartPanel <- list(
  title = "Drug Restart",
  icon = "chart-gantt",
  data = list(result_type = "summarise_drug_restart"),
  automatic_filters = c("cohort_name", "strata", "estimate_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Drug Restart",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableDrugRestart(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "variable_level",
          "<strata>",
          "follow_up_days",
          "estimate_name"
        ),
        header = c("cdm_name", "cohort_name"),
        groupColumn = c("variable_name"),
        hide = c("<settings>")
      ),
      download = downloadGtTable("table_drug_restart")
    ),
    plot = list(
      title = "Plot Drug Restart",
      output_type = "ui",
      reactive = "<filtered_data> |>
      DrugUtilisation::plotDrugRestart(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "variable_name",
            "variable_level",
            "follow_up_days",
            "<settings>"
          ),
          selected = c("cdm_name", "cohort_name", "follow_up_days", "<strata>"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "variable_name",
            "variable_level",
            "follow_up_days",
            "<settings>"
          ),
          selected = c("variable_level"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_drug_restart.png")
    )
  )
)
## summarise drug utilisation ----
dusPanel <- list(
  title = "Drug Utilisation",
  icon = "capsules",
  data = list(result_type = "summarise_drug_utilisation"),
  automatic_filters = c(
    "cohort_name",
    "strata",
    "additional",
    "variable_name",
    "estimate_name",
    "settings"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Drug Utilisation",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableDrugUtilisation(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("concept_set", "ingredient", "variable_name", "estimate_name"),
        header = c("cdm_name"),
        groupColumn = c("cohort_name", "<strata>"),
        hide = c("variable_level", "<settings>")
      ),
      download = downloadGtTable("table_drug_utilisation")
    ),
    plot = list(
      title = "Plot Drug Utilisation",
      output_type = "ui",
      reactive = "<filtered_data> |>
      DrugUtilisation::plotDrugUtilisation(
      variable = input$variable,
      plotType = input$plot_type,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        variable = list(
          button_type = "pickerInput",
          label = "Variable",
          choices = "<variable_name>",
          selected = character(),
          multiple = FALSE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("scatterplot", "barplot", "densityplot", "boxplot"),
          selected = "barplot"
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("<strata>"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "<additional>",
            "<settings>"
          ),
          selected = c("cohort_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_drug_utilisation.png")
    )
  )
)
## indication ----
indicationPanel <- list(
  title = "Indication",
  icon = "disease",
  data = list(result_type = "summarise_indication"),
  automatic_filters = c(
    "cohort_name",
    "strata",
    "additional",
    "variable_name",
    "estimate_name",
    "mutually_exclusive",
    "index_date"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Indication",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableIndication(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("variable_level", "estimate_name"),
        header = c("cdm_name", "cohort_name", "<strata>"),
        groupColumn = c("variable_name"),
        hide = c("window_name", "<settings>")
      ),
      download = downloadGtTable("table_indication")
    ),
    plot = list(
      title = "Plot Indication",
      output_type = "ui",
      reactive = "<filtered_data> |>
      DrugUtilisation::plotIndication(
      facet = input$facet,
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "window_name",
            "<settings>"
          ),
          selected = c("cdm_name", "cohort_name", "<strata>", "window_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_indication.png")
    )
  )
)
## treatment ----
treatmentPanel <- list(
  title = "Treatments",
  icon = "disease",
  data = list(result_type = "summarise_treatment"),
  automatic_filters = c(
    "cohort_name",
    "strata",
    "additional",
    "variable_name",
    "estimate_name",
    "mutually_exclusive",
    "index_date"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Treatments",
      output_type = "gt",
      reactive = "<filtered_data> |>
      DrugUtilisation::tableTreatment(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("variable_level", "estimate_name"),
        header = c("cdm_name", "cohort_name", "<strata>"),
        groupColumn = c("variable_name"),
        hide = c("window_name", "<settings>")
      ),
      download = downloadGtTable("table_treatment")
    ),
    plot = list(
      title = "Plot Treatment",
      output_type = "ui",
      reactive = "<filtered_data> |>
      DrugUtilisation::plotTreatment(
      facet = input$facet
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "window_name",
            "<settings>"
          ),
          selected = c("cdm_name", "cohort_name", "<strata>", "window_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_treatment.png")
    )
  )
)
## summarise large scale characteristics ----
lscPanel <- list(
  title = "Large Scale Characteristics",
  icon = "arrow-up-right-dots",
  data = list(result_type = "summarise_large_scale_characteristics"),
  automatic_filters = c("group", "strata", "variable_level", "settings"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table_lsc = list(
      title = "Table",
      output_type = "reactable",
      reactive = "if (identical(input$compare_by, 'no compare')) {
      cb <- NULL
      } else {
      cb <- input$compare_by
      }
      if (identical(input$smd_reference, 'no SMD')) {
      sr <- NULL
      } else {
      sr <- input$smd_reference
      }
      <filtered_data> |>
      CohortCharacteristics::tableLargeScaleCharacteristics(
      compareBy = cb,
      hide = input$hide,
      smdReference = sr
      )",
      render = "<reactive_data>",
      observe = "shiny::observeEvent(input$compare_by,{
        opts <- values[[paste0('<panel>_', input$compare_by)]]
        opts <- c('no SMD', opts)
        shinyWidgets::updatePickerInput(
          inputId = '<prefix>_smd_reference',
          choices = opts,
          selected = 'no SMD'
        )
      })",
      filters = list(
        compare_by = list(
          button_type = "pickerInput",
          label = "Compare by",
          choices = c(
            "no compare",
            "cdm_name",
            "cohort_name",
            "<strata>",
            "type",
            "variable_level"
          ),
          selected = c("no compare"),
          multiple = FALSE
        ),
        hide = list(
          button_type = "pickerInput",
          label = "Hide",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "type",
            "variable_level"
          ),
          selected = c("type"),
          multiple = TRUE
        ),
        smd_reference = list(
          button_type = "pickerInput",
          label = "SMD reference",
          choices = c("no SMD"),
          selected = c("no SMD"),
          multiple = FALSE
        )
      ),
      download = list(
        label = "Download table",
        render = "rt <- <reactive_data>
        rt$x$tag$attribs$data |>
        unclass() |>
        jsonlite::fromJSON() |>
        dplyr::as_tibble() |>
        readr::write_csv(file)",
        filename = "smd_results.csv"
      )
    ),
    table_most_common = list(
      title = "Most common codes",
      output_type = "gt",
      reactive = "<filtered_data> |>
      CohortCharacteristics::tableTopLargeScaleCharacteristics(
      topConcepts = input$top_concepts,
      type = 'gt'
      )",
      render = "<reactive_data>",
      filters = list(
        top_concepts = list(
          button_type = "pickerInput",
          label = "Top concepts",
          choices = c(10L, 25L, 100L),
          selected = c(10L),
          multiple = FALSE
        )
      ),
      download = downloadGtTable("top_concepts")
    ),
    plot_compared = list(
      title = "Plot Compared",
      output_type = "plotly",
      reactive = "if (input$missings) {
      mis <- 0
      } else {
      mis <- NULL
      }
      <filtered_data> |>
      CohortCharacteristics::plotComparedLargeScaleCharacteristics(
      colour = input$colour,
      reference = input$reference,
      facet = input$facet,
      missings = mis
      )",
      render = "<reactive_data>",
      observe = "shiny::observeEvent(input$colour,{
        opts <- values[[paste0('<panel>_', input$colour)]]
        shinyWidgets::updatePickerInput(
          inputId = '<prefix>_reference',
          choices = opts,
          selected = opts[1]
        )
      })",
      filters = list(
        missings = list(
          button_type = "Toggle.shinyInput",
          label = "Missing data",
          onText = "Interpolate 0",
          offText = "Eliminate",
          value = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "type",
            "variable_level"
          ),
          selected = NULL,
          multiple = FALSE
        ),
        reference = list(
          button_type = "pickerInput",
          label = "Reference",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_name",
            "<strata>",
            "type",
            "variable_level"
          ),
          selected = c("cdm_name", "cohort_name", "<strata>"),
          multiple = TRUE
        )
      ),
      download = NULL
    )
  )
)
## measurement timing ----
measurementTimingPanel <- list(
  title = "Measurement timing",
  icon = "timeline",
  data = list(result_type = "measurement_timings"),
  automatic_filters = c("group", "strata", "timing", "variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Measurement Timing",
      output_type = "gt",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::tableMeasurementTimings(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = 'timing'
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("variable_name", "estimate_name", "<settings>"),
        header = "<strata>",
        groupColumn = c("codelist_name", "cdm_name"),
        hide = c("variable_level")
      ),
      download = downloadGtTable("table_measurement_timing")
    ),
    plot = list(
      title = "Plot Measurement Timing",
      output_type = "ui",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::plotMeasurementTimings(
      plotType = input$plot_type,
      timeScale = input$time_scale,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("boxplot", "densityplot"),
          selected = c("boxplot"),
          multiple = FALSE
        ),
        time_scale = list(
          button_type = "pickerInput",
          label = "Time scale",
          choices = c("days", "years"),
          selected = c("days"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "codelist_name", "<strata>", "<settings>"),
          selected = c("<strata>"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "codelist_name", "<strata>", "<settings>"),
          selected = c("cdm_name", "codelist_name"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_measurement_timing.png")
    )
  )
)
## measurement value as numeric ----
measurementNumericPanel <- list(
  title = "Measurement as numeric",
  icon = "gauge-high",
  data = list(result_type = "measurement_value_as_numeric"),
  automatic_filters = c("group", "strata", "timing", "variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Measurement Numeric",
      output_type = "gt",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::tableMeasurementValueAsNumeric(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = 'timing'
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("<group>", "<settings>", "estimate_name"),
        header = "<strata>",
        groupColumn = c( "cdm_name"),
        hide = c("variable_name", "variable_level")
      ),
      download = downloadGtTable("table_measurement_numeric")
    ),
    plot = list(
      title = "Plot Measurement Numeric",
      output_type = "ui",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::plotMeasurementValueAsNumeric(
      x = input$x,
      plotType = input$plot_type,
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        x = list(
          button_type = "pickerInput",
          label = "X coordinate",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("unit_concept_name"),
          multiple = TRUE
        ),
        plot_type = list(
          button_type = "pickerInput",
          label = "Plot type",
          choices = c("boxplot", "densityplot"),
          selected = c("boxplot"),
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("codelist_name", "concept_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "<settings>"),
          selected = c("cdm_name", "unit_concept_name", "<strata>"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_measurement_as_numeric.png")
    )
  )
)
## measurement value as concept ----
measurementConceptPanel <- list(
  title = "Measurement as concept",
  icon = "cubes",
  data = list(result_type = "measurement_value_as_concept"),
  automatic_filters = c("group", "strata", "timing", "variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Measurement Concept",
      output_type = "gt",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::tableMeasurementValueAsConcept(
      header = input$header,
      groupColumn = input$group_column,
      hide = input$hide,
      settingsColumn = 'timing'
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c("<group>", "<settings>", "estimate_name"),
        header = "<strata>",
        groupColumn = c( "cdm_name"),
        hide = c("variable_name", "variable_level")
      ),
      download = downloadGtTable("table_measurement_concept")
    ),
    plot = list(
      title = "Plot Measurement Concept",
      output_type = "ui",
      reactive = "<filtered_data> |>
      MeasurementDiagnostics::plotMeasurementValueAsConcept(
      facet = input$facet,
      colour = input$colour
      )",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "variable_name", "variable_level", "<settings>"),
          selected = c("codelist_name", "concept_name"),
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c("cdm_name", "<group>", "<strata>", "<additional>", "variable_name", "variable_level", "<settings>"),
          selected = c("concept_name", "variable_level", "<strata>"),
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_measurement_as_numeric.png")
    )
  )
)
## logs ----
logsPanel <- list(
  title = "Logs",
  icon = "clipboard-list",
  data = list(result_type = "summarise_log_file"),
  automatic_filters = c("variable_name"),
  filters = list(cdm_name = cdmFilter),
  content = list(
    table = list(
      title = "Table Logs",
      output_type = "gt",
      reactive = "<filtered_data> |>
        dplyr::filter(.data$estimate_name == 'date_time') |>
        visOmopResults::visOmopTable(
          header = 'cdm_name',
          hide = c('variable_level', 'estimate_name')
        )
      ",
      render = "<reactive_data>",
      download = downloadGtTable("table_log")
    ),
    plot = list(
      title = "Plot Timing",
      output_type = "ui",
      reactive = "<filtered_data> |>
        dplyr::filter(.data$estimate_name == 'elapsed_time') |>
        visOmopResults::barPlot(
          x = c('log_id', 'variable_name'),
          y = 'elapsed_time',
          colour = 'cdm_name',
          position = input$position
        )
      ",
      render = "x <- <reactive_data>
      renderInteractivePlot(x, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        position = list(
          button_type = "pickerInput",
          label = "Columns",
          choices = c("dodge", "stack"),
          selected = "stack",
          multiple = FALSE
        )
      ),
      download = downloadPlot("plot_logs.png")
    )
  )
)
## deafult ----
defaultPanel <- list(
  title = "<result_type>",
  icon = "folder",
  data = list(),
  automatic_filters = c(
    "group",
    "strata",
    "additional",
    "variable_name",
    "estimate_name",
    "settings"
  ),
  filters = list(cdm_name = cdmFilter),
  content = list(
    tidy = tidyContent,
    table = list(
      title = "Table",
      output_type = "gt",
      reactive = "<filtered_data> |>
      simpleTable(
      header = input$header,
      group = input$group_column,
      hide = input$hide
      )",
      render = "<reactive_data>",
      filters = rankTableButton(
        none = c(
          "cdm_name",
          "<group>",
          "<strata>",
          "<additional>",
          "<settings>",
          "variable_name",
          "variable_level",
          "estimate_name"
        ),
        header = character(),
        groupColumn = character(),
        hide = character()
      ),
      download = downloadGtTable("table")
    )
  )
)

# all panels ----
omopViewerPanels <- list(
  # OmopSketch
  summarise_omop_snapshot = snapshotPanel,
  summarise_observation_period = observationPeriodPanel,
  summarise_clinical_records = clinicalRecordsPanel,
  summarise_record_count = recordCountPanel,
  summarise_missing_data = missingPanel,
  summarise_in_observation = inObservationPanel,
  summarise_trend = trendPanel,
  summarise_concept_id_counts = conceptCountsPanel,
  summarise_person = personPanel,
  # CodelistGenerator
  orphan_code_use = orphanCodesPanel,
  cohort_code_use = cohortCodeUsePanel,
  code_use = codeUsePanel,
  achilles_code_use = achillesCodeUsePanel,
  unmapped_codes = unmappedPanel,
  # CohortCharacteristics
  summarise_cohort_overlap = cohortOverlapPanel,
  summarise_cohort_count = cohortCountPanel,
  summarise_cohort_attrition = cohortAttritionPanel,
  summarise_cohort_timing = cohortTimingPanel,
  summarise_characteristics = characteristicsPanel,
  summarise_large_scale_characteristics = lscPanel,
  # IncidencePrevalence
  incidence = incidencePanel,
  incidence_attrition = incidenceAttritionPanel,
  prevalence = prevalencePanel,
  prevalence_attrition = prevalenceAttritionPanel,
  # DrugUtilisation
  summarise_dose_coverage = doseCoveragePanel,
  summarise_proportion_of_patients_covered = ppcPanel,
  summarise_drug_restart = drugRestartPanel,
  summarise_drug_utilisation = dusPanel,
  summarise_indication = indicationPanel,
  summarise_treatment = treatmentPanel,
  # MeasurementDiagnostics
  measurement_timings = measurementTimingPanel,
  measurement_value_as_numeric = measurementNumericPanel,
  measurement_value_as_concept = measurementConceptPanel,
  # omopgenerics
  summarise_log_file = logsPanel,
  # default
  default = defaultPanel
) |>
  purrr::map(\(x) newOmopViewerPanel(x))

# exported panels ----
usethis::use_data(omopViewerPanels, overwrite = TRUE, internal = FALSE)
