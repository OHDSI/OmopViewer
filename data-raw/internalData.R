
# you need to load the package before running these lines of code as styleCode()
# is required.

# to search icons see: https://fontawesome.com/search?m=free&o=r

omopViewerTabs <- dplyr::tribble(
  ~result_tab_id, ~result_type, ~package, ~title, ~icon, ~information,
  1L, "summarise_cohort_overlap", "CohortCharacteristics", "Cohort overlap", "circle-half-stroke", "Cohort overlap shows the number of subjects that contribute to a pair of cohorts.",
  2L, "summarise_cohort_attrition", "CohortCharacteristics", "Cohort Attrition", "layer-group", "",
  3L, "summarise_cohort_timing", "CohortCharacteristics", "Cohort timing", "chart-simple", "",
  4L, "summarise_characteristics", "CohortCharacteristics", "Cohort characteristics", "users-gear", "",
  5L, "summarise_cohort_count", "CohortCharacteristics", "Cohort count", "users", "",
  6L, "summarise_large_scale_characteristics", "CohortCharacteristics", "Large Scale Characteristics", "arrow-up-right-dots", ""
)

omopViewerPlots <- dplyr::tribble(
  ~plot_id, ~result_tab_id, ~title, ~fun, ~output,
  1L, 1L, "Plot cohort overlap", "plotCohortOverlap", "ggplot2",
  2L, 2L, "Diagram", "plotCohortAttrition", "grViz",
  3L, 3L, "Plot cohort timing", "plotCohortTiming", "ggplot2",
  4L, 4L, "Plot characteristics", "plotCharacteristics", "ggplot2",
  5L, 5L, "Plot cohort count", "plotCohortCount", "ggplot2"
)

omopViewerPlotArguments <- dplyr::tribble(
  ~plot_id, ~argument, ~type, ~opts, ~multiple,
  1L, "facet", "selector", "<grouping>, <variable>, <settings>", TRUE,
  1L, "uniqueCombinations", "check", "", NA,
  3L, "plotType", "selector", "boxplot, density", FALSE,
  3L, "timeScale", "selector", "days, years", FALSE,
  3L, "facet", "selector", "<grouping>, <variable>, <settings>", TRUE,
  3L, "colour", "selector", "<grouping>, <variable>, <settings>", TRUE,
  3L, "uniqueCombinations", "check", "", NA,
  4L, "plotStyle", "selector", "boxplot, barplot, scatterplot", FALSE,
  4L, "facet", "selector", "<grouping>, <variable>, <settings>", TRUE,
  4L, "colour", "selector", "<grouping>, <variable>, <settings>", TRUE,
  5L, "facet", "selector", "<grouping>, <variable>", TRUE,
  5L, "colour", "selector", "<grouping>, <variable>", TRUE,
)

omopViewerProj <- c(
  "Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
  "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
  "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8", "",
  "RnwWeave: Sweave", "LaTeX: pdfLaTeX"
)

omopViewerGlobal <- c(
  "library(shiny)",
  "library(omopViewer)",
  "",
  "data <- importSummarisedResult(here::here(\"data\"))"
) |>
  styleCode()

# plot_config ----
plot_config <- list(
  summarised_large_scale_characteristics = list(
    name = "summarised_large_scale_characteristics",
    plotFunc = CohortCharacteristics::plotLargeScaleCharacteristics,
    plotParams = list(
      facet = list(id = "lsc_plot_facet", multiple = TRUE),
      colorVars = list(id = "lsc_plot_colour", multiple = TRUE),
      splitStrata = list(id = "lsc_plot_strata", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
    ),
    plotButtons = list(
      facet = list(id = "lsc_plot_facet", multiple = TRUE),
      colorVars = list(id = "lsc_plot_colour", multiple = TRUE),
      position = list(id = "lsc_plot_style", multiple = FALSE, choices = c("horizontal", "vertical"), selected = "horizontal"),
      splitStrata = list(id = "lsc_plot_strata", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
      variable_level = list(id = "lsc_plot_var_level", multiple = TRUE),
      variable_name = list(id = "lsc_plot_variable", multiple = TRUE)
    ),
    dataArg = "data",
    updatePickerInputIDs = list(
      facet = "lsc_plot_facet",
      colorVars = "lsc_plot_colour",
      variable_name = "lsc_plot_variable",
      variable_level = "lsc_plot_var_level"
    )
  ),
  cohort_overlap = list(
    name = "cohort_overlap",
    plotFunc = CohortCharacteristics::plotCohortOverlap,
    plotParams = list(
      facet = list(id = "co_plot_facet", multiple = TRUE),
      uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
    ),
    plotButtons = list(
      facet = list(id = "co_plot_facet", multiple = TRUE),
      uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
    ),

    dataArg = "result",
    updatePickerInputIDs = list(
      facet = "co_plot_facet"
    )
  ),
  summarised_characteristics = list(
    name = "summarised_characteristics",
    plotFunc = CohortCharacteristics::plotCharacteristics,
    plotButtons= list(
      facet = list(id = "sc_plot_facet", multiple = TRUE),
      colour = list(id = "sc_plot_colour", multiple = TRUE),
      plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                       choices = c("boxplot", "barplot"), selected = "barplot"),
      x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                           "variable_level", "estimate_type", "group_name",
                                                                           "group_level"),
               selected = "variable_name"),
      variable_name = list(id = "sc_plot_variable", multiple = FALSE)
    ),
    plotParams= list(
      facet = list(id = "sc_plot_facet", multiple = TRUE),
      colour = list(id = "sc_plot_colour", multiple = TRUE),
      plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                       choices = c("boxplot", "barplot"), selected = "barplot"),
      x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                           "variable_level", "estimate_type", "group_name",
                                                                           "group_level"),
               selected = "variable_name")
    ),
    dataArg = "data",
    updatePickerInputIDs = list(
      variable_name = "sc_plot_variable",
      facet = "sc_plot_facet",
      colour = "sc_plot_colour"
    )
  ),
  cohort_timing = list(
    name = "cohort_timing",
    plotFunc = CohortCharacteristics::plotCohortTiming,
    plotParams = list(
      facet = list(id = "ct_plot_facet", multiple = TRUE),
      colour = list(id = "ct_plot_colour", multiple = TRUE),
      uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
      plotType = list(id = "ct_plot_style", multiple = FALSE,
                      choices = c("boxplot"), selected = "boxplot")
    ),
    plotButtons = list(
      facet = list(id = "ct_plot_facet", multiple = TRUE),
      colour = list(id = "ct_plot_colour", multiple = TRUE),
      uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
    ),

    dataArg = "result",
    updatePickerInputIDs = list(
      facet = "ct_plot_facet",
      colour = "ct_plot_colour"
    )
  ),
  cohort_timing_density = list(
    name = "cohort_timing_density",
    plotFunc = CohortCharacteristics::plotCohortTiming,
    plotParams = list(
      facet = list(id = "ctd_plot_facet", multiple = TRUE),
      colour = list(id = "ctd_plot_colour", multiple = TRUE),
      uniqueCombinations = list(id = "ctd_unique_comb",
                                multiple = FALSE,
                                choices = c(TRUE, FALSE),
                                selected = TRUE),
      plotType = list(id = "ctd_plot_style", multiple = FALSE,
                      choices = c("density"), selected = "density")
    ),
    plotButtons = list(
      facet = list(id = "ctd_plot_facet", multiple = TRUE),
      colour = list(id = "ctd_plot_colour", multiple = TRUE),
      uniqueCombinations = list(id = "ctd_unique_comb",
                                multiple = FALSE,
                                choices = c(TRUE, FALSE),
                                selected = TRUE),
      plotType = list(id = "ctd_plot_style", multiple = FALSE,
                      choices = c("density"), selected = "density")
    ),
    dataArg = "result",
    updatePickerInputIDs = list(
      facet = "ctd_plot_facet",
      colour = "ctd_plot_colour"
    )
  )
)
# end plot config ----

# TO ADD NEW LOGOS YOU HAVE TO ADD THEM IN THIS LIST AND IN `inst/www/images/`
# FOLLOW THIS NAMING: '{keyword}_logo.svg'
# NOTE IT IS NOT CASE SENSITIVE
logoKeywords <- c("hds", "ohdsi") |>
  stringr::str_to_lower()

usethis::use_data(
  omopViewerTabs, omopViewerPlots, omopViewerPlotArguments, omopViewerProj,
  omopViewerGlobal, plot_config, logoKeywords, overwrite = TRUE,
  internal = TRUE)

