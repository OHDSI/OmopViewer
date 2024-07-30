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



usethis::use_data(plot_config, internal = TRUE, overwrite = TRUE)

