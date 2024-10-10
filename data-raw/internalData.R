
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
  6L, "summarise_large_scale_characteristics", "CohortCharacteristics", "Large Scale Characteristics", "arrow-up-right-dots", "",
  7L, "orphan_code_use", "CodelistGenerator", "Orphan codes", "magnifying-glass-arrow-right", "",
  8L, "cohort_code_use", "CodelistGenerator", "Cohort code use", "chart-column", "",
  9L, "code_use", "CodelistGenerator", "Code use", "chart-column", "",
  10L, "achilles_code_use", "CodelistGenerator", "Achilles code use", "chart-column", ""
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

omopViewerTables <- dplyr::tribble(
  ~table_id, ~result_tab_id, ~title, ~fun, ~output,
  0L, 0L, "Formatted", "omopViewer::visTable", "gt",
  1L, 1L, "Table cohort overlap", "tableCohortOverlap", "gt",
  2L, 2L, "Table cohort attrition", "tableCohortAttrition", "gt",
  3L, 3L, "Table cohort timing", "tableCohortTiming", "gt",
  4L, 4L, "Table cohort characteristics", "tableCharacteristics", "gt",
  5L, 5L, "Table cohort count", "tableCohortCount", "gt",
  6L, 7L, "Table orphan codes", "tableOrphanCodes", "gt",
  7L, 8L, "Table cohort code use", "tableCohortCodeUse", "gt",
  8L, 9L, "Table code use", "tableCodeUse", "gt",
  9L, 10L, "Table achilles code use", "tableAchillesCodeUse", "gt"
)

omopViewerTableArguments <- dplyr::tribble(
  ~ table_id, ~argument, ~name, ~value,
  0L, "header", "type", "rank",
  0L, "header", "default", "cdm_name",
  0L, "group", "type", "rank",
  0L, "group", "default", "cohort_name",
  0L, "hide", "type", "rank",
  0L, "hide", "default", "<settings>",
  0L, NA, "rank_options", "<grouping>, <variable>, <settings>",
  1L, "uniqueCombinations", "type", "check",
  1L, "header", "type", "rank",
  1L, "groupColumn", "type", "rank",
  1L, "hide", "type", "rank",
  1L, NA, "rank_options", "<grouping>, <variable>",
  2L, "header", "type", "rank",
  2L, "groupColumn", "type", "rank",
  2L, "hide", "type", "rank",
  2L, NA, "rank_options", "<grouping>, <variable>",
  3L, "uniqueCombinations", "type", "check",
  3L, "timeScale", "type", "selector",
  3L, "timeScale", "options", "days, years",
  3L, "timeScale", "multiple", "FALSE",
  3L, "header", "type", "rank",
  3L, "groupColumn", "type", "rank",
  3L, "hide", "type", "rank",
  3L, NA, "rank_options", "<grouping>, <variable>",
  4L, "header", "type", "rank",
  4L, "groupColumn", "type", "rank",
  4L, "hide", "type", "rank",
  4L, NA, "rank_options", "<grouping>, <variable>",
  5L, "header", "type", "rank",
  5L, "groupColumn", "type", "rank",
  5L, "hide", "type", "rank",
  5L, NA, "rank_options", "<grouping>, <variable>",
  6L, "header", "type", "rank",
  6L, "groupColumn", "type", "rank",
  6L, "hide", "type", "rank",
  6L, NA, "rank_options", "<grouping>, <variable>",
  7L, "timing", "type", "check",
  7L, "header", "type", "rank",
  7L, "groupColumn", "type", "rank",
  7L, "hide", "type", "rank",
  7L, NA, "rank_options", "<grouping>, <variable>",
  8L, "header", "type", "rank",
  8L, "groupColumn", "type", "rank",
  8L, "hide", "type", "rank",
  8L, NA, "rank_options", "<grouping>, <variable>",
  9L, "header", "type", "rank",
  9L, "groupColumn", "type", "rank",
  9L, "hide", "type", "rank",
  9L, NA, "rank_options", "<grouping>, <variable>"
)

omopViewerProj <- c(
  "Version: 1.0",
  "",
  "RestoreWorkspace: Default",
  "SaveWorkspace: Default",
  "AlwaysSaveHistory: Default",
  "",
  "EnableCodeIndexing: Yes",
  "UseSpacesForTab: Yes",
  "NumSpacesForTab: 2",
  "Encoding: UTF-8",
  "",
  "RnwWeave: Sweave",
  "LaTeX: pdfLaTeX"
)

omopViewerGlobal <- c(
  "data <- omopViewer::importSummarisedResult(here::here(\"data\")) |>",
  "omopViewer::correctSettings()"
) |>
  styleCode()

# TO ADD NEW LOGOS YOU HAVE TO ADD THEM IN THIS LIST AND IN `inst/logos/`
# FOLLOW THIS NAMING: '{keyword}_logo.svg'
# NOTE IT IS NOT CASE SENSITIVE
logoKeywords <- c("hds", "ohdsi") |>
  stringr::str_to_lower()

backgroundKeywords <- dplyr::tribble(
  ~keyword, ~fun, ~link,
  "header", "bslib::card_header", "https://rstudio.github.io/bslib//reference/card_body.html",
  "footer", "bslib::card_footer", "https://rstudio.github.io/bslib//reference/card_body.html"
)

usethis::use_data(
  omopViewerTabs, omopViewerPlots, omopViewerPlotArguments, omopViewerProj,
  omopViewerGlobal, logoKeywords, backgroundKeywords, omopViewerTables,
  omopViewerTableArguments, overwrite = TRUE, internal = TRUE)
