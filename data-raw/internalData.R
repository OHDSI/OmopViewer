
# you need to load the package before running these lines of code as styleCode()
# is required.

# to search icons see: https://fontawesome.com/search?m=free&o=r

omopViewerTabs <- dplyr::tribble(
  ~result_tab_id, ~result_type, ~title, ~icon, ~information,
  1L, "summarise_cohort_overlap", "Cohort overlap", "circle-half-stroke", "Cohort overlap shows the number of subjects that contribute to a pair of cohorts.",
  2L, "summarise_cohort_attrition", "Cohort Attrition", "layer-group", "",
  3L, "summarise_cohort_timing", "Cohort timing", "chart-simple", "",
  4L, "summarise_characteristics", "Cohort characteristics", "users-gear", "",
  5L, "summarise_cohort_count", "Cohort count", "users", "",
  6L, "summarise_large_scale_characteristics", "Large Scale Characteristics", "arrow-up-right-dots", "",
  7L, "orphan_code_use", "Orphan codes", "magnifying-glass-arrow-right", "",
  8L, "cohort_code_use", "Cohort code use", "chart-column", "",
  9L, "code_use", "Code use", "chart-column", "",
  10L, "achilles_code_use", "Achilles code use", "chart-column", ""
)

omopViewerOutput <- dplyr::tribble(
  ~output_id, ~result_tab_id, ~output_title, ~output_function, ~output_type,
  # default table
  0L, 0L, "Formatted", "OmopViewer::omopViewerTable", "gt",
  # cohort overlap
  1L, 1L, "Table cohort overlap", "CohortCharacteristics::tableCohortOverlap", "gt",
  2L, 1L, "Plot cohort overlap", "CohortCharacteristics::plotCohortOverlap", "ggplot2",
  # cohort attrition
  3L, 2L, "Table cohort attrition", "CohortCharacteristics::tableCohortAttrition", "gt",
  4L, 2L, "Diagram", "CohortCharacteristics::plotCohortAttrition", "grViz",
  # cohort timing
  5L, 3L, "Table cohort timing", "CohortCharacteristics::tableCohortTiming", "gt",
  6L, 3L, "Plot cohort timing", "CohortCharacteristics::plotCohortTiming", "ggplot2",
  # cohort characteristics
  7L, 4L, "Table cohort characteristics", "CohortCharacteristics::tableCharacteristics", "gt",
  8L, 4L, "Plot characteristics", "CohortCharacteristics::plotCharacteristics", "ggplot2",
  # cohort count
  9L, 5L, "Table cohort count", "CohortCharacteristics::tableCohortCount", "gt",
  10L, 5L, "Plot cohort count", "CohortCharacteristics::plotCohortCount", "ggplot2",
  # orphan codes
  11L, 7L, "Table orphan codes", "CodelistGenerator::tableOrphanCodes", "gt",
  # cohort code use
  12L, 8L, "Table cohort code use", "CodelistGenerator::tableCohortCodeUse", "gt",
  # code use
  13L, 9L, "Table code use", "CodelistGenerator::tableCodeUse", "gt",
  # achilles code us
  14L, 10L, "Table achilles code use", "CodelistGenerator::tableAchillesCodeUse", "gt"
)

omopViewerOutputArguments <- dplyr::tribble(
  ~ output_id, ~argument, ~name, ~value,
  # OmopViewer::omopViewerTable
  0L, "header", "type", "rank",
  0L, "header", "default", "cdm_name",
  0L, "group", "type", "rank",
  0L, "group", "default", "cohort_name",
  0L, "hide", "type", "rank",
  0L, "hide", "default", "<settings>",
  0L, NA, "rank_options", "<grouping>, <variable>, <settings>",
  # CohortCharacteristics::tableCohortOverlap
  1L, "uniqueCombinations", "type", "check",
  1L, "header", "type", "rank",
  1L, "groupColumn", "type", "rank",
  1L, "hide", "type", "rank",
  1L, NA, "rank_options", "<grouping>, <variable>",
  # CohortCharacteristics::plotCohortOverlap
  2L, "facet", "type", "selector",
  2L, "facet", "options", "<grouping>, <variable>, <settings>",
  2L, "facet", "multiple", "TRUE",
  2L, "uniqueCombinations", "type", "check",
  # CohortCharacteristics::tableCohortAttrition
  3L, "header", "type", "rank",
  3L, "groupColumn", "type", "rank",
  3L, "hide", "type", "rank",
  3L, NA, "rank_options", "<grouping>, <variable>",
  # CohortCharacteristics::plotCohortAttrition
  # CohortCharacteristics::tableCohortTiming
  5L, "uniqueCombinations", "type", "check",
  5L, "timeScale", "type", "selector",
  5L, "timeScale", "options", "days, years",
  5L, "timeScale", "multiple", "FALSE",
  5L, "header", "type", "rank",
  5L, "groupColumn", "type", "rank",
  5L, "hide", "type", "rank",
  5L, NA, "rank_options", "<grouping>, <variable>",
  # CohortCharacteristics::plotCohortTiming
  6L, "plotType", "type", "selector",
  6L, "plotType", "options", "boxplot, density",
  6L, "plotType", "multiple", "FALSE",
  6L, "timeScale", "type", "selector",
  6L, "timeScale", "options", "days, years",
  6L, "timeScale", "multiple", "FALSE",
  6L, "facet", "type", "selector",
  6L, "facet", "options", "<grouping>, <variable>, <settings>",
  6L, "facet", "multiple", "TRUE",
  6L, "colour", "type", "selector",
  6L, "colour", "options", "<grouping>, <variable>, <settings>",
  6L, "colour", "multiple", "TRUE",
  6L, "uniqueCombinations", "type", "check",
  # CohortCharacteristics::tableCharacteristics
  7L, "header", "type", "rank",
  7L, "groupColumn", "type", "rank",
  7L, "hide", "type", "rank",
  7L, NA, "rank_options", "<grouping>, <variable>",
  # CohortCharacteristics::plotCharacteristics
  8L, "plotType", "type", "selector",
  8L, "plotType", "options", "boxplot, barplot, scatterplot",
  8L, "plotType", "multiple", "FALSE",
  8L, "facet", "type", "selector",
  8L, "facet", "options", "<grouping>, <variable>, <settings>",
  8L, "facet", "multiple", "TRUE",
  8L, "colour", "type", "selector",
  8L, "colour", "options", "<grouping>, <variable>, <settings>",
  8L, "colour", "multiple", "TRUE",
  # CohortCharacteristics::tableCohortCount
  9L, "header", "type", "rank",
  9L, "groupColumn", "type", "rank",
  9L, "hide", "type", "rank",
  9L, NA, "rank_options", "<grouping>, <variable>",
  # CohortCharacteristics::plotCohortCount
  10L, "facet", "type", "selector",
  10L, "facet", "options", "<grouping>, <variable>",
  10L, "facet", "multiple", "TRUE",
  10L, "colour", "type", "selector",
  10L, "colour", "options", "<grouping>, <variable>",
  10L, "colour", "multiple", "TRUE",
  # CodelistGenerator::tableOrphanCodes
  11L, "header", "type", "rank",
  11L, "groupColumn", "type", "rank",
  11L, "hide", "type", "rank",
  11L, NA, "rank_options", "<grouping>, <variable>",
  # CodelistGenerator::tableCohortCodeUse
  12L, "timing", "type", "check",
  12L, "header", "type", "rank",
  12L, "groupColumn", "type", "rank",
  12L, "hide", "type", "rank",
  12L, NA, "rank_options", "<grouping>, <variable>",
  # CodelistGenerator::tableCodeUse
  13L, "header", "type", "rank",
  13L, "groupColumn", "type", "rank",
  13L, "hide", "type", "rank",
  13L, NA, "rank_options", "<grouping>, <variable>",
  # CodelistGenerator::tableAchillesCodeUse
  14L, "header", "type", "rank",
  14L, "groupColumn", "type", "rank",
  14L, "hide", "type", "rank",
  14L, NA, "rank_options", "<grouping>, <variable>"
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
  "data <- OmopViewer::importSummarisedResult(here::here(\"data\")) |>",
  "OmopViewer::correctSettings()"
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
  omopViewerTabs, omopViewerOutput, omopViewerOutputArguments, omopViewerProj,
  omopViewerGlobal, logoKeywords, backgroundKeywords, overwrite = TRUE,
  internal = TRUE)
