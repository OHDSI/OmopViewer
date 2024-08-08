## code to prepare `DATASET` dataset goes here

resultTypeTabs <- dplyr::tribble(
  ~result_type, ~title, ~icon, ~raw, ~formatted, ~plot,
  "summarise_cohort_characteristics", "Cohort characteristics", "people-group", TRUE, TRUE, TRUE
)

proj <- c(
  "Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
  "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
  "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8", "",
  "RnwWeave: Sweave", "LaTeX: pdfLaTeX"
)

global <- c(
  "library(shiny)", "library(omopViewer)", "",
  "data <- importSummarisedResult(here::here(\"data\"))"
)

usethis::use_data(resultTypeTabs, proj, global, overwrite = TRUE, internal = TRUE)
