
# NOTES
# load the package before trying to use styleCode()
# project ----
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

# preprocess ----
omopViewerPreprocess <- c(
  "",
  "source(file.path(getwd(), \"functions.R\"))",
  "",
  "result <- omopgenerics::importSummarisedResult(file.path(getwd(), \"data\"))",
  "data <- prepareResult(result, resultList)",
  "values <- getValues(result, resultList)",
  "",
  "# edit choices and values of interest",
  "choices <- values",
  "selected <- getSelected(values)",
  "",
  "save(data, choices, selected, values, file = file.path(getwd(), \"data\", \"shinyData.RData\"))",
  "",
  "rm(result, values, choices, selected, resultList, data)"
)

# global -----
omopViewerGlobal <- c(
  "# preprocess data if it has not been done",
  "fileData <- file.path(getwd(), \"data\", \"shinyData.RData\")",
  "if (!file.exists(fileData)) {",
  "source(file.path(getwd(), \"data\", \"preprocess.R\"))",
  "}",
  "",
  "# uncomment to load the raw data",
  "# rawData <- omopgenerics::importSummarisedResult(file.path(getwd(), \"data\"))",
  "",
  "# load shiny data",
  "load(fileData)",
  "",
  "# source functions",
  "source(file.path(getwd(), \"functions.R\"))"
) |>
  styleCode()

# logos ----
# TO ADD NEW LOGOS YOU HAVE TO ADD THEM IN `inst/logos/`
# FOLLOW THIS NAMING: '{keyword}_logo.svg'
# NOTE IT IS NOT CASE SENSITIVE
logoKeywords <- list.files(path = system.file("logos", package = "OmopViewer")) |>
  stringr::str_to_lower() |>
  purrr::keep(\(x) stringr::str_ends(string = x, pattern = "_logo.svg")) |>
  stringr::str_replace_all(pattern = "_logo.svg$", replacement = "")

# background keywords ----
# IT HAS TO BE EDITED HERE AND IN `functions.R`!!
backgroundKeywords <- dplyr::tribble(
  ~keyword, ~fun, ~link,
  "header", "bslib::card_header", "https://rstudio.github.io/bslib//reference/card_body.html",
  "footer", "bslib::card_footer", "https://rstudio.github.io/bslib//reference/card_body.html"
)

# default structure
panelStructureDefaults <- list(
  OmopSketch = c(
    "summarise_omop_snapshot", "summarise_observation_period",
    "summarise_clinical_records", "summarise_record_count",
    "summarise_missing_data", "summarise_in_observation",
    "summarise_trend", "summarise_concept_id_counts", "summarise_person"
  ),
  CodelistGenerator = c(
    "orphan_code_use", "cohort_code_use", "code_use", "achilles_code_use",
    "unmapped_codes"
  ),
  CohortCharacteristics = c(
    "summarise_cohort_overlap", "summarise_cohort_count",
    "summarise_cohort_attrition", "summarise_cohort_timing",
    "summarise_characteristics", "summarise_large_scale_characteristics"
  ),
  IncidencePrevalence = c(
    "incidence", "incidence_attrition", "prevalence", "prevalence_attrition"
  ),
  DrugUtilisation = c(
    "summarise_dose_coverage", "summarise_proportion_of_patients_covered",
    "summarise_drug_restart", "summarise_drug_utilisation",
    "summarise_indication", "summarise_treatment"
  ),
  MeasurementDiagnostics =c(
    "measurement_timings", "measurement_value_as_numeric",
    "measurement_value_as_concept"
  )
)

# add internal data ----
usethis::use_data(
  omopViewerProj, omopViewerGlobal, omopViewerPreprocess, logoKeywords,
  backgroundKeywords, panelStructureDefaults,
  overwrite = TRUE, internal = TRUE
)
