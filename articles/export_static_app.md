# Create a static shiny app

## Introduction

This vignette shows how to create static *Shiny apps* using the
`OmopViewer` package. These static shiny apps are separated projects
with their own `ui`, `server`, `global` and files to pre-process the
results that later can easily later be customised and deployed. This
functionality is adequate if what you are willing is to deploy a shiny,
customise it later and/or be able to access this shiny app in the
future. If what you want is to visualise the data briefly and you do not
need to access to the shiny in the future, edit it or deploy it maybe
the [Dynamic
app](https://ohdsi.github.io/OmopViewer/articles/dynamic_app.Rmd)
functionality is what are you searching for.

## Loading Necessary Libraries and Data

In this vignette we will use simply to packages:

``` r
library(OmopViewer)
library(omopgenerics, warn.conflicts = FALSE)
library(shiny)
```

We’ll use the `omopViewerResults` mock data from this package for
illustration:

``` r
# Inspect the structure of the sample data
summary(omopViewerResults)
#> A summarised_result object with 53067 rows, 122 different result_id, 1 and 1
#> different cdm names, and 53 settings.
#> CDM names: synthea-covid19-200k and mock database.
#> Settings: result_type, package_name, package_version, group, strata,
#> additional, min_cell_count, analysis, analysis_censor_cohort_name,
#> analysis_complete_database_intervals, analysis_full_contribution,
#> analysis_level, analysis_outcome_washout, analysis_repeated_events,
#> analysis_type, censor_date, censor_on_cohort_exit, cohort_definition_id, …,
#> type, and unknown_indication_table.
```

## Subsetting the Data

For this example, we’ll use a subset of the data containing specific
result types:

``` r
result <- omopViewerResults |>
  filterSettings(
    result_type %in% c("summarise_omop_snapshot", "summarise_characteristics", "incidence")
  )
```

This filters the `omopViewerResults` data to include only entries where
result_type is one of “summarise_omop_snapshot”,
“summarise_characteristics”, or “incidence”.

``` r
summary(result)
#> A summarised_result object with 1254 rows, 20 different result_id, 1 different
#> cdm names, and 20 settings.
#> CDM names: synthea-covid19-200k.
#> Settings: result_type, package_name, package_version, group, strata,
#> additional, min_cell_count, analysis_censor_cohort_name,
#> analysis_complete_database_intervals, analysis_outcome_washout,
#> analysis_repeated_events, denominator_age_group,
#> denominator_days_prior_observation, denominator_end_date,
#> denominator_requirements_at_entry, denominator_sex, denominator_start_date,
#> denominator_target_cohort_name, denominator_time_at_risk, and table_name.
```

## Generating the Shiny App

The exportStaticApp function generates a Shiny app from the prepared
data. Using the default parameters, it only requires a directory to save
the app and the processed data (a object):

``` r
dir <- tempdir()
exportStaticApp(result = result, directory = dir)
#> ℹ Processing data
#> ✔ Data processed: 3 panels idenfied: `summarise_omop_snapshot`,
#>   `summarise_characteristics`, and `incidence`.
#> ℹ Creating `shiny` from provided data
#> ✔ Shiny created in: /tmp/Rtmp8mdbFB/shiny
```

Note that by default if executed in an interactive environment like *R
Studio* the project will be opened in a separated window. Use
`open = FALSE` if you do not wish to open the shiny app after generating
it.

### Generated shiny app

See that this created a new project **shiny** in the specified directory
along with some files:

``` r
cat(list.files(path = here::here(dir, "shiny"), recursive = TRUE), sep = "\n")
#> _brand.yml
#> background.md
#> data/README.md
#> functions.R
#> global.R
#> rawData/preprocess.R
#> rawData/README.md
#> rawData/results.csv
#> server.R
#> shiny.Rproj
#> ui.R
#> www/hds_logo.svg
#> www/ohdsi_logo.svg
```

- `background.R` is only generated if background argument is set to
  `TRUE` and is used to customise the landing page of the shiny app.

- `data` folder contains the raw result object `results.csv` and the
  script to process the data `preprocess.R`.

- `funcitons.R` contains several functions that are used internally in
  the shiny app.

- `global.R`, `ui.R`, and `server.R` define the shiny app itself.

- `www` contains images and logos used in the shiny app, by default
  `hds_logo.svg` and `ohdsi_logo.svg`.

## Panels generated (panelDetails)

The shiny generated contained a total of 3 panels, this was determined
by the argument `panelDetails`. Each element in `panelDetails` will be
used to create a different panel in the shiny app. The package contains
in total 36 predefined panels:

``` r
omopViewerPanels
#> $summarise_omop_snapshot
#> Snapshot (OmopViewer panel)
#> •  icon: camera
#> •  data: result_type: <summarise_omop_snapshot>
#> •  filters: 1 filters + 1 automatic filters
#> •  content: Table Snapshot (gt)
#> 
#> $summarise_observation_period
#> Observation period Summary (OmopViewer panel)
#> •  icon: eye
#> •  data: result_type: <summarise_observation_period>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Observation period (gt); Plot Observation period (ui)
#> 
#> $summarise_clinical_records
#> Clinical Tables Summary (OmopViewer panel)
#> •  icon: bars-staggered
#> •  data: result_type: <summarise_clinical_records>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Clinical records (gt)
#> 
#> $summarise_record_count
#> Record count (OmopViewer panel)
#> •  icon: signal
#> •  data: result_type: <summarise_record_count>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Plot record count (ui)
#> 
#> $summarise_missing_data
#> Missing data (OmopViewer panel)
#> •  icon: circle-exclamation
#> •  data: result_type: <summarise_missing_data>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Missing data (gt)
#> 
#> $summarise_in_observation
#> In Observation (OmopViewer panel)
#> •  icon: explosion
#> •  data: result_type: <summarise_in_observation>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Plot in observation (ui)
#> 
#> $summarise_trend
#> Trends (OmopViewer panel)
#> •  icon: arrow-trend-up
#> •  data: result_type: <summarise_trend>
#> •  filters: 1 filters + 5 automatic filters
#> •  content: Table Trends (reactable); Plot Trends (ui)
#> 
#> $summarise_concept_id_counts
#> Concept Counts (OmopViewer panel)
#> •  icon: database
#> •  data: result_type: <summarise_concept_id_counts>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Concept Counts (reactable); Top Concept Counts (ui)
#> 
#> $summarise_person
#> Person Table Summary (OmopViewer panel)
#> •  icon: person
#> •  data: result_type: <summarise_person>
#> •  filters: 1 filters + 5 automatic filters
#> •  content: Table Person (gt); Plot Person (ui)
#> 
#> $orphan_code_use
#> Orphan codes (OmopViewer panel)
#> •  icon: magnifying-glass-arrow-right
#> •  data: result_type: <orphan_code_use>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Orphan codes (reactable) (reactable); Table Orphan codes (gt) (gt)
#> 
#> $cohort_code_use
#> Cohort code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <cohort_code_use>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Cohort code use (reactable) (reactable); Table Cohort code use (gt) (gt)
#> 
#> $code_use
#> Code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <code_use>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Code use (reactable) (reactable); Table Code use (gt) (gt)
#> 
#> $achilles_code_use
#> Achilles code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <achilles_code_use>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Achilles code use (reactable) (reactable); Table Achilles code use (gt) (gt)
#> 
#> $unmapped_codes
#> Unmapped codes (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <unmapped_codes>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Unmapped codes (reactable) (reactable); Table Unmapped codes (gt) (gt)
#> 
#> $summarise_cohort_overlap
#> Cohort Overlap (OmopViewer panel)
#> •  icon: circle-half-stroke
#> •  data: result_type: <summarise_cohort_overlap>
#> •  filters: 1 filters + 5 automatic filters
#> •  content: Table Overlap (gt); Plot Overlap (ui)
#> 
#> $summarise_cohort_count
#> Cohort Count (OmopViewer panel)
#> •  icon: users
#> •  data: result_type: <summarise_cohort_count>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Counts (gt); Plot Counts (ui)
#> 
#> $summarise_cohort_attrition
#> Cohort Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <summarise_cohort_attrition>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Table Attrition (gt); Diagram (grViz)
#> 
#> $summarise_cohort_timing
#> Cohort Timing (OmopViewer panel)
#> •  icon: chart-simple
#> •  data: result_type: <summarise_cohort_timing>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Table Timing (gt); Plot Timing (ui)
#> 
#> $summarise_characteristics
#> Cohort Characteristics (OmopViewer panel)
#> •  icon: users-gear
#> •  data: result_type: <summarise_characteristics>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Characteristics (gt); Plot Characteristics (ui)
#> 
#> $summarise_large_scale_characteristics
#> Large Scale Characteristics (OmopViewer panel)
#> •  icon: arrow-up-right-dots
#> •  data: result_type: <summarise_large_scale_characteristics>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table (reactable); Most common codes (gt); Plot Compared (plotly)
#> 
#> $incidence
#> Incidence (OmopViewer panel)
#> •  icon: chart-line
#> •  data: result_type: <incidence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
#> 
#> $incidence_attrition
#> Incidence Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <incidence_attrition>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Incidence Attrition (gt)
#> 
#> $prevalence
#> Prevalence (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <prevalence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Prevalence (gt); Plot Prevalence (ui); Plot population (ui)
#> 
#> $prevalence_attrition
#> Prevalence Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <prevalence_attrition>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Prevalence Attrition (gt)
#> 
#> $summarise_dose_coverage
#> Dose coverage (OmopViewer panel)
#> •  icon: pills
#> •  data: result_type: <summarise_dose_coverage>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Dose coverage (gt)
#> 
#> $summarise_proportion_of_patients_covered
#> Proportion of patients covered (OmopViewer panel)
#> •  icon: chart-gantt
#> •  data: result_type: <summarise_proportion_of_patients_covered>
#> •  filters: 1 filters + 3 automatic filters
#> •  content: Table PPC (gt); Plot PPC (ui)
#> 
#> $summarise_drug_restart
#> Drug Restart (OmopViewer panel)
#> •  icon: chart-gantt
#> •  data: result_type: <summarise_drug_restart>
#> •  filters: 1 filters + 3 automatic filters
#> •  content: Table Drug Restart (gt); Plot Drug Restart (ui)
#> 
#> $summarise_drug_utilisation
#> Drug Utilisation (OmopViewer panel)
#> •  icon: capsules
#> •  data: result_type: <summarise_drug_utilisation>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Drug Utilisation (gt); Plot Drug Utilisation (ui)
#> 
#> $summarise_indication
#> Indication (OmopViewer panel)
#> •  icon: disease
#> •  data: result_type: <summarise_indication>
#> •  filters: 1 filters + 7 automatic filters
#> •  content: Table Indication (gt); Plot Indication (ui)
#> 
#> $summarise_treatment
#> Treatments (OmopViewer panel)
#> •  icon: disease
#> •  data: result_type: <summarise_treatment>
#> •  filters: 1 filters + 7 automatic filters
#> •  content: Table Treatments (gt); Plot Treatment (ui)
#> 
#> $measurement_timings
#> Measurement timing (OmopViewer panel)
#> •  icon: timeline
#> •  data: result_type: <measurement_timings>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Measurement Timing (gt); Plot Measurement Timing (ui)
#> 
#> $measurement_value_as_numeric
#> Measurement as numeric (OmopViewer panel)
#> •  icon: gauge-high
#> •  data: result_type: <measurement_value_as_numeric>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Measurement Numeric (gt); Plot Measurement Numeric (ui)
#> 
#> $measurement_value_as_concept
#> Measurement as concept (OmopViewer panel)
#> •  icon: cubes
#> •  data: result_type: <measurement_value_as_concept>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Measurement Concept (gt); Plot Measurement Concept (ui)
#> 
#> $survival
#> Survival Analyses (OmopViewer panel)
#> •  icon: stairs
#> •  data: result_type: <survival_summary>, <survival_estimates>, <survival_events>, <survival_attrition>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Summary (gt); Table Events (gt); Table Attrition (gt); Plot Survival (ui)
#> 
#> $summarise_log_file
#> Logs (OmopViewer panel)
#> •  icon: clipboard-list
#> •  data: result_type: <summarise_log_file>
#> •  filters: 1 filters + 1 automatic filters
#> •  content: Table Logs (gt); Plot Timing (ui)
#> 
#> $default
#> <result_type> (OmopViewer panel)
#> •  icon: folder
#> •  data: -no data-
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table (gt)
```

Each panel is associated with a determined `result_type` as showed in
the print. By default, the function
[`panelDetailsFromResult()`](https://ohdsi.github.io/OmopViewer/reference/panelDetailsFromResult.md)
groups the results by `result_type` and displays each `result_type` in a
separate panel, if a pre-build panel does not exist for that
`result_type` then the *default* panel is used. In our case we have
pre-build panels that are:

``` r
panelDetailsFromResult(result = result)
#> $summarise_omop_snapshot
#> Snapshot (OmopViewer panel)
#> •  icon: camera
#> •  data: result_type: <summarise_omop_snapshot>
#> •  filters: 1 filters + 1 automatic filters
#> •  content: Table Snapshot (gt)
#> 
#> $summarise_characteristics
#> Cohort Characteristics (OmopViewer panel)
#> •  icon: users-gear
#> •  data: result_type: <summarise_characteristics>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Table Characteristics (gt); Plot Characteristics (ui)
#> 
#> $incidence
#> Incidence (OmopViewer panel)
#> •  icon: chart-line
#> •  data: result_type: <incidence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
```

### Understanding Panel Details (panelDetails)

Each panel’s entry contains key information:

1.  Icon: Defines the icon displayed next to the panel, helping to
    visually distinguish between the different types of panels (e.g.,
    clipboard-list, chart-line …).

2.  Data: the fields used in data will be passed to
    [`filterSettings()`](https://darwin-eu.github.io/omopgenerics/reference/filterSettings.html)
    function to determine the data that will be included in that panel.
    Usually `result_type` is the most common way
    (e.g. `panelDetails$data <- list(result_type = "incidence")`), but
    other fields can be used for example:
    `panelDetails$data <- list(result_type = "incidence", denominator_age_group = c("0 to 19", "20 to 39"))`
    would only include the results obtained by the following code:

``` r
result |>
  filterSettings(
    result_type == "incidence" & 
      denominator_age_group %in% c("0 to 19", "20 to 39")
  ) 
```

3.  Filters: Filters (filters + automatic_filters) that allow users to
    refine the results within each panel. Each panel has its own
    defaults. For example, the default incidence panel includes
    automatic_filters: “settings”, “variable_name” and filters:
    “cdm_name”. This means that there will be a field for any column in
    settings, and variable_name and cdm_name columns.

4.  Content: Defines the types of content displayed in the panel, such
    as tables (DT, gt, reactable, …) and plots (ui, ggplot2, plotly, …).
    For example, the `summarise_omop_snapshot` panel includes a table
    displaying the snapshot data and a gt table generated from that
    snapshot data.

All of these elements - icon, data, filters, and content - can be
customised by the user if needed. See more details in the
**customised_panels** vignette.

## Panel Structure (omopViewerPanels)

The arrangement of panels within the app is controlled by the
`panelStructure` variable. This variable determines how panels are
grouped into logical sections or tabs within the Shiny app. By default,
the different panels are grouped by the package the produced the result
object. In this case *OmopSketch*, *CohortCharacteristics* and
*IncidencePrevalence* respectively.

### Customise panelStructure

``` r
ps1 <- list(
  grp_1 = c("summarise_omop_snapshot", "incidence"),
  grp_2 = c("summarise_characteristics")
)

exportStaticApp(result = result, directory = tempdir(), panelStructure = ps1)
```

This custom `panelStructure` groups the “summarise_omop_snapshot” and
“incidence” result types together in grp_1, while placing the
“summarise_characteristics” result type in grp_2. You can pass this
custom structure to `exportStaticApp` to organize the panels according
to your preference.
