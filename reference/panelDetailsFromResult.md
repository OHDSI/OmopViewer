# Obtain default panel details from a `<summarised_result>` object.

Obtain default panel details from a `<summarised_result>` object.

## Usage

``` r
panelDetailsFromResult(result, includeOneChoiceFilters = TRUE)
```

## Arguments

- result:

  A `summarised_result` object.

- includeOneChoiceFilters:

  Whether to include filters that contain only one choice.

## Value

A list of `omop_viewer_panel` objects.

## Examples

``` r
panelDetailsFromResult(omopViewerResults)
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
#> $summarise_missing_data
#> Missing data (OmopViewer panel)
#> •  icon: circle-exclamation
#> •  data: result_type: <summarise_missing_data>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Missing data (gt)
#> 
#> $summarise_trend
#> Trends (OmopViewer panel)
#> •  icon: arrow-trend-up
#> •  data: result_type: <summarise_trend>
#> •  filters: 1 filters + 5 automatic filters
#> •  content: Table Trends (reactable); Plot Trends (ui)
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
#> $summarise_log_file
#> Logs (OmopViewer panel)
#> •  icon: clipboard-list
#> •  data: result_type: <summarise_log_file>
#> •  filters: 1 filters + 1 automatic filters
#> •  content: Table Logs (gt); Plot Timing (ui)
#> 
```
