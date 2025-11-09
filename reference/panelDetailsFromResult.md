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
#> •  icon: clipboard-list
#> •  data: result_type: <summarise_omop_snapshot>
#> •  filters: 1 filters + 1 automatic filters
#> •  content: Tidy (DT); Table Snapshot (gt)
#> 
#> $summarise_observation_period
#> Observation period (OmopViewer panel)
#> •  icon: eye
#> •  data: result_type: <summarise_observation_period>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Tidy (DT); Table Observation period (gt); Plot Observation period (ui)
#> 
#> $summarise_clinical_records
#> Clinical records (OmopViewer panel)
#> •  icon: bars-staggered
#> •  data: result_type: <summarise_clinical_records>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Clinical records (gt)
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
#> •  content: Tidy (DT); Table Missing data (gt)
#> 
#> $summarise_in_observation
#> In Observation (OmopViewer panel)
#> •  icon: explosion
#> •  data: result_type: <summarise_in_observation>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Plot in observation (ui)
#> 
#> $orphan_code_use
#> Orphan codes (OmopViewer panel)
#> •  icon: magnifying-glass-arrow-right
#> •  data: result_type: <orphan_code_use>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Tidy (DT); Table Orphan codes (gt)
#> 
#> $cohort_code_use
#> Cohort code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <cohort_code_use>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Tidy (DT); Table Cohort code use (gt)
#> 
#> $code_use
#> Code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <code_use>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Code use (gt)
#> 
#> $achilles_code_use
#> Achilles code use (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <achilles_code_use>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Achilles code use (gt)
#> 
#> $unmapped_codes
#> Unmapped codes (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <unmapped_codes>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Unmapped codes (gt)
#> 
#> $summarise_cohort_overlap
#> Cohort Overlap (OmopViewer panel)
#> •  icon: circle-half-stroke
#> •  data: result_type: <summarise_cohort_overlap>
#> •  filters: 1 filters + 5 automatic filters
#> •  content: Tidy (DT); Table Overlap (gt); Plot Overlap (ui)
#> 
#> $summarise_cohort_count
#> Cohort Count (OmopViewer panel)
#> •  icon: users
#> •  data: result_type: <summarise_cohort_count>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Tidy (DT); Table Counts (gt); Plot Counts (ui)
#> 
#> $summarise_cohort_attrition
#> Cohort Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <summarise_cohort_attrition>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Tidy (DT); Table Attrition (gt); Diagram (grViz)
#> 
#> $summarise_cohort_timing
#> Cohort Timing (OmopViewer panel)
#> •  icon: chart-simple
#> •  data: result_type: <summarise_cohort_timing>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Tidy (DT); Table Timing (gt); Plot Timing (ui)
#> 
#> $summarise_characteristics
#> Cohort Characteristics (OmopViewer panel)
#> •  icon: users-gear
#> •  data: result_type: <summarise_characteristics>
#> •  filters: 1 filters + 4 automatic filters
#> •  content: Tidy (DT); Table Characteristics (gt); Plot Characteristics (ui)
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
#> •  content: Tidy (DT); Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
#> 
#> $incidence_attrition
#> Incidence Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <incidence_attrition>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Tidy (DT); Table Incidence Attrition (gt)
#> 
#> $prevalence
#> Prevalence (OmopViewer panel)
#> •  icon: chart-column
#> •  data: result_type: <prevalence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Prevalence (gt); Plot Prevalence (ui); Plot population (ui)
#> 
#> $prevalence_attrition
#> Prevalence Attrition (OmopViewer panel)
#> •  icon: layer-group
#> •  data: result_type: <prevalence_attrition>
#> •  filters: 1 filters + 2 automatic filters
#> •  content: Tidy (DT); Table Prevalence Attrition (gt)
#> 
#> $summarise_dose_coverage
#> Dose coverage (OmopViewer panel)
#> •  icon: pills
#> •  data: result_type: <summarise_dose_coverage>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Dose coverage (gt)
#> 
#> $summarise_proportion_of_patients_covered
#> Proportion of patients covered (OmopViewer panel)
#> •  icon: chart-gantt
#> •  data: result_type: <summarise_proportion_of_patients_covered>
#> •  filters: 1 filters + 3 automatic filters
#> •  content: Tidy (DT); Table PPC (gt); Plot PPC (ui)
#> 
#> $summarise_drug_restart
#> Drug Restart (OmopViewer panel)
#> •  icon: chart-gantt
#> •  data: result_type: <summarise_drug_restart>
#> •  filters: 1 filters + 3 automatic filters
#> •  content: Tidy (DT); Table Drug Restart (gt); Plot Drug Restart (ui)
#> 
#> $summarise_drug_utilisation
#> Drug Utilisation (OmopViewer panel)
#> •  icon: capsules
#> •  data: result_type: <summarise_drug_utilisation>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Tidy (DT); Table Drug Utilisation (gt); Plot Drug Utilisation (ui)
#> 
#> $summarise_indication
#> Indication (OmopViewer panel)
#> •  icon: disease
#> •  data: result_type: <summarise_indication>
#> •  filters: 1 filters + 7 automatic filters
#> •  content: Tidy (DT); Table Indication (gt); Plot Indication (ui)
#> 
#> $summarise_treatment
#> Treatments (OmopViewer panel)
#> •  icon: disease
#> •  data: result_type: <summarise_treatment>
#> •  filters: 1 filters + 7 automatic filters
#> •  content: Tidy (DT); Table Treatments (gt); Plot Treatment (ui)
#> 
```
