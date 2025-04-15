-----
header: "`OmopViewer` example shiny"
-----

#### Shiny content

This Shiny App contains the supported panels by the **OmopViewer** package. They are aggregated by package name as follows:

[**OmopSketch**](https://cran.r-project.org/package=OmopSketch):
- [summariseOmopSnaphot()](https://ohdsi.github.io/OmopSketch/reference/summariseOmopSnapshot.html)
- [summariseObservationPeriod()](https://ohdsi.github.io/OmopSketch/reference/summariseObservationPeriod.html)
- [summariseClinicalRecords()](https://ohdsi.github.io/OmopSketch/reference/summariseClinicalRecords.html)
- [summariseRecordCount()](https://ohdsi.github.io/OmopSketch/reference/summariseRecordCount.html)
- [summariseMissingData()](https://ohdsi.github.io/OmopSketch/reference/summariseMissingData.html)
- [summariseInObservation()](https://ohdsi.github.io/OmopSketch/reference/summariseInObservation.html)

[**CodelistGenerator**]
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

#### Shiny generation

This shiny app is generated with the following code:

```
library(OmopViewer)
exportStaticApp(
  result = omopViewerResults,
  directory = here::here(),
  background = "https://raw.githubusercontent.com/OHDSI/OmopViewer/refs/heads/main/extras/backgroundExample.md"
)
```
