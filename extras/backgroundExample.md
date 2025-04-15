-----
header: "`OmopViewer` example shiny"
-----

#### Shiny content

This Shiny App contains the defined panels by the **OmopViewer** package. They are aggregated by package name as follows:

[**OmopSketch**](https://cran.r-project.org/package=OmopSketch):

- *Snapshot*: metadata extracted from the `cdm_source` table, generated from [summariseOmopSnaphot()](https://ohdsi.github.io/OmopSketch/reference/summariseOmopSnapshot.html) output.
- *Observation period*: summary of the observation period lengths and characteristics, generated from [summariseObservationPeriod()](https://ohdsi.github.io/OmopSketch/reference/summariseObservationPeriod.html) output.
- *Clinical records*: summary of the clinical tables centered on vocabularies and quality checks, generated from [summariseClinicalRecords()](https://ohdsi.github.io/OmopSketch/reference/summariseClinicalRecords.html) output.
- *Record count*: number of records per year of given 'omop tables', generated from [summariseRecordCount()](https://ohdsi.github.io/OmopSketch/reference/summariseRecordCount.html) output.
- *Missing data*: report missing (and counts of id = 0) of 'omop tables' columns, generated from [summariseMissingData()](https://ohdsi.github.io/OmopSketch/reference/summariseMissingData.html) output.
- *In observation*: summary of the number of individuals in observation per year, generated from [summariseInObservation()](https://ohdsi.github.io/OmopSketch/reference/summariseInObservation.html) output.

[**CodelistGenerator**](https://cran.r-project.org/package=CodelistGenerator):

- [summariseOrphanCodes()](https://darwin-eu.github.io/CodelistGenerator/reference/summariseOrphanCodes.html)
- [summariseCohortCodeUse()](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCohortCodeUse.html)
- [summariseCodeUse()](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCodeUse.html)
- [summariseAchillesCodeUse()](https://darwin-eu.github.io/CodelistGenerator/reference/summariseAchillesCodeUse.html)
- [summariseUnmappedCodes()](https://darwin-eu.github.io/CodelistGenerator/reference/summariseUnmappedCodes.html)

[**CohortCharacteristics**](https://cran.r-project.org/package=CohortCharacteristics):

- [summariseCohortOverlap()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortOverlap.html)
- [summariseCohortTiming()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortTiming.html)
- [summariseCohortCount()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortCount.html)
- [summariseCohortAttrition()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortAttrition.html)
- [summariseCharacteristics()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCharacteristics.html)
- [summariseLargeScaleCharacteristics()](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseLargeScaleCharacteristics.html)

[**IncidencePrevalence**](https://cran.r-project.org/package=IncidencePrevalence):

- [estimateIncidence()](https://darwin-eu.github.io/IncidencePrevalence/reference/estimateIncidence.html)
- [estimatePointPrevalence()](https://darwin-eu.github.io/IncidencePrevalence/reference/estimatePointPrevalence.html) or [estimatePeriodPrevalence()](https://darwin-eu.github.io/IncidencePrevalence/reference/estimatePeriodPrevalence.html)

[**DrugUtilisation**](https://cran.r-project.org/package=DrugUtilisation):

- [summariseDoseCoverage()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDoseCoverage.html)
- [summariseProportionOfPatientsCovered()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseProportionOfPatientsCovered.html)
- [summariseDrugRestart()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugRestart.html)
- [summariseDrugUtilisation()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugUtilisation.html)
- [summariseIndication()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseIndication.html)
- [summariseTreatment()](https://darwin-eu.github.io/DrugUtilisation/reference/summariseTreatment.html)


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
