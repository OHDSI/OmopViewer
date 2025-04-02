# default set of results ----
dbName <- "GiBleed"
con <- duckdb::dbConnect(
  duckdb::duckdb(), CDMConnector::eunomiaDir(datasetName = dbName)
)
cdm <- CDMConnector::cdmFromCon(
  con = con, cdmSchema = "main", writeSchema = "main", cdmName = dbName
)

codelist <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm, name = "acetaminophen", nameStyle = "{concept_name}"
)
codelistConditions <- list(
  viral_sinusitis = 40481087L,
  acute_viral_pharyngitis = 4112343L,
  acute_bronchitis = 260139L,
  otitis_media = 372328L,
  osteoarthritis = 80180L
)

snapshot <- OmopSketch::summariseOmopSnapshot(cdm = cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm, name = "acetaminophen", conceptSet = codelist
)
cdm$target <- cdm$acetaminophen |>
  DrugUtilisation::requirePriorDrugWashout(days = 365, name = "target") |>
  DrugUtilisation::requireObservationBeforeDrug(days = 365)

cdm$conditions <- CohortConstructor::conceptCohort(
  cdm = cdm, name = "conditions", conceptSet = codelistConditions
)

overlap <- CohortCharacteristics::summariseCohortOverlap(cdm$conditions)
timing <- CohortCharacteristics::summariseCohortTiming(cdm$conditions)
counts <- CohortCharacteristics::summariseCohortCount(cdm$target)
counts2 <- CohortCharacteristics::summariseCohortCount(cdm$conditions)
attrition <- CohortCharacteristics::summariseCohortAttrition(cdm$target)
attrition2 <- CohortCharacteristics::summariseCohortAttrition(cdm$conditions)
characteristics <- cdm$target |>
  CohortCharacteristics::summariseCharacteristics(
    cohortIntersectFlag = list(
      "Conditions prior year" = list(
        targetCohortTable = "conditions", window = c(-Inf, 0)
      )
    )
  )
treatmentPersistence <- DrugUtilisation::summariseProportionOfPatientsCovered(cdm$target)
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  ageGroup = list(
    c(0, 150), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)
  ),
  daysPriorObservation = 365,
  sex = c("Both", "Male", "Female"),
  cohortDateRange = as.Date(c("2000-01-01", "2009-12-31"))
)
incidence <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = "acetaminophen",
  interval = "years", outcomeWashout = 365
)
pointPrevalence <- IncidencePrevalence::estimatePointPrevalence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = "acetaminophen",
  interval = "years"
)

op <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

omopViewerResults <- omopgenerics::bind(
  snapshot, op, overlap, counts, counts2, attrition, attrition2, characteristics,
  timing, treatmentPersistence, incidence, pointPrevalence
) |>
  omopgenerics::suppress()

usethis::use_data(omopViewerResults, overwrite = TRUE, internal = FALSE)
