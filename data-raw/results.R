# default set of results ----
install.packages("pak")
install.packages("renv")

# install needed packages
deps <- sort(unique(renv::dependencies(here::here("data-raw", "results.R"))$Package))
message("Installing dependencies: ", paste(deps, collapse = ", "))
pak::pak(deps, upgrade = FALSE)

# cdm reference
dbName <- "synthea-covid19-200k"
cdm <- omock::mockCdmFromDataset(datasetName = dbName)

# modify cdm
set.seed(12345)
cdm$drug_exposure <- cdm$drug_exposure |>
  dplyr::mutate(
    quantity = runif(n = dplyr::n()),
    quantity = dplyr::case_when(
      .data$quantity >= 0   & .data$quantity < 0.2 ~ 1,
      .data$quantity >= 0.2 & .data$quantity < 0.4 ~ 2,
      .data$quantity >= 0.4 & .data$quantity < 0.6 ~ 7,
      .data$quantity >= 0.6 & .data$quantity < 0.8 ~ 30,
      .data$quantity >= 0.8 & .data$quantity < 1   ~ 50
    )
  )
cdm$condition_occurrence <- cdm$condition_occurrence |>
  dplyr::mutate(condition_concept_id = dplyr::if_else(
    .data$condition_occurrence_id %% 10 & .data$condition_concept_id == 381316L,
    0L, .data$condition_concept_id
  ))

# copy cdm
src <- CDMConnector::dbSource(con = duckdb::dbConnect(drv = duckdb::duckdb()), writeSchema = "main")

# copy cdm to
cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = src)

# achilles tables
cdm <- CodelistGenerator::buildAchillesTables(cdm)

# create cohorts
codelistConditions <- list(
  coronary_arteriosclerosis = 381316L,
  covid_19 = 37311061L,
  fever = 437663L,
  cough = 254761L,
  fatigue = 4223659L,
  respiratory_distress = 4158346L
)
cdm$conditions <- CohortConstructor::conceptCohort(
  cdm = cdm, conceptSet = codelistConditions, name = "conditions"
)
codelist <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm, name = "acetaminophen", nameStyle = "{concept_name}"
)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm, name = "acetaminophen", conceptSet = codelist
)
cdm$target <- cdm$acetaminophen |>
  DrugUtilisation::requirePriorDrugWashout(days = 365, name = "target") |>
  DrugUtilisation::requireObservationBeforeDrug(days = 365)
cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "alternative",
  ingredient = c("warfarin", "simvastatin", "enoxaparin", "verapamil")
)

# OmopSketch
snapshot <- OmopSketch::summariseOmopSnapshot(cdm = cdm)
obsPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period)
clinicalTables <- OmopSketch::summariseClinicalRecords(
  cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence")
)
missingData <- OmopSketch::summariseMissingData(cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence"))
recordCount <- OmopSketch::summariseRecordCount(
  cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence"),
  interval = "years"
)
inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, interval = "years")

# CodelistGenerator
orphanCodes <- CodelistGenerator::summariseOrphanCodes(codelistConditions, cdm = cdm)
cohortCodeUse <- CodelistGenerator::summariseCohortCodeUse(codelistConditions, cdm = cdm, "conditions")
codeUse <- CodelistGenerator::summariseCodeUse(codelistConditions, cdm = cdm)
achillesUse <- CodelistGenerator::summariseAchillesCodeUse(codelistConditions, cdm = cdm)
unmapped <- CodelistGenerator::summariseUnmappedCodes(codelistConditions, cdm = cdm)

# CohortCharacteristics
overlap <- CohortCharacteristics::summariseCohortOverlap(cdm$conditions)
timing <- CohortCharacteristics::summariseCohortTiming(cdm$conditions)
counts <- CohortCharacteristics::summariseCohortCount(cdm$target)
attrition <- CohortCharacteristics::summariseCohortAttrition(cdm$target)
characteristics <- cdm$target |>
  CohortCharacteristics::summariseCharacteristics(
    cohortIntersectFlag = list(
      "Conditions prior year" = list(
        targetCohortTable = "conditions", window = c(-Inf, 0)
      )
    )
  )
lsc <- cdm$target |>
  CohortCharacteristics::summariseLargeScaleCharacteristics(
    window = list(c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365)),
    eventInWindow = c("observation", "condition_occurrence"),
    episodeInWindow = "drug_exposure"
  )

# IncidencePrevalence
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

# DrugUtilisation
treatmentPersistence <- DrugUtilisation::summariseProportionOfPatientsCovered(cdm$target)
doseCoverage <- DrugUtilisation::summariseDoseCoverage(cdm = cdm, ingredientConceptId = 1125315)
indication <- DrugUtilisation::summariseIndication(
  cdm$acetaminophen,
  indicationCohortName = "conditions",
  indicationWindow = list(c(-30, 0), c(0, 0)),
  unknownIndicationTable = "condition_occurrence",
  mutuallyExclusive = FALSE
)
drugUtilisation <- DrugUtilisation::summariseDrugUtilisation(
  cdm$acetaminophen, ingredientConceptId = 1125315
)
drugRestart <- cdm$acetaminophen |>
  DrugUtilisation::summariseDrugRestart(
    switchCohortTable = "alternative",
    followUpDays = c(180, 365, Inf)
  )
treatments <- cdm$acetaminophen |>
  DrugUtilisation::summariseTreatment(
    window = list(c(-365, -181), c(-180, -1), c(0, 0), c(1, 180), c(181, 365)),
    treatmentCohortName = "alternative"
  )

omopViewerResults <- omopgenerics::bind(
  # OmopSketch
  snapshot, obsPeriod, clinicalTables, missingData, recordCount, inObservation,
  # CodelistGenerator
  orphanCodes, cohortCodeUse, codeUse, achillesUse, unmapped,
  # CohortCharacteristics
  overlap, counts, attrition, characteristics, timing, lsc,
  # IncidencePrevalence
  incidence, pointPrevalence,
  # DrugUtilisation
  treatmentPersistence, doseCoverage, indication, drugUtilisation, drugRestart, treatments
) |>
  omopgenerics::suppress()

usethis::use_data(omopViewerResults, overwrite = TRUE, internal = FALSE)
