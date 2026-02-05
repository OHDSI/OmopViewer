# default set of results ----
install.packages("pak")
install.packages("renv")
install.packages("here")

# install needed packages
deps <- sort(unique(renv::dependencies(here::here("data-raw", "results.R"))$Package))
message("Installing dependencies: ", paste(deps, collapse = ", "))
pak::pak(deps)
pak::pkg_install("ohdsi/OmopConstructor@build_achilles")

logFile <- tempfile(fileext = ".txt")
omopgenerics::createLogFile(logFile = logFile)

# cdm reference
omopgenerics::logMessage("Create cdm_reference")
dbName <- "synthea-covid19-200k"
path <- here::here("extras", "omock")
dir.create(path = path)
Sys.setenv("MOCK_DATASETS_FOLDER" = path)
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

omopgenerics::logMessage("Copy cdm to duckdb")

# copy cdm
dbdir <- here::here("extras", "test.duckdb")
con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = dbdir))
src <- CDMConnector::dbSource(con = con, writeSchema = "main")

# copy cdm to
cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = src)
unlink(x = path, recursive = TRUE)
DBI::dbExecute(conn = con, statement = "PRAGMA memory_limit='2GB';")

# achilles tables
omopgenerics::logMessage("Create Achilles tables")
cdm <- OmopConstructor::buildAchilles(cdm, "minimal")

# create cohorts
omopgenerics::logMessage("Create cohorts of interest")
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
omopgenerics::logMessage("Retrieve snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm = cdm)

omopgenerics::logMessage("Summarise observation_period")
obsPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

omopgenerics::logMessage("Summarise clinical records")
clinicalTables <- OmopSketch::summariseClinicalRecords(
  cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence")
)

omopgenerics::logMessage("Summarise missing data")
missingData <- OmopSketch::summariseMissingData(cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence"))

omopgenerics::logMessage("Summarise record count")
recordCount <- OmopSketch::summariseRecordCount(
  cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence"),
  interval = "years"
)

omopgenerics::logMessage("Summarise in observation")
inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, interval = "years")

# CodelistGenerator
omopgenerics::logMessage("Summarise orphan codes")
orphanCodes <- CodelistGenerator::summariseOrphanCodes(codelistConditions, cdm = cdm)

omopgenerics::logMessage("Summarise cohort code use")
cohortCodeUse <- CodelistGenerator::summariseCohortCodeUse(codelistConditions, cdm = cdm, "conditions")

omopgenerics::logMessage("Summarise code use")
codeUse <- CodelistGenerator::summariseCodeUse(codelistConditions, cdm = cdm)

omopgenerics::logMessage("Summarise achilles code use")
achillesUse <- CodelistGenerator::summariseAchillesCodeUse(codelistConditions, cdm = cdm)

omopgenerics::logMessage("Summarise unmapped codes")
unmapped <- CodelistGenerator::summariseUnmappedCodes(codelistConditions, cdm = cdm)

# CohortCharacteristics
omopgenerics::logMessage("Summarise overlap")
overlap <- CohortCharacteristics::summariseCohortOverlap(cdm$conditions)

omopgenerics::logMessage("Summarise timing")
timing <- CohortCharacteristics::summariseCohortTiming(cdm$conditions)

omopgenerics::logMessage("Summarise cohort counts")
counts <- CohortCharacteristics::summariseCohortCount(cdm$target)

omopgenerics::logMessage("Summarise cohort attrition")
attrition <- CohortCharacteristics::summariseCohortAttrition(cdm$target)

omopgenerics::logMessage("Summarise characteristics")
characteristics <- cdm$target |>
  CohortCharacteristics::summariseCharacteristics(
    cohortIntersectFlag = list(
      "Conditions prior year" = list(
        targetCohortTable = "conditions", window = c(-Inf, 0)
      )
    )
  )

omopgenerics::logMessage("Summarise Large Scale Characteristics")
lsc <- cdm$target |>
  CohortCharacteristics::summariseLargeScaleCharacteristics(
    window = list(c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365)),
    eventInWindow = c("observation", "condition_occurrence"),
    episodeInWindow = "drug_exposure"
  )

# IncidencePrevalence
omopgenerics::logMessage("Create denominator cohort")
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

omopgenerics::logMessage("Estimate incidence")
incidence <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = "acetaminophen",
  interval = "years", outcomeWashout = 365
)

omopgenerics::logMessage("Estimate point prevalence")
pointPrevalence <- IncidencePrevalence::estimatePointPrevalence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = "acetaminophen",
  interval = "years"
)

# DrugUtilisation
omopgenerics::logMessage("Summarise proportion of patients covered")
treatmentPersistence <- DrugUtilisation::summariseProportionOfPatientsCovered(cdm$target)

omopgenerics::logMessage("Summarise dose coverage for acetaminophen")
doseCoverage <- DrugUtilisation::summariseDoseCoverage(cdm = cdm, ingredientConceptId = 1125315)

omopgenerics::logMessage("Summarise indication")
indication <- DrugUtilisation::summariseIndication(
  cdm$acetaminophen,
  indicationCohortName = "conditions",
  indicationWindow = list(c(-30, 0), c(0, 0)),
  unknownIndicationTable = "condition_occurrence",
  mutuallyExclusive = FALSE
)

omopgenerics::logMessage("Summarise drug use")
drugUtilisation <- DrugUtilisation::summariseDrugUtilisation(
  cdm$acetaminophen, ingredientConceptId = 1125315
)

omopgenerics::logMessage("Summarise drug restart")
drugRestart <- cdm$acetaminophen |>
  DrugUtilisation::summariseDrugRestart(
    switchCohortTable = "alternative",
    followUpDays = c(180, 365, Inf)
  )

omopgenerics::logMessage("Summarise alternative treatments over windows")
treatments <- cdm$acetaminophen |>
  DrugUtilisation::summariseTreatment(
    window = list(c(-365, -181), c(-180, -1), c(0, 0), c(1, 180), c(181, 365)),
    treatmentCohortName = "alternative"
  )

# cohort survival
omopgenerics::logMessage("Instantiate death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm = cdm, name = "death_cohort")

omopgenerics::logMessage("Single event survival")
survivalSingle <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "acetaminophen",
  outcomeCohortTable = "alternative",
  censorOnCohortExit = FALSE,
  followUpDays = 365
)

omopgenerics::logMessage("Competing risk survival")
survivalCompetting <- CohortSurvival::estimateCompetingRiskSurvival(
  cdm = cdm,
  targetCohortTable = "acetaminophen",
  outcomeCohortTable = "alternative",
  outcomeCohortId = 2,
  competingOutcomeCohortTable = "death_cohort",
  followUpDays = 365
)

# MeasurementDiagnostics
omopgenerics::logMessage("Run measurement diagnostics for concept")
cdm <- MeasurementDiagnostics::mockMeasurementDiagnostics(nPerson = 1000)
measurementUse <- MeasurementDiagnostics::summariseMeasurementUse(
  cdm = cdm,
  codes = list("test_codelist" = c(3001467L, 45875977L))
)
omopgenerics::logMessage("Run measurement diagnostics for cohort")
measurementCohortUse <- MeasurementDiagnostics::summariseCohortMeasurementUse(
  codes = list("test_codelist" = c(3001467L, 45875977L)),
  cohort = cdm$my_cohort,
  timing = "cohort_start_date"
)

# export log file
logSummary <- omopgenerics::summariseLogFile(
  logFile = logFile,
  cdmName = omopgenerics::cdmName(cdm)
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
  treatmentPersistence, doseCoverage, indication, drugUtilisation, drugRestart, treatments,
  # CohortSurvival
  survivalSingle, survivalCompetting,
  # MeasurementDiagnostics
  measurementUse, measurementCohortUse,
  # log
  logSummary
) |>
  omopgenerics::suppress()

usethis::use_data(omopViewerResults, overwrite = TRUE, internal = FALSE)
