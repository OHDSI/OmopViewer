test_that("IncidencePrevalence shiny", {
  skip_on_cran()
  cdm <- omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(recordPerson = 0.5)

  con <- duckdb::dbConnect(duckdb::duckdb())
  cdm <- CDMConnector::copyCdmTo(con = con, cdm = cdm, schema = "main")

  ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", ageGroup = ageGroup,
    sex = c("Male", "Female", "Both")
  )

  # mock results
  result <- omopgenerics::bind(
    IncidencePrevalence::estimateIncidence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "cohort",
      outcomeWashout = c(0, 365, Inf)
    ),
    IncidencePrevalence::estimatePeriodPrevalence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "cohort"
    ),
    IncidencePrevalence::estimatePointPrevalence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "cohort"
    )
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = TRUE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # test server snapshot
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))

  # test global snapshot
  global <- readLines(file.path(tdir, "shiny", "global.R"))
  expect_snapshot(cat(global, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # test summary = FALSE
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("DrugUtilisation shiny", {
  skip_on_cran()
  skip_if(Sys.getenv("EUNOMIA_DATA_FOLDER") == "")
  con <- duckdb::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(
    con = con, cdmSchema = "main", writeSchema = "main")

  codes <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm, name = "acetaminophen")
  ingredient <- 1125315

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm, name = "my_cohort", conceptSet = codes, gapEra = 30
  )

  codes <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm, name = c("amoxicillin", "morphine", "warfarin"))
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm, name = "switch_cohort", conceptSet = codes, gapEra = 30
  )

  codes <- list(
    sinusitis = c(4294548, 40481087, 4283893, 257012),
    bronchitis = c(260139, 258780)
  )
  cdm$indication <- CohortConstructor::conceptCohort(
    cdm = cdm, conceptSet = codes, name = "indication"
  )

  ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))

  cdm$my_cohort <- cdm$my_cohort |>
    PatientProfiles::addDemographics(
      age = FALSE,
      ageGroup = ageGroup,
      sex = TRUE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = "my_cohort"
    )

  strata <- omopgenerics::combineStrata(c("age_group", "sex"))

  # mock results
  result <- omopgenerics::bind(
    DrugUtilisation::summariseDoseCoverage(cdm = cdm, ingredientConceptId = ingredient),
    cdm$my_cohort |>
      DrugUtilisation::summariseDrugRestart(
        switchCohortTable = "switch_cohort",
        strata = strata,
        restrictToFirstDiscontinuation = TRUE
      ),
    cdm$my_cohort |>
      DrugUtilisation::summariseDrugUtilisation(
        strata = strata, ingredientConceptId = ingredient
      ),
    cdm$my_cohort |>
      DrugUtilisation::summariseIndication(
        indicationCohortName = "indication",
        indicationWindow = list(c(-Inf, 0), c(-30, 0), c(0, 0)),
        unknownIndicationTable = "condition_occurrence"
      ),
    cdm$my_cohort |>
      DrugUtilisation::summariseProportionOfPatientsCovered(strata = strata),
    cdm$my_cohort |>
      DrugUtilisation::summariseTreatment(
        window = list(c(-Inf, -1), c(0, 0), c(1, 365), c(366, Inf)),
        treatmentCohortName = "switch_cohort",
        strata = strata
      )
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = TRUE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # test server snapshot
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))

  # test global snapshot
  global <- readLines(file.path(tdir, "shiny", "global.R"))
  expect_snapshot(cat(global, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # test summary = FALSE
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("CohortCharacteristics shiny", {
  skip_on_cran()
  # create mock cdm
  set.seed(123456)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence(recordPerson = 3) |>
    omock::mockDrugExposure(recordPerson = 4.5) |>
    omock::mockCohort(
      numberCohorts = 3, cohortName = c("covid", "tb", "asthma"))

  # TO BE REMOVED WHEN CohortCharacteristics works with local cdms
  cdm <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main")

  # generate result set
  result <- omopgenerics::bind(
    cdm$cohort |>
      CohortCharacteristics::summariseCharacteristics(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortAttrition(),
    cdm$cohort |>
      PatientProfiles::addAge(ageGroup = list(c(0, 44), c(45, Inf))) |>
      PatientProfiles::addSex(name = "cohort") |>
      CohortCharacteristics::summariseCohortCount(
        strata = list("sex", "age_group", c("age_group", "sex"))
      ),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortOverlap(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortTiming(),
    cdm$cohort |>
      CohortCharacteristics::summariseLargeScaleCharacteristics(
        eventInWindow = "condition_occurrence",
        episodeInWindow = "drug_exposure"
      )
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = TRUE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # test server snapshot
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))

  # test global snapshot
  global <- readLines(file.path(tdir, "shiny", "global.R"))
  expect_snapshot(cat(global, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # test summary = FALSE
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("OmopSketch shiny", {
  skip_on_cran()
  cdm <- OmopSketch::mockOmopSketch()

  # mock results
  result <- omopgenerics::bind(
    OmopSketch::summariseObservationPeriod(cdm$observation_period, sex = TRUE),
    OmopSketch::summariseOmopSnapshot(cdm)
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # test server snapshot
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))

  # test global snapshot
  global <- readLines(file.path(tdir, "shiny", "global.R"))
  expect_snapshot(cat(global, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("CodelistGenerator shiny", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  # test cdm object
  db <- RPostgres::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    writeSchema = c(
      schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"), prefix = "ov_"),
    achillesSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  # create mock result
  codes <- CodelistGenerator::getCandidateCodes(
    cdm = cdm,
    domains = "condition",
    keywords = "sinusitis",
    includeDescendants = FALSE
  )
  codelist <- list("sinusitis" = codes$concept_id)
  cdm$my_cohort <- CohortConstructor::conceptCohort(
    cdm = cdm, conceptSet = codelist, name = "my_cohort"
  )

  # mock results
  result <- omopgenerics::bind(
    codelist |>
      CodelistGenerator::summariseOrphanCodes(cdm = cdm),
    codelist |>
      CodelistGenerator::summariseCohortCodeUse(
        cdm = cdm, cohortTable = "my_cohort", timing = "entry"
      ),
    codelist |>
      CodelistGenerator::summariseCodeUse(cdm = cdm, byYear = TRUE, bySex = TRUE),
    codelist |>
      CodelistGenerator::summariseAchillesCodeUse(cdm = cdm)
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))

  # test ui snapshot
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # test server snapshot
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))

  # test global snapshot
  global <- readLines(file.path(tdir, "shiny", "global.R"))
  expect_snapshot(cat(global, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  CDMConnector::cdmDisconnect(cdm)
})
