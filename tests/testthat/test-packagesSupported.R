test_that("CohortCharacteristics shiny", {
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

test_that("CodelistGenerator shiny", {
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

