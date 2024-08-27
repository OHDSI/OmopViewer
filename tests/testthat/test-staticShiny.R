test_that("empty shiny", {
  tdir <- here::here()
  expect_no_error(exportStaticApp(directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(asText = TRUE) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(asText = TRUE) |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
})

test_that("CohortCharacteristics shiny", {
  # create mock cdm
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
      CohortCharacteristics::summariseCohortCount(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortOverlap(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortTiming(),
    cdm$cohort |>
      PatientProfiles::addAge(ageGroup = list(c(0, 44), c(45, Inf))) |>
      PatientProfiles::addSex() |>
      CohortCharacteristics::summariseLargeScaleCharacteristics(
        strata = list("sex", "age_group", c("age_group", "sex")),
        eventInWindow = "condition_occurrence",
        episodeInWindow = "drug_exposure"
      )
  )

  # Update settings so there is no overlap between groupping and settings
  set <- omopgenerics::settings(result) |>
    dplyr::select(-"cohort_name")
  result <- result |>
    omopgenerics::newSummarisedResult(settings = set)

  # generate shiny
  tdir <- here::here()
  expect_no_error(exportStaticApp(result = result, directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(result = result, asText = TRUE) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(result = result, asText = TRUE) |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})
