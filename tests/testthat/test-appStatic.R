test_that("logo", {
  tdir <- here::here()
  expect_warning(exportStaticApp(directory = tdir, logo = NULL))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  expect_no_error(exportStaticApp(directory = tdir, logo = "HDS"))
  expect_no_error(exportStaticApp(directory = tdir, logo = "OHDSI"))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  expect_no_error(exportStaticApp(
    directory = tdir, logo = here::here("inst", "images", "testim.png")))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  expect_snapshot(
    uiStatic(logo = "my_pic.png") |> cat(sep = "\n"))
})

test_that("empty shiny", {
  tdir <- here::here()
  expect_no_error(exportStaticApp(directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic() |> cat(sep = "\n"))
  expect_snapshot(serverStatic() |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
})

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

  # generate shiny
  tdir <- here::here()
  expect_no_error(exportStaticApp(result = result, directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(choices = getChoices(result)) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(resultTypes = names(getChoices(result))) |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})
