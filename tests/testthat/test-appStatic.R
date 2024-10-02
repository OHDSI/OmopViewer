test_that("logo", {
  tdir <- here::here()

  # test no logo
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(), directory = tdir, logo = NULL
  ))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # test keywords
  for (key in logoKeywords) {
    expect_identical(basename(logoPath(key)), paste0(key, "_logo.svg"))
    expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, logo = key))
    expect_true("shiny" %in% list.files(tdir))
    unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  }

  # custom logo
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),
    directory = tdir, logo = here::here("inst", "oxford.png")))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # test generated ui
  expect_snapshot(uiStatic(logo = "my_pic.png") |> cat(sep = "\n"))
})

test_that("empty shiny", {
  tdir <- here::here()
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir))
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
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(choices = getChoices(result)) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(resultTypes = names(getChoices(result))) |> cat(sep = "\n"))

  # use summary + test overwrite (question when interactive and overwrite by default when not)
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = TRUE))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(choices = getChoices(result), summary = capture.output(summary(result), type = "message"), logo = NULL) |> cat(sep = "\n"))
  expect_snapshot(uiStatic(choices = getChoices(result), summary = capture.output(summary(result), type = "message"), logo = "HDS") |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("background", {
  full <- c(
    "header" = "Abstract",
    "title" = "**Introduction**",
    "body" = "Example of an [introduction](https://github.com/oxford-pharmacoepi/omopViewer).",
    "title" = "Methods",
    "body" = "Methods example, with a footer* example.",
    "footer" = "*Here is the footer."
  )
  tdir <- here::here()
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, logo = NULL, background = NULL))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  expect_snapshot(createBackground(full))
  # with logo
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, logo = "OHDSI", background = NULL))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  expect_snapshot(createBackground(full, "OHDSI"))
  # no background
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, logo = "OHDSI", background = NULL))
  expect_true("shiny" %in% list.files(tdir))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
  expect_null(createBackground(NULL, "OHDSI"))

  # expected behaviour
  expect_warning(x <- validateBackground("bslib::hola("))
  expect_null(x)
  expect_warning(x <- validateBackground(c("hi" = "drop", "title" = "keep")))
  expect_equal(x, c("title" = "keep"))
})

test_that("title", {
  tdir <- here::here()
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, title = "example"))
  expect_true("shiny" %in% list.files(tdir))
  x <- readLines(file.path(tdir, "shiny/ui.R"))
  expect_snapshot(cat(x, sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
})

test_that("order tabs", {
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
      CohortCharacteristics::summariseCohortAttrition(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortCount(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortOverlap()
  )

  # generate shiny
  tdir <- here::here()
  unique(omopgenerics::settings(result) |>
    dplyr::pull("result_type"))
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                  "summarise_cohort_attrition",
                                                  "summarise_cohort_overlap")))


  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                  "summarise_cohort_attrition",
                                                  "summarise_cohort_overlap",
                                                  "not an option",
                                                  "another missing result")))
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("not an option")))
  # missing a result
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                  "summarise_cohort_attrition")))

  # will show all results
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list()))

  expect_error(exportStaticApp(result = result,
                               directory = tdir,
                               panels = c("must be a list")))


})
