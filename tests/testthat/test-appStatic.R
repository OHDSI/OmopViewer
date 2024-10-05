test_that("logo", {
  #tdir <- here::here()
  tdir <- tempdir()

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
  #tdir <- here::here()
  tdir <- tempdir()
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
      PatientProfiles::addSex(name = "cohort") |>
      CohortCharacteristics::summariseLargeScaleCharacteristics(
        strata = list("sex", "age_group", c("age_group", "sex")),
        eventInWindow = "condition_occurrence",
        episodeInWindow = "drug_exposure"
      )
  )

  # generate shiny
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = FALSE))
  expect_true("shiny" %in% list.files(tdir))
  panels <- validatePanels(list(), getChoices(result))
  expect_snapshot(uiStatic(choices = panels$choices) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(resultTypes = panels$result_order) |> cat(sep = "\n"))
  x <- readLines(file.path(tdir, "shiny/global.R"))
  expect_snapshot(cat(x, sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # use summary + test overwrite (question when interactive and overwrite by default when not)
  expect_no_error(exportStaticApp(result = result, directory = tdir, summary = TRUE))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(choices = panels$choices, summary = capture.output(summary(result), type = "message"), logo = NULL) |> cat(sep = "\n"))
  expect_snapshot(uiStatic(choices = panels$choices, summary = capture.output(summary(result), type = "message"), logo = "HDS") |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  PatientProfiles::mockDisconnect(cdm)
})

test_that("title", {
  #tdir <- here::here()
  tdir <- tempdir()
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
  tdir <- tempdir()
  unique(omopgenerics::settings(result) |>
           dplyr::pull("result_type"))
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                "summarise_cohort_attrition",
                                                "summarise_cohort_overlap")))
  panels <- validatePanels(list("summarise_cohort_count",
                                "summarise_cohort_attrition",
                                "summarise_cohort_overlap") , getChoices(result))
  expect_snapshot(uiStatic(choices = panels$choices))

  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  expect_warning(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                "summarise_cohort_attrition",
                                                "summarise_cohort_overlap",
                                                "not an option",
                                                "another missing result")))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  expect_warning(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("not an option")))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # missing a result
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("summarise_cohort_count",
                                                "summarise_cohort_attrition")))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # will show all results
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list()))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # menu with results not in result
  expect_warning(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("CHARACTERISTICS" = c("summarise_characteristics", "summarise_large_scale_characteristics", "hi"), "summarise_cohort_overlap")))
  expect_warning(panels <- validatePanels(list("CHARACTERISTICS" = c("summarise_characteristics", "summarise_large_scale_characteristics", "hi"), "summarise_cohort_overlap"), getChoices(result)))
  expect_snapshot(uiStatic(choices = panels$choices))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  # menu for details
  expect_no_error(exportStaticApp(result = result,
                                  directory = tdir,
                                  panels = list("DETAILS" = c("summarise_cohort_count", "summarise_cohort_attrition"), "summarise_cohort_overlap")))
  panels <- validatePanels(list("DETAILS" = c("summarise_cohort_count", "summarise_cohort_attrition"), "summarise_cohort_overlap"), getChoices(result))
  expect_snapshot(uiStatic(choices = panels$choices))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)

  expect_error(exportStaticApp(result = result,
                               directory = tdir,
                               panels = c("must be a list")))
  expect_error(validatePanels(list("summarise_cohort_count", "summarise_cohort_count"), getChoices(result)))
})

test_that("theme", {
  #tdir <- here::here()
  tdir <- tempdir()

  expect_no_error(exportStaticApp(result = emptySummarisedResult(), directory = tdir, theme = NULL, open = FALSE))

  ui <- uiStatic(theme = "bslib::bs_theme(bg = '#bb0a1e', fg = '#0000ff')")

  expect_true(grepl('theme = bslib::bs_theme\\(bg = "#bb0a1e", fg = "#0000ff"\\)', ui[2]))
})
