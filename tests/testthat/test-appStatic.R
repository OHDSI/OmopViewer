test_that("logo", {
  tdir <- tempdir()

  # test no logo
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(), directory = tdir, logo = NULL
  ))
  expect_true("shiny" %in% list.files(tdir))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # test keywords
  for (key in logoKeywords) {
    expect_identical(basename(logoPath(key)), paste0(key, "_logo.svg"))
    expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir, logo = key))
    expect_true("shiny" %in% list.files(tdir))
    unlink(file.path(tdir, "shiny"), recursive = TRUE)
  }

  # custom logo
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(),
    directory = tdir,
    logo = system.file("oxford.png", package = "OmopViewer")
  ))
  expect_true("shiny" %in% list.files(tdir))
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})

test_that("empty shiny", {
  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = emptySummarisedResult(),directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_snapshot(cat(server, sep = "\n"))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})

test_that("title", {
  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(), directory = tdir, title = "example"
  ))

  # check shiny is created
  expect_true("shiny" %in% list.files(tdir))

  # snapshot for ui
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
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
  expect_no_error(exportStaticApp(
    result = result,
    directory = tdir,
    panelStructure = list(
      "summarise_cohort_count",
      "summarise_cohort_overlap",
      "summarise_cohort_attrition"
    )
  ))

  # snapshot ui
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # expect warning if panel is not present
  expect_warning(exportStaticApp(
    result = result,
    directory = tdir,
    panelStructure = list(
      "summarise_cohort_count", "summarise_cohort_attrition",
      "summarise_cohort_overlap", "not an option",
      "another missing result"
    )
  ))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # expect warning if panel is not present
  expect_warning(exportStaticApp(
    result = result, directory = tdir, panelStructure = list("not an option")
  ))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # menu with results not in result
  expect_warning(exportStaticApp(
    result = result,
    directory = tdir,
    panelStructure = list(
      "CHARACTERISTICS" = c("summarise_characteristics", "summarise_large_scale_characteristics", "hi"),
      "summarise_cohort_overlap"
    )
  ))

  # snapshot ui
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # menu for details
  expect_no_error(exportStaticApp(
    result = result,
    directory = tdir,
    panelStructure = list(
      "DETAILS" = c("summarise_cohort_count", "summarise_cohort_attrition"),
      "summarise_cohort_overlap"
    ),
    panelDetails = list(
      "summarise_cohort_attrition" = list(
        result_type = "summarise_cohort_attrition",
        title = "Attrition"
      ),
      "summarise_cohort_overlap" = list(
        result_type = "summarise_cohort_overlap",
        title = "Overlap"
      )
    )
  ))

  # snapshot ui
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # expect error if it is not a list
  expect_error(exportStaticApp(result = result, directory = tdir, panelStructure = c("must be a list")))

  # expect error if duplicated elements
  expect_error(exportStaticApp(result = result, directory = tdir, panelStructure = list(
    "summarise_cohort_overlap", "summarise_cohort_overlap")))
})

test_that("theme", {
  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(),
    directory = tdir,
    theme = NULL,
    open = FALSE
  ))
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))
  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})

test_that("check preprocess file works", {
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
      CohortCharacteristics::summariseCohortTiming()
  )

  # with no panelDetails
  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = result, directory = tdir, open = FALSE
  ))
  expect_true(dir.exists(file.path(tdir, "shiny")))
  expect_true(file.exists(file.path(tdir, "shiny", "data", "preprocess.R")))
  expect_true(file.exists(file.path(tdir, "shiny", "data", "shinyData.RData")))
  load(file.path(tdir, "shiny", "data", "shinyData.RData"))
  savedData <- data
  savedFilterValues <- filterValues
  # delete shinyData
  unlink(file.path(tdir, "shiny", "data", "shinyData.RData"))
  expect_false(file.exists(file.path(tdir, "shiny", "data", "shinyData.RData")))
  # create same file file with
  currentDirectory <- getwd()
  setwd(file.path(tdir, "shiny"))
  source(file.path(tdir, "shiny", "data", "preprocess.R"))
  setwd(currentDirectory)
  expect_true(file.exists(file.path(tdir, "shiny", "data", "shinyData.RData")))
  load(file.path(tdir, "shiny", "data", "shinyData.RData"))
  # settings are in different order
  removeSettings <- function(x) {
    purrr::map(x, \(x) {
      attr(x, "settings") <- NULL
      return(x)
    })
  }
  expect_identical(removeSettings(savedData), removeSettings(data))
  expect_identical(savedFilterValues, filterValues)
  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

})
