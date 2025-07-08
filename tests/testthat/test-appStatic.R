test_that("logo", {
  tdir <- tempdir()

  # test no logo
  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    logo = NULL
  ))
  expect_true("shiny" %in% list.files(tdir))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  # test keywords
  for (key in logoKeywords) {
    expect_identical(basename(logoPath(key)), paste0(key, "_logo.svg"))
    expect_no_error(exportStaticApp(
      result = omopgenerics::emptySummarisedResult(),
      directory = tdir,
      logo = key
    ))
    expect_true("shiny" %in% list.files(tdir))
    unlink(file.path(tdir, "shiny"), recursive = TRUE)
  }

  # custom logo
  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
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
  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir
  ))
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
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    title = "example"
  ))

  # check shiny is created
  expect_true("shiny" %in% list.files(tdir))

  # snapshot for ui
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  expect_snapshot(cat(ui, sep = "\n"))

  # delete created shiny
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})

test_that("panelStructure argument works", {
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
      CohortCharacteristics::summariseCohortAttrition(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortCount(),
    cdm$cohort |>
      CohortCharacteristics::summariseCohortOverlap()
  )

  tdir <- tempdir()

  # default panelDetails
  panelDetails <- panelDetailsFromResult(result) |>
    populatePanelDetailsOptions(result)
  panels <- writeUiPanels(panelDetails, updateButtons = TRUE)

  # default panelStructure
  panelStructure <- as.list(names(panelDetails))
  uiPanels <- structurePanels(panels, panelStructure)
  order <- panels |>
    purrr::map_int(\(x) {
      stringr::str_locate(string = uiPanels, pattern = stringr::fixed(x)) |>
        dplyr::as_tibble() |>
        dplyr::pull("start")
    }) |>
    sort() |>
    names()
  expect_identical(unlist(panelStructure), order)

  # custom panel order
  panelStructure <- list("summarise_cohort_count", "summarise_cohort_overlap", "summarise_cohort_attrition")
  uiPanels <- structurePanels(panels, panelStructure)
  order <- panels |>
    purrr::map_int(\(x) {
      stringr::str_locate(string = uiPanels, pattern = stringr::fixed(x)) |>
        dplyr::as_tibble() |>
        dplyr::pull("start")
    }) |>
    sort() |>
    names()
  expect_identical(unlist(panelStructure), order)

  # dropdown menu
  panelStructure <- list(
    "Panel details" = c("summarise_cohort_count", "summarise_cohort_attrition"),
    "summarise_cohort_overlap"
  )
  uiPanels <- structurePanels(panels, panelStructure)
  order <- panels |>
    purrr::map_int(\(x) {
      stringr::str_locate(string = uiPanels, pattern = stringr::fixed(x)) |>
        dplyr::as_tibble() |>
        dplyr::pull("start")
    }) |>
    sort() |>
    names()
  expect_identical(unname(unlist(panelStructure)), order)
  expect_no_error(exportStaticApp(
    result = result,
    directory = tdir,
    panelDetails = panelDetailsFromResult(result),
    panelStructure = panelStructure
  ))
  expect_true("shiny" %in% list.files(tdir))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  panels <- c("summarise_cohort_count", "summarise_cohort_overlap", "summarise_cohort_attrition")

  # panel present in details but not in structure
  panelStructure <- list("summarise_cohort_count", "summarise_cohort_overlap")
  expect_warning(panelStructure <- validatePanelStructure(panelStructure, panels))
  expect_identical(panelStructure, as.list(panels))

  # panel present in structure but not in details
  panelStructure <- list("summarise_cohort_count", "summarise_cohort_overlap", "summarise_cohort_attrition", "not_present")
  expect_warning(panelStructure <- validatePanelStructure(panelStructure, panels))
  expect_identical(panelStructure, as.list(panels))

})

test_that("theme", {
  # no theme
  expect_identical(validateTheme(NULL), omopViewerThemes$default)

  # us a pre build theme
  expect_identical(validateTheme("sad_robot"), omopViewerThemes$sad_robot)

  # custom theme
  theme <- "bslib::bs_theme(bootswatch = 'sandstone',
    primary = '#605ca8',
    bg = 'white',
    fg = 'black',
    success = '#3B9AB2',
    base_font = bslib::font_google('Space Mono'),
    code_font = bslib::font_google('Space Mono'))"
  expect_identical(validateTheme(theme), theme)

  # not bslib call
  expect_error(validateTheme('bslib::accordion()'))
})

test_that("default panel", {
  result <- dplyr::tibble(
    cdm_name = "mock",
    cohort_name = "cohort 1",
    age_group = c(rep("overall", 2), rep("0 to 24", 2)),
    variable_name = c("concept 1", "concept 2", "concept 1", "concept 2"),
    variable_level = c("1", "2", "1", "2"),
    smd = c(0.001, 0.6, NA, 0.05),
    p = c(0.0001, 0.001, 0.01, 0.13),
    time = "years",
    result_type = "custom_result"
  ) |>
    omopgenerics::transformToSummarisedResult(
      group = "cohort_name",
      strata = "age_group",
      estimates = c("smd", "p"),
      settings = c("result_type", "time")
    )

  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

})
