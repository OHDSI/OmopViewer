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

  # url logo
  tdir <- file.path(tempdir(), "ov_test")
  dir.create(tdir, showWarnings = FALSE)
  expect_no_error(theme <- validateTheme("darwin"))
  expect_no_error(logo <- validateLogo(logo = NULL, theme = theme))
  expect_identical(copyLogos(logo = logo, directory = tdir), "logo.png")
  expect_true("www/logo.png" %in% list.files(tdir, recursive = TRUE))
  unlink(tdir, recursive = TRUE)
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

  # check includeOneChoiceFilters
  pd1 <- panelDetailsFromResult(result)
  pd2 <- panelDetailsFromResult(result, includeOneChoiceFilters = TRUE)
  # check default
  expect_identical(pd1, pd2)
  # no filter is excluded
  expect_identical(pd2$summarise_cohort_overlap$exclude_filters, NULL)
  pd3 <- panelDetailsFromResult(result, includeOneChoiceFilters = FALSE)
  # one option length are trimmed
  expect_identical(
    pd3$summarise_cohort_overlap$exclude_filters,
    c("cdm_name", "variable_level", "overlap_by")
  )

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
  expect_identical(
    bslib::bs_theme(brand = validateTheme(NULL)),
    getThemes()[["default"]]
  )

  # us a pre build theme
  expect_identical(
    bslib::bs_theme(brand = validateTheme("sad_robot")),
    getThemes()[["sad_robot"]]
  )

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

test_that("survival panel creates custom static app", {
  result <- dplyr::tibble(
    cdm_name = "mock",
    target_cohort = "target",
    sex = "overall",
    age_group = "overall",
    reason = "Initial",
    variable_name = "survival",
    variable_level = "death_cohort",
    estimate = 1,
    result_type = c(
      "survival_summary",
      "survival_estimates",
      "survival_events",
      "survival_attrition"
    ),
    analysis_type = "single_event",
    censor_on_cohort_exit = "TRUE",
    competing_outcome = "none",
    eventgap = "0",
    follow_up_days = "365",
    minimum_survival_days = "0",
    outcome = "death_cohort",
    outcome_date_variable = "cohort_start_date",
    outcome_washout = "0",
    restricted_mean_follow_up = "365"
  ) |>
    omopgenerics::transformToSummarisedResult(
      group = "target_cohort",
      strata = c("sex", "age_group", "reason"),
      settings = c(
        "result_type",
        "analysis_type",
        "censor_on_cohort_exit",
        "competing_outcome",
        "eventgap",
        "follow_up_days",
        "minimum_survival_days",
        "outcome",
        "outcome_date_variable",
        "outcome_washout",
        "restricted_mean_follow_up"
      ),
      estimates = "estimate"
    )

  panelDetails <- panelDetailsFromResult(result)
  expect_identical(names(panelDetails), "survival")
  expect_identical(
    names(panelDetails$survival$content),
    c("table_survival", "table_events", "table_attrition", "plot_survival")
  )

  tdir <- tempdir()
  expect_no_error(exportStaticApp(result = result, directory = tdir, open = FALSE))
  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  server <- readLines(file.path(tdir, "shiny", "server.R"))

  expect_no_error(parse(file.path(tdir, "shiny", "ui.R")))
  expect_no_error(parse(file.path(tdir, "shiny", "server.R")))
  expect_true(any(grepl("survival_active_tab", ui, fixed = TRUE)))
  expect_true(any(grepl("Table Attrition", ui, fixed = TRUE)))
  expect_true(any(grepl("survival_plot_survival_colour", ui, fixed = TRUE)))
  expect_true(any(grepl("appliedSurvivalInputs", server, fixed = TRUE)))
  expect_true(any(grepl("cumulativeFailure = isCompetingRisk", server, fixed = TRUE)))

  partial <- result |>
    omopgenerics::filterSettings(
      .data$result_type %in% c("survival_summary", "survival_estimates")
    )
  partialDetails <- panelDetailsFromResult(partial)
  expect_identical(
    names(partialDetails$survival$content),
    c("table_survival", "plot_survival")
  )

  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})
