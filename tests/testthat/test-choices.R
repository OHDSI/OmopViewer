test_that("test choices", {
  # empty result
  res <- omopgenerics::emptySummarisedResult()
  resultList <- purrr::map(panelDetailsFromResult(res), \(x) x$data)
  expect_no_error(x <- prepareResult(res, resultList))
  expected <- list()
  expect_equal(unname(x), expected)

  # check only settings
  res <- omopgenerics::emptySummarisedResult(settings = dplyr::tibble(
    result_id = c(1L, 2L),
    result_type = c("custom_result_1", "custom_result_2"),
    package_name = "OmopViewer",
    package_version = "1.0.0",
    param = c("TRUE", NA),
    x = c("0", "1")
  ))
  panelList <- list(my_result = list(result_id = c(1L, 2L)))
  expect_no_error(x <- getValues(res, panelList))
  expect_identical(x$my_result_cdm_name, character())
  expect_identical(x$my_result_variable_name, character())
  expect_identical(x$my_result_variable_level, character())
  expect_identical(x$my_result_estimate_name, character())
  expect_identical(x$my_result_param, "TRUE")
  expect_identical(x$my_result_x, c("0", "1"))

  # a set of summarised_results
  res <- omopgenerics::newSummarisedResult(
    x = dplyr::tibble(
      result_id = c(1L, 2L),
      cdm_name = c("cdm1", "cdm2"),
      group_name = "cohort_name",
      group_level = "cohort_1",
      strata_name = c("age &&& sex", "year"),
      strata_level = c("<40 &&& F", "2010"),
      variable_name = "number_subjects",
      variable_level = NA_character_,
      estimate_name = "count",
      estimate_type = "integer",
      estimate_value = "100",
      additional_name = c("overall", "time"),
      additional_level = c("overall", "1")
    ),
    settings = dplyr::tibble(
      result_id = c(1L, 2L),
      result_type = c("custom_result_1", "custom_result_2"),
      package_name = "OmopViewer",
      package_version = "1.0.0",
      param = c("TRUE", NA),
      x = c("0", "1")
    )
  )
  resultList <- list(
    custom_result_1 = list(result_id = 1L),
    custom_result_2 = list(result_id = 2L)
  )
  expect_no_error(x <- getValues(res, resultList))

  # present names
  expect_identical(
    sort(names(x)),
    sort(c("custom_result_1_cdm_name", "custom_result_1_cohort_name",
      "custom_result_1_age", "custom_result_1_sex",
      "custom_result_1_variable_name", "custom_result_1_variable_level",
      "custom_result_1_estimate_name", "custom_result_1_param",
      "custom_result_1_x", "custom_result_2_cdm_name",
      "custom_result_2_cohort_name", "custom_result_2_year",
      "custom_result_2_variable_name", "custom_result_2_variable_level",
      "custom_result_2_estimate_name", "custom_result_2_time",
      "custom_result_2_x"))
  )

  # custom_result_1
  expect_identical(x$custom_result_1_cdm_name, "cdm1")
  expect_identical(x$custom_result_1_cohort_name, "cohort_1")
  expect_identical(x$custom_result_1_age, "<40")
  expect_identical(x$custom_result_1_sex, "F")
  expect_identical(x$custom_result_1_variable_name, "number_subjects")
  expect_identical(x$custom_result_1_variable_level, character())
  expect_identical(x$custom_result_1_estimate_name, "count")
  expect_identical(x$custom_result_1_param, "TRUE")
  expect_identical(x$custom_result_1_x, "0")

  # custom_result_2
  expect_identical(x$custom_result_2_cdm_name, "cdm2")
  expect_identical(x$custom_result_2_cohort_name, "cohort_1")
  expect_identical(x$custom_result_2_year, "2010")
  expect_identical(x$custom_result_2_variable_name, "number_subjects")
  expect_identical(x$custom_result_2_variable_level, character())
  expect_identical(x$custom_result_2_estimate_name, "count")
  expect_identical(x$custom_result_2_time, "1")
  expect_identical(x$custom_result_2_x, "1")
})

test_that("test getSelected", {
  # check only settings
  res <- omopgenerics::newSummarisedResult(
    x = dplyr::tibble(
      result_id = c(1L, 2L),
      cdm_name = c("cdm1", "cdm1"),
      group_name = "denominator_cohort_name &&& outcome_cohort_name",
      group_level = "denominator_cohort_1 &&& acetaminophen",
      strata_name = "overall",
      strata_level =  "overall",
      variable_name = "Outcome",
      variable_level = NA_character_,
      estimate_name = "incidence_100000_pys",
      estimate_type = "numeric",
      estimate_value = "100",
      additional_name = "incidence_start_date &&& incidence_end_date &&& analysis_interval",
      additional_level = c("2000-01-01 &&& 2000-12-31 &&& years",
                           "2001-01-01 &&& 2000-12-31 &&& years")
    ),
    settings = dplyr::tibble(
      result_id = c(1L, 2L),
      result_type = c("incidence", "incidence"),
      package_name = "IncidencePrevalence",
      package_version = "1.2.0",
      group = "denominator_cohort_name &&& outcome_cohort_name",
      additional = "incidence_start_date &&& incidence_end_date &&& analysis_interval",
      denominator_age_group = c("0 to 150", "80 to 150"),
      denominator_sex = c("Both", "Female")
  ))
  panelList <- list("incidence" = list(result_type = "incidence"))
  expect_no_error(x <- getSelected(getValues(res, panelList)))

  x_values <- getValues(res, panelList)
  x_selected <- getSelected(getValues(res, panelList))

  expect_true(all(
    x_values$incidence_denominator_age_group == c("0 to 150", "80 to 150")
  ))

  expect_true(all(
    x_selected$incidence_denominator_age_group == "0 to 150"
  ))

  expect_true(all(
    x_values$incidence_denominator_sex == c("Both", "Female")
  ))

  expect_true(all(
    x_selected$incidence_denominator_sex == "Both"
  ))
})

test_that("test prepare result", {
  x <- omopViewerResults
  xl <- list(
    "abc" = list(result_id = c(1, 2)),
    "dsk2" = list(result_type = c("prevalence", "prevalence_attrition")),
    "nxx" = list(result_type = "incidence", denominator_sex = "Female")
  )

  expect_no_error(res <- prepareResult(result = x, resultList = xl))
  expect_identical(
    res,
    list(
      "abc" = x |>
        omopgenerics::filterSettings(result_id %in% c(1, 2)),
      "dsk2" = x |>
        omopgenerics::filterSettings(result_type %in% c("prevalence", "prevalence_attrition")),
      "nxx" = x |>
        omopgenerics::filterSettings(result_type %in% "incidence") |>
        omopgenerics::filterSettings(denominator_sex %in% "Female")
    )
  )
})
