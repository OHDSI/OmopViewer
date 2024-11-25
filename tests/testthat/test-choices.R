test_that("test choices", {
  # empty result
  res <- omopgenerics::emptySummarisedResult()
  panelList <- res |>
    panelDetailsFromResult() |>
    purrr::map(\(x) x$result_id)
  expect_no_error(x <- defaultFilterValues(res, panelList))
  expected <- list()
  expect_identical(x, expected)

  # check only settings
  resT <- c("custom_result_1", "custom_result_2")
  res <- omopgenerics::emptySummarisedResult(settings = dplyr::tibble(
    result_id = c(1L, 2L),
    result_type = resT,
    package_name = "OmopViewer",
    package_version = "1.0.0",
    param = c("TRUE", NA),
    x = c("0", "1")
  ))
  panelDetails <- res |>
    panelDetailsFromResult() |>
    addFilterNames(result = res)
  panelList <- panelDetails |>
    purrr::map(\(x) x$result_id)
  expect_no_error(x <- defaultFilterValues(res, panelList))
  nm <- names(x)
  expect_true(all(startsWith(nm, "custom_result_1") | startsWith(nm, "custom_result_2")))
  # names
  nm1 <- nm[startsWith(nm, "custom_result_1")]
  nm1 <- substr(nm1, 17, nchar(nm1))
  expect_identical(nm1, c(panelDetails$custom_result_1$filter_name, "tidy_columns"))
  nm2 <- nm[startsWith(nm, "custom_result_2")]
  nm2 <- substr(nm2, 17, nchar(nm2))
  expect_identical(nm2, c(panelDetails$custom_result_2$filter_name, "tidy_columns"))
  # settings
  expect_identical(x$custom_result_1_settings_param, "TRUE")
  expect_identical(x$custom_result_1_settings_x, "0")
  expect_false("custom_result_2_settings_param" %in% names(x))
  expect_identical(x$custom_result_2_settings_x, "1")

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
      result_type = resT,
      package_name = "OmopViewer",
      package_version = "1.0.0",
      param = c("TRUE", NA),
      x = c("0", "1")
    )
  )
  panelDetails <- res |>
    panelDetailsFromResult() |>
    addFilterNames(result = res)
  panelList <- panelDetails |>
    purrr::map(\(x) x$result_id)
  expect_no_error(x <- defaultFilterValues(res, panelList))
  nm <- names(x)
  expect_true(all(startsWith(nm, "custom_result_1") | startsWith(nm, "custom_result_2")))
  # names
  nm1 <- nm[startsWith(nm, "custom_result_1")]
  nm1 <- substr(nm1, 17, nchar(nm1)) |> sort()
  expect_identical(nm1, sort(c(panelDetails$custom_result_1$filter_name, "tidy_columns")))
  nm2 <- nm[startsWith(nm, "custom_result_2")]
  nm2 <- substr(nm2, 17, nchar(nm2)) |> sort()
  expect_identical(nm2, sort(c(panelDetails$custom_result_2$filter_name, "tidy_columns")))
  # settings
  expect_identical(x$custom_result_1_settings_param, "TRUE")
  expect_identical(x$custom_result_1_settings_x, "0")
  expect_false("custom_result_2_settings_param" %in% names(x))
  expect_identical(x$custom_result_2_settings_x, "1")
  # exact names
  expect_identical(
    c("settings_param", "settings_x", "grouping_cdm_name",
      "grouping_cohort_name", "grouping_age", "grouping_sex", "variable_name",
      "estimate_name", "tidy_columns") |>
      sort(),
    sort(nm1)
  )
  expect_identical(
    c("estimate_name", "grouping_cdm_name", "grouping_cohort_name",
      "grouping_time", "grouping_year", "settings_x", "tidy_columns",
      "variable_name") |>
      sort(),
    sort(nm2)
  )
  expect_identical(x$custom_result_1_grouping_cdm_name, "cdm1")
  expect_identical(x$custom_result_1_grouping_cohort_name, "cohort_1")
  expect_identical(x$custom_result_1_grouping_age, "<40")
  expect_identical(x$custom_result_1_grouping_sex, "F")
  expect_identical(x$custom_result_2_grouping_cdm_name, "cdm2")
  expect_identical(x$custom_result_2_grouping_cohort_name, "cohort_1")
  expect_identical(x$custom_result_2_grouping_year, "2010")
  expect_identical(x$custom_result_2_grouping_time, "1")
})
