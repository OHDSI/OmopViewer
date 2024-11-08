test_that("test choices", {
  # empty result
  res <- emptySummarisedResult() |>
    correctSettings()
  panelDetails <- res |>
    panelDetailsFromResult() |>
    addFilterNames(result = res)
  expect_no_error(x <- getFilterValues(panelDetails, res))
  expected <- list()
  names(expected) <- character()
  expect_identical(x, expected)

  # check only settings
  resT <- c("custom_result_1", "custom_result_2")
  res <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = c(1L, 2L),
    result_type = resT,
    package_name = "OmopViewer",
    package_version = "1.0.0",
    param = c(TRUE, NA),
    x = c(0, 1)
  )) |>
    correctSettings()
  panelDetails <- res |>
    panelDetailsFromResult() |>
    addFilterNames(result = res)
  expect_no_error(x <- getFilterValues(panelDetailsFromResult(res), res))
  expect_true(all(sort(resT) == sort(names(x))))
  # names
  expect_identical(names(x$custom_result_1), panelDetails$custom_result_1$filters)
  expect_identical(names(x$custom_result_2), panelDetails$custom_result_2$filters)
  # settings
  expect_identical(x$custom_result_1$settings_param, "TRUE")
  expect_identical(x$custom_result_1$settings_x, "0")
  expect_false("settings_param" %in% names(x$custom_result_2))
  expect_identical(x$custom_result_2$settings_x, "1")

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
      param = c(TRUE, NA),
      x = c(0, 1)
    )
  ) |>
    correctSettings()
  panelDetails <- res |>
    panelDetailsFromResult() |>
    addFilterNames(result = res)
  expect_no_error(x <- getFilterValues(panelDetailsFromResult(res), res))
  expect_true(all(sort(resT) == sort(names(x))))
  # names
  expect_identical(names(x$custom_result_1), panelDetails$custom_result_1$filters)
  expect_identical(names(x$custom_result_2), panelDetails$custom_result_2$filters)
  # settings
  expect_identical(x$custom_result_1$settings_param, "TRUE")
  expect_identical(x$custom_result_1$settings_x, "0")
  expect_false("settings_param" %in% names(x$custom_result_2))
  expect_identical(x$custom_result_2$settings_x, "1")
  # grouping
  expect_identical(
    sort(paste0("grouping_", c("cdm_name", "cohort_name", "age", "sex"))),
    names(x$custom_result_1)[startsWith(names(x$custom_result_1), "grouping_")] |>
      sort()
  )
  expect_identical(
    sort(paste0("grouping_", c("cdm_name", "cohort_name", "year", "time"))),
    names(x$custom_result_2)[startsWith(names(x$custom_result_2), "grouping_")] |>
      sort()
  )
  expect_identical(x$custom_result_1$grouping_cdm_name, "cdm1")
  expect_identical(x$custom_result_1$grouping_cohort_name, "cohort_1")
  expect_identical(x$custom_result_1$grouping_age, "<40")
  expect_identical(x$custom_result_1$grouping_sex, "F")
  expect_identical(x$custom_result_2$grouping_cdm_name, "cdm2")
  expect_identical(x$custom_result_2$grouping_cohort_name, "cohort_1")
  expect_identical(x$custom_result_2$grouping_year, "2010")
  expect_identical(x$custom_result_2$grouping_time, "1")
})
