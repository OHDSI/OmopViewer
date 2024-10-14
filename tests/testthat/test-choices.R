test_that("test getChoices", {
  # empty result
  res <- emptySummarisedResult()
  expect_no_error(x <- getChoices(res))
  expected <- list()
  names(expected) <- character()
  expect_identical(x, expected)

  # check only settings
  resT <- c("custom_result_1", "custom_result_2")
  res <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = c(1L, 2L),
    result_type = resT,
    package_name = "omopViewer",
    package_version = "1.0.0",
    param = c(TRUE, NA),
    x = c(0, 1)
  ))
  expect_no_error(x <- getChoices(res))
  expect_true(all(sort(resT) == sort(names(x))))
  # names
  expect_identical(names(x$custom_result_1), c("settings", "grouping", "variable_name", "estimate_name", "tidy_columns"))
  expect_identical(names(x$custom_result_2), c("settings", "grouping", "variable_name", "estimate_name", "tidy_columns"))
  # tidy columns
  expect_identical(x$custom_result_1$tidy_columns, c("cdm_name", "param", "x"))
  expect_identical(x$custom_result_2$tidy_columns, c("cdm_name", "x"))
  # settings
  expect_identical(x$custom_result_1$settings$param, "TRUE")
  expect_identical(x$custom_result_1$settings$x, "0")
  expect_false("param" %in% names(x$custom_result_2$settings))
  expect_identical(x$custom_result_2$settings$x, "1")

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
      package_name = "omopViewer",
      package_version = "1.0.0",
      param = c(TRUE, NA),
      x = c(0, 1)
    )
  )
  expect_no_error(x <- getChoices(res))
  expect_true(all(sort(resT) == sort(names(x))))
  # names
  expect_identical(names(x$custom_result_1), c("settings", "grouping", "variable_name", "estimate_name", "tidy_columns"))
  expect_identical(names(x$custom_result_2), c("settings", "grouping", "variable_name", "estimate_name", "tidy_columns"))
  # tidy columns
  expect_identical(x$custom_result_1$tidy_columns, c("cdm_name", "cohort_name", "age", "sex", "param", "x"))
  expect_identical(x$custom_result_2$tidy_columns, c("cdm_name", "cohort_name", "year", "time", "x"))
  # settings
  expect_identical(x$custom_result_1$settings$param, "TRUE")
  expect_identical(x$custom_result_1$settings$x, "0")
  expect_false("param" %in% names(x$custom_result_2$settings))
  expect_identical(x$custom_result_2$settings$x, "1")
  # grouping
  expect_identical(names(x$custom_result_1$grouping), c("cdm_name", "cohort_name", "age", "sex"))
  expect_identical(names(x$custom_result_2$grouping), c("cdm_name", "cohort_name", "year", "time"))
  expect_identical(x$custom_result_1$grouping$cdm_name, "cdm1")
  expect_identical(x$custom_result_1$grouping$cohort_name, "cohort_1")
  expect_identical(x$custom_result_1$grouping$age, "<40")
  expect_identical(x$custom_result_1$grouping$sex, "F")
  expect_identical(x$custom_result_2$grouping$cdm_name, "cdm2")
  expect_identical(x$custom_result_2$grouping$cohort_name, "cohort_1")
  expect_identical(x$custom_result_2$grouping$year, "2010")
  expect_identical(x$custom_result_2$grouping$time, "1")
})
