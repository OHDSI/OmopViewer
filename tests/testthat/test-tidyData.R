test_that("test tidyData", {
  result <- dplyr::tibble(
    result_id = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L),
    cdm_name = c(rep("cdm1", 3), rep("cdm2", 2), rep("cdm1", 4)),
    group_name = "cohort_name",
    group_level = c(rep("acetaminophen_inc", 5), rep("acetaminophen_prev", 4)),
    strata_name = c("overall", rep("age_group", 2), rep("sex", 2), rep("age_group &&& sex", 4)),
    strata_level = c("overall", "<40", ">40", "Male", "Female", rep("<40 &&& Female", 4)),
    variable_name = c(rep("number subjects", 5), "outcome1", "outcome2", "outcome3", "outcome1"),
    variable_level = NA_character_,
    estimate_name = c(rep("count", 5), rep("blood_type", 4)),
    estimate_type = c(rep("integer", 5), rep("character", 4)),
    estimate_value = c("100", "40", "60", "20", "80", "A", "AB", "0", "A"),
    additional_name = c(rep("overall", 5), rep("time", 4)),
    additional_level = c(rep("overall", 5), "1", "2", "3", "4")
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = c(1L, 2L, 3L),
      "result_type" = c("custom", "custom", "type2"),
      "package_name" = "",
      "package_version" = "",
      "my_param" = 1L,
      "analysis" = c(TRUE, FALSE, TRUE)
    ))

  expect_no_error(
    x <- result |>
      tidyData(
        prefixSet = "",
        prefixGroup = "",
        showSettings = TRUE,
        showGroupping = TRUE,
        pivotEstimates = FALSE)
  )
  expect_identical(colnames(x), c(
    "my_param", "analysis", "cdm_name", "cohort_name", "age_group", "sex",
    "time", "variable_name", "variable_level", "estimate_name", "estimate_type",
    "estimate_value"
  ))
  expect_true(nrow(x) == nrow(result))
  expect_no_error(
    x <- result |>
      tidyData(
        prefixSet = "pref_set_",
        showSettings = TRUE,
        showGroupping = FALSE,
        pivotEstimates = FALSE)
  )
  expect_identical(colnames(x), c(
    "pref_set_my_param", "pref_set_analysis", "variable_name", "variable_level",
    "estimate_name", "estimate_type", "estimate_value"
  ))
  expect_true(nrow(x) == nrow(result))
  expect_no_error(
    x <- result |>
      tidyData(
        prefixGroup = "pref_group_",
        showSettings = FALSE,
        showGroupping = TRUE,
        pivotEstimates = FALSE)
  )
  expect_identical(colnames(x), c(
    "result_id", "pref_group_cdm_name", "pref_group_cohort_name",
    "pref_group_age_group", "pref_group_sex", "pref_group_time",
    "variable_name", "variable_level", "estimate_name", "estimate_type",
    "estimate_value"
  ))
  expect_true(nrow(x) == nrow(result))
  expect_no_error(
    x <- result |>
      tidyData(
        showSettings = FALSE,
        showGroupping = FALSE,
        pivotEstimates = FALSE)
  )
  expect_identical(colnames(x), c(
    "result_id", "variable_name", "variable_level", "estimate_name",
    "estimate_type", "estimate_value"
  ))
  expect_true(nrow(x) == nrow(result))
  expect_no_error(
    x <- result |>
      tidyData(
        showSettings = TRUE,
        showGroupping = TRUE,
        pivotEstimates = TRUE)
  )
  expect_identical(colnames(x), c(
    "my_param", "analysis", "cdm_name", "cohort_name", "age_group", "sex",
    "time", "variable_name", "variable_level", "count", "blood_type"
  ))
  expect_true(nrow(x) == nrow(result))
  expect_identical(x$count |> dplyr::type_sum(), "int")
  expect_identical(x$blood_type |> dplyr::type_sum(), "chr")
})
