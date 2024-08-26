test_that("check filterData functionality", {
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

  input <- list()
  expect_no_error(
    x <- result |>
      filterData(
        resultType = "custom",
        input = input)
  )
  idCustom <- omopgenerics::settings(result) |>
    dplyr::filter(.data$result_type == "custom") |>
    dplyr::pull("result_id")
  expect_true(nrow(x) == sum(result$result_id %in% idCustom))

  # filtering works
  input <- list(custom_groupping_cdm_name = "cdm1")
  expect_no_error(
    x <- filterData(result = result, resultType = "custom", input = input)
  )
  expect_true(nrow(x) == sum(result$cdm_name == "cdm1" & result$result_id %in% idCustom))

  input <- list(
    custom_groupping_cdm_name = "cdm1",
    custom_random = 1L,
    custom_settings_analysis = TRUE)
  expect_no_error(
    x <- filterData(result = result, resultType = "custom", input = input)
  )
  idAnalysis <- omopgenerics::settings(result) |>
    dplyr::filter(.data$analysis == TRUE) |>
    dplyr::pull("result_id")
  expect_true(nrow(x) == sum(
    result$cdm_name == "cdm1" & result$result_id %in% idCustom &
      result$result_id %in% idAnalysis))

  input <- list(
    custom_groupping_cdm_name = "cdm1",
    custom_random = 1L,
    custom_variables_and_estimates_variable_name = "number subjects")
  expect_no_error(
    x <- filterData(result = result, resultType = "custom", input = input)
  )
  expect_true(nrow(x) == sum(
    result$cdm_name == "cdm1" & result$result_id %in% idCustom &
      result$variable_name == "number subjects"))
})
