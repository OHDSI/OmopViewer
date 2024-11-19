test_that("check filterData functionality", {
  resultList <- list(custom = c(1, 2), type2 = 3)
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
    )) |>
    prepareResult(resultList)

  input <- list()
  expect_no_error(
    x <- result |>
      filterData(prefix = "custom", input = input)
  )
  expect_identical(x, result$custom)

  # filtering works
  input <- list(custom_grouping_cdm_name = "cdm1")
  expect_no_error(
    x <- filterData(result = result, prefix = "custom", input = input)
  )
  expect_identical(x, result$custom |> dplyr::filter(.data$cdm_name == "cdm1"))

  input <- list(
    custom_grouping_cdm_name = "cdm1",
    custom_random = 1L,
    custom_settings_analysis = TRUE)
  expect_no_error(
    x <- filterData(result = result, prefix = "custom", input = input)
  )
  idAnalysis <- result$custom |>
    omopgenerics::settings() |>
    dplyr::filter(.data$analysis == TRUE) |>
    dplyr::pull("result_id")
  expect_identical(
    x,
    result$custom |>
      dplyr::filter(
        .data$cdm_name == "cdm1", .data$result_id %in% .env$idAnalysis
      ) |>
      omopgenerics::newSummarisedResult(
        settings = omopgenerics::settings(result$custom) |>
          dplyr::filter(.data$result_id %in% .env$idAnalysis)
      )
  )

  input <- list(
    custom_grouping_cdm_name = "cdm1",
    custom_random = 1L,
    custom_variable_name = "number subjects")
  expect_no_error(
    x <- filterData(result = result, prefix = "custom", input = input)
  )
  expect_identical(
    x,
    result$custom |>
      dplyr::filter(
        .data$cdm_name == "cdm1", .data$variable_name == "number subjects"
      )
  )
})
