test_that("correctSettings columns", {

  # empty summarised_result
  expect_identical(
    emptySummarisedResult(settings = dplyr::tibble(
      result_id = integer(),
      result_type = character(),
      package_name = character(),
      package_version = character(),
      group = character(),
      strata = character(),
      additional = character()
    )),
    emptySummarisedResult() |>
      correctSettings()
  )

  # simple example
  x <- dplyr::tibble(
    result_id = 1L,
    cdm_name = "mock data",
    group_name = "cohort_name_ref &&& cohort_name_comp",
    group_level = "cohort1 &&& cohort2",
    strata_name = c("age &&& sex", "year"),
    strata_level = c("<40 &&& Female", "2010"),
    variable_name = "number subjects",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = "10",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = 1L,
      result_type = "toy_data",
      package_name = "omopViewer",
      package_version = "0.1.0"
    ))
  expect_no_error(xCorrected <- correctSettings(x))
  expect_identical(colnames(x), colnames(xCorrected))
  expect_true(
    x |>
      dplyr::anti_join(xCorrected, by = colnames(x)) |>
      nrow() == 0
  )
  expect_identical(
    omopgenerics::settings(xCorrected),
    omopgenerics::settings(x) |>
      dplyr::mutate(
        group = "cohort_name_ref &&& cohort_name_comp",
        strata = "age &&& sex &&& year",
        additional = NA_character_
      )
  )

  # multiple result_ids with different columns
  x <- dplyr::tibble(
    result_id = c(1L, 2L),
    cdm_name = "mock data",
    group_name = "cohort_name_ref &&& cohort_name_comp",
    group_level = "cohort1 &&& cohort2",
    strata_name = c("age &&& sex", "year"),
    strata_level = c("<40 &&& Female", "2010"),
    variable_name = "number subjects",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = "10",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L),
      result_type = "toy_data",
      package_name = "omopViewer",
      package_version = "0.1.0"
    ))
  expect_no_error(xCorrected <- correctSettings(x))
  expect_identical(colnames(x), colnames(xCorrected))
  expect_true(
    x |>
      dplyr::anti_join(xCorrected, by = colnames(x)) |>
      nrow() == 0
  )
  expect_identical(
    omopgenerics::settings(xCorrected),
    omopgenerics::settings(x) |>
      dplyr::mutate(
        group = "cohort_name_ref &&& cohort_name_comp",
        strata = c("age &&& sex", "year"),
        additional = NA_character_
      )
  )

  # one of the result_ids is empty
  x <- x |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = "toy_data",
      package_name = "omopViewer",
      package_version = "0.1.0"
    ))
  expect_no_error(xCorrected <- correctSettings(x))
  expect_identical(colnames(x), colnames(xCorrected))
  expect_true(
    x |>
      dplyr::anti_join(xCorrected, by = colnames(x)) |>
      nrow() == 0
  )
  expect_identical(
    omopgenerics::settings(xCorrected),
    omopgenerics::settings(x) |>
      dplyr::mutate(
        group = c(
          "cohort_name_ref &&& cohort_name_comp",
          "cohort_name_ref &&& cohort_name_comp",
          NA_character_
        ),
        strata = c("age &&& sex", "year", NA_character_),
        additional = NA_character_
      )
  )

  # expect warning
  x <- x |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = "toy_data",
      package_name = "omopViewer",
      package_version = "0.1.0",
      strata = "test"
    ))
  expect_warning(xCorrected <- correctSettings(x))
  expect_identical(colnames(x), colnames(xCorrected))
  expect_true(
    x |>
      dplyr::anti_join(xCorrected, by = colnames(x)) |>
      nrow() == 0
  )
  expect_identical(
    omopgenerics::settings(xCorrected),
    omopgenerics::settings(x) |>
      dplyr::mutate(
        group = c(
          "cohort_name_ref &&& cohort_name_comp",
          "cohort_name_ref &&& cohort_name_comp",
          NA_character_
        ),
        strata = c("age &&& sex", "year", NA_character_),
        additional = NA_character_
      )
  )
})
