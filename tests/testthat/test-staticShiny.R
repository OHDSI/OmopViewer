test_that("multiplication works", {
  cdm <- CohortCharacteristics::mockCohortCharacteristics()
  result <- cdm$cohort1 |>
    CohortCharacteristics::summariseCharacteristics() |>
    omopgenerics::bind(
      cdm$cohort1 |>
        CohortCharacteristics::summariseCohortAttrition()
    )
  set <- omopgenerics::settings(result) |>
    dplyr::select(-"cohort_name")
  result <- result |>
    omopgenerics::newSummarisedResult(settings = set)

  expect_no_error(exportStaticApp(data = result))
})
