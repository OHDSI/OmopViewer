
test_that("output of CohortCharacteristics is a summarised result", {
  cdm <- CohortCharacteristics::mockCohortCharacteristics()

  result <- cdm$cohort1 |> CohortCharacteristics::summariseCharacteristics()

  expect_true(inherits(result, "summarised_result"))
  expect_true("result_type" %in% colnames(omopgenerics::settings(result)))
  expect_equal(
    unique(omopgenerics::settings(result)$result_type),
    "summarised_characteristics"
  )
})
