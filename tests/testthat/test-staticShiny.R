test_that("empty shiny", {
  tdir <- here::here()
  expect_no_error(exportStaticApp(directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(asText = TRUE) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(asText = TRUE) |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
})

test_that("CohortCharacteristics shiny", {
  cdm <- CohortCharacteristics::mockCohortCharacteristics(seed = 1L)
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

  tdir <- here::here()
  expect_no_error(exportStaticApp(result = result, directory = tdir))
  expect_true("shiny" %in% list.files(tdir))
  expect_snapshot(uiStatic(result = result, asText = TRUE) |> cat(sep = "\n"))
  expect_snapshot(serverStatic(result = result, asText = TRUE) |> cat(sep = "\n"))
  unlink(paste0(tdir, "/shiny/"), recursive = TRUE)
})
