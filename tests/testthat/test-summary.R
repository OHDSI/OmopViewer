test_that("test cardSummary", {
  displayOutput <- function(x) {
    x |> as.character() |> stringr::str_split_1("\n") |> cat(sep = "\n")
  }

  # empty summarised result
  expect_no_error(x <- cardSummary(emptySummarisedResult()))
  expect_true(inherits(x, "bslib_fragment"))
  expect_snapshot(displayOutput(x))

  # not suppressed summarised result
  res <- dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    cdm_name = c("mock", "mock", "eunomia"),
    variable_name = "number subjects",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = "10"
  ) |>
    visOmopResults::uniteGroup() |>
    visOmopResults::uniteStrata() |>
    visOmopResults::uniteAdditional() |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = c("counts", "counts", "sums"),
      package_name = c("omopViewer", "omopViewer", "omopgenerics"),
      package_version = c("0.1.0", "0.2.0", "1.0.0"),
      min_cell_count = c(NA, 1, 5)
    ))
  expect_no_error(x <- cardSummary(res))
  expect_true(inherits(x, "bslib_fragment"))
  expect_snapshot(displayOutput(x))

})
