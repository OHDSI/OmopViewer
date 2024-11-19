test_that("test summaryCard", {
  displayOutput <- function(x) {
    x |>
      as.character() |>
      stringr::str_split_1("\n") |>
      purrr::keep(\(x) !grepl("htmlwidget-", x)) |>
      cat(sep = "\n")
  }

  # empty summarised result
  res <- omopgenerics::emptySummarisedResult() |>
    prepareResult(list())
  expect_no_error(x <- summaryCard(res))
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
      package_name = c("OmopViewer", "OmopViewer", "omopgenerics"),
      package_version = c("0.1.0", "0.2.0", "1.0.0"),
      min_cell_count = c(NA, 1, 5)
    )) |>
    prepareResult(list(counts = c(1, 2), sums = 3))
  expect_no_error(x <- summaryCard(res))
  expect_true(inherits(x, "bslib_fragment"))
  expect_snapshot(displayOutput(x))
})
