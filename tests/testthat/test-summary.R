test_that("test summaryCdmName", {
  result <- omopgenerics::newSummarisedResult(dplyr::tibble(
    result_id = 1L,
    cdm_name = c("cdm1", "cdm2"),
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = "date",
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ))
  x <- list("panel1" = result)
  expect_identical(
    summaryCdmName(x),
    list("<b>CDM names</b>" = list(
      "cdm1 (1)" = "cdm1 (1)", "cdm2 (1)" = "cdm2 (1)"
    ))
  )
})

test_that("test summaryPackages", {
  result <- dplyr::tibble(
    result_id = 1L,
    cdm_name = "cdm1",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = "date",
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    result_type = "summarised_result",
    package_name = c("Package1", "Package2", "Package3"),
    package_version = c("0.4.3", "0.2.1", "1.3.1")
  ))
  expect_identical(
    summaryPackages(list(summarised_result = result)),
    list("<b>Packages versions</b>" = list(
      "Package1 (version = 0.4.3; number records = 1)" = "Package1",
      "Package2 (version = 0.2.1; number records = 0)" = "Package2",
      "Package3 (version = 1.3.1; number records = 0)" = "Package3"
    ))
  )

  # multiple versions
  result <- dplyr::tibble(
    result_id = 1L,
    cdm_name = "cdm1",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = "date",
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = "summarised_result",
      package_name = c("Package1", "Package1", "Package3"),
      package_version = c("0.4.3", "0.2.1", "1.3.1")
    ))
  expect_identical(
    summaryPackages(list(summarised_result = result)),
    list("<b style='color:red'>Packages versions</b>" = list(
      "<b style='color:red'>Package1 (Multiple versions!) </b>" = list(
        "version = 0.2.1; number records = 0" = "version = 0.2.1; number records = 0",
        "version = 0.4.3; number records = 1" = "version = 0.4.3; number records = 1"
      ),
      "Package3 (version = 1.3.1; number records = 0)" = "Package3"
    ))
  )
})

test_that("test summaryMinCellCount", {
  result <- dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    cdm_name = "cdm1",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = "date",
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = "summarised_result",
      package_name = c("Package1", "Package2", "Package3"),
      package_version = c("0.4.3", "0.2.1", "1.3.1"),
      min_cell_count = c("4", "10", "4")
    ))
  expect_identical(
    summaryMinCellCount(list(summarised_result = result)),
    list("<b>Min Cell Count Suppression</b>" = list(
      "Min cell count = 4 (2)" = "Min cell count = 4 (2)",
      "Min cell count = 10 (1)" = "Min cell count = 10 (1)"
    ))
  )

  # multiple versions
  result <- dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    cdm_name = "cdm1",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = "date",
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L, 3L),
      result_type = "summarised_result",
      package_name = c("Package1", "Package1", "Package3"),
      package_version = c("0.4.3", "0.2.1", "1.3.1"),
      min_cell_count = c("4", "0", "4")
    ))
  expect_identical(
    summaryMinCellCount(list(summarised_result = result)),
    list("<b style='color:red'>Min Cell Count Suppression</b>" = list(
      "<b style='color:red'>Not censored</b> (1)" = "<b style='color:red'>Not censored</b> (1)",
      "Min cell count = 4 (2)" = "Min cell count = 4 (2)"
    ))
  )
})

test_that("test summaryPanels", {
  result <- dplyr::tibble(
    result_id = c(1L, 2L, 2L),
    cdm_name = "cdm1",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "test",
    variable_level = NA_character_,
    estimate_name = c("date1", "date1", "date2"),
    estimate_type = "character",
    estimate_value = "today",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = c(1L, 2L),
      result_type = "summarised_result",
      parameter = c("value1", "value2")
    ))
  expect_identical(
    summaryPanels(list(summarised_result = result)),
    list("<b>Panels</b>" = list("summarised_result" = list(
      "result_type" = list(
        "summarised_result (number rows = 3)" = "summarised_result (number rows = 3)"
      ),
      "parameter" = list(
        "value1 (number rows = 1)" = "value1 (number rows = 1)",
        "value2 (number rows = 2)" = "value2 (number rows = 2)"
      ),
      "cdm_name" = list(
        "cdm1 (number rows = 3)" = "cdm1 (number rows = 3)"
      )
    )))
  )
})
