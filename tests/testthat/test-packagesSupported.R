test_that("IncidencePrevalence shiny", {
  skip_on_cran()
  directory <- tempdir()
  exportStaticApp(result = omopViewerResults, directory = directory)
  expect_true("shiny" %in% list.files(directory))
  unlink(file.path(directory, "shiny"), recursive = TRUE)
})
