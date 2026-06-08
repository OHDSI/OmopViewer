test_that("report generation templates are valid", {
  expect_true(grepl("format-links: false", reportTemplate, fixed = TRUE))

  script <- renderReportScript()
  expect_no_error(parse(text = paste(script, collapse = "\n")))
  expect_true(any(grepl("here::here()", script, fixed = TRUE)))
  expect_true(any(grepl("report_metadata.rds", script, fixed = TRUE)))
  expect_true(any(grepl("rootArtifacts", script, fixed = TRUE)))
  expect_true(any(grepl("unlink(renderedFile", script, fixed = TRUE)))

  server <- createReportServer(TRUE)
  expect_true(grepl(
    'file.path("www", "reports", paste0("report.", extension))',
    server,
    fixed = TRUE
  ))
  expect_true(grepl(
    'paste0("reports/report.", extension)',
    server,
    fixed = TRUE
  ))
})

test_that("static report app files are generated", {
  tdir <- tempdir()
  suppressWarnings(expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    report = TRUE,
    open = FALSE
  )))

  app <- file.path(tdir, "shiny")
  expect_true(file.exists(file.path(app, "report.qmd")))
  expect_true(file.exists(file.path(app, "renderReport.R")))
  expect_true(file.exists(file.path(app, "server.R")))

  report <- readLines(file.path(app, "report.qmd"))
  script <- readLines(file.path(app, "renderReport.R"))
  server <- readLines(file.path(app, "server.R"))

  expect_true(any(report == "format-links: false"))
  expect_no_error(parse(text = paste(script, collapse = "\n")))
  expect_true(any(grepl("here::here()", script, fixed = TRUE)))
  expect_true(any(grepl("rootArtifacts", script, fixed = TRUE)))
  expect_true(any(grepl('file.path("www", "reports", paste0("report.", extension))', server, fixed = TRUE)))
  expect_true(any(grepl('paste0("reports/report.", extension)', server, fixed = TRUE)))

  unlink(app, recursive = TRUE)
})
