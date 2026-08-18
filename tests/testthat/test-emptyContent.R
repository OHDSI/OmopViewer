test_that("empty content placeholders construct without error", {
  expect_true(inherits(emptyTableGt(), "gt_tbl"))
  expect_true(inherits(emptyTableReactable(), "reactable"))
  expect_true(inherits(emptyTableDT(), "datatables"))
  expect_true(inherits(emptyPlot(), "ggplot"))
  expect_true(inherits(emptyPlotly(), "plotly"))
  expect_true(inherits(emptyDiagram(), "grViz"))
})

test_that("writeEmptyContentGuard only emits a guard when updateButtons is TRUE", {
  expect_identical(writeEmptyContentGuard("nm", FALSE, "gt"), "")

  guardGt <- writeEmptyContentGuard("summarise_cohort_count", TRUE, "gt")
  expect_true(grepl("input$update_summarise_cohort_count", guardGt, fixed = TRUE))
  expect_true(grepl("emptyTableGt()", guardGt, fixed = TRUE))

  guardUi <- writeEmptyContentGuard("incidence", TRUE, "ui")
  expect_true(grepl("renderInteractivePlot(emptyPlot(), FALSE)", guardUi, fixed = TRUE))

  guardReactable <- writeEmptyContentGuard("cohort_code_use", TRUE, "reactable")
  expect_true(grepl("emptyTableReactable()", guardReactable, fixed = TRUE))

  guardGrViz <- writeEmptyContentGuard("summarise_cohort_attrition", TRUE, "grViz")
  expect_true(grepl("emptyDiagram()", guardGrViz, fixed = TRUE))

  guardPlotly <- writeEmptyContentGuard("summarise_large_scale_characteristics", TRUE, "plotly")
  expect_true(grepl("emptyPlotly()", guardPlotly, fixed = TRUE))
})

test_that("generated static app renders placeholders until update is clicked", {
  skip_on_cran()

  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = omopViewerResults, directory = tdir, open = FALSE, summary = FALSE
  ))

  ui <- readLines(file.path(tdir, "shiny", "ui.R"))
  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_no_error(parse(file.path(tdir, "shiny", "ui.R")))
  expect_no_error(parse(file.path(tdir, "shiny", "server.R")))

  # every panel's outputs are gated behind their own update button
  expect_true(any(grepl(
    "input$update_summarise_cohort_count) || input$update_summarise_cohort_count == 0",
    server,
    fixed = TRUE
  )))
  expect_true(any(grepl("emptyTableGt()", server, fixed = TRUE)))
  expect_true(any(grepl("emptyTableReactable()", server, fixed = TRUE)))
  expect_true(any(grepl("emptyDiagram()", server, fixed = TRUE)))
  expect_true(any(grepl("emptyPlotly()", server, fixed = TRUE)))
  expect_true(any(grepl(
    "renderInteractivePlot(emptyPlot(), FALSE)", server, fixed = TRUE
  )))

  # the survival panel (hand-written generator) is gated the same way
  expect_true(any(grepl(
    "input$update_survival) || input$update_survival == 0", server, fixed = TRUE
  )))

  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})

test_that("without update buttons no placeholder guard is generated", {
  skip_on_cran()

  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = omopViewerResults, directory = tdir, open = FALSE, summary = FALSE,
    updateButtons = FALSE
  ))

  server <- readLines(file.path(tdir, "shiny", "server.R"))
  expect_no_error(parse(file.path(tdir, "shiny", "server.R")))
  expect_false(any(grepl("is.null(input$update_", server, fixed = TRUE)))

  unlink(file.path(tdir, "shiny"), recursive = TRUE)
})
