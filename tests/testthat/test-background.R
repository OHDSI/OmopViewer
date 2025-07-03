test_that("background", {
  tdir <- tempdir()

  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = TRUE,
    open = FALSE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = FALSE,
    open = FALSE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  expect_identical(createBackground(NULL), character())

  expect_identical(createBackground(""), 'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("book-atlas"),
    backgroundCard("background.md")
  )')

  # existing md file
  backgroundFile <- tempfile(fileext = ".md")
  content <- "# test\n\ncustom background"
  writeLines(content, con = backgroundFile)
  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = backgroundFile,
    open = FALSE
  ))
  background <- readLines(file.path(tdir, "shiny", "background.md"))
  expect_identical(content, paste0(background, collapse = "\n"))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

})

test_that("test backgroundCard", {
  tfile <- tempfile(fileext = ".md")

  def <- defaultBackground(logo = NULL)
  writeLines(def, con = tfile)

  expect_no_error(bkg <- backgroundCard(tfile))
  expect_true(inherits(bkg, "bslib_fragment"))

  unlink(tfile)
})
