test_that("background", {
  tdir <- tempdir()

  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = TRUE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = FALSE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  expect_snapshot(createBackground(TRUE) |> cat(sep = "\n"))

  expect_snapshot(createBackground(FALSE) |> cat(sep = "\n"))

  # existing md file
  backgroundFile <- tempfile(fileext = ".md")
  content <- "# test\n\ncustom background"
  writeLines(content, con = backgroundFile)
  expect_no_error(exportStaticApp(
    result = omopgenerics::emptySummarisedResult(),
    directory = tdir,
    background = backgroundFile
  ))
  background <- readLines(file.path(tdir, "shiny", "background.md"))
  expect_identical(content, paste0(background, collapse = "\n"))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

})

test_that("test cardFromMd", {
  tfile <- tempfile(fileext = ".md")

  def <- defaultBackground()
  writeLines(def, con = tfile)

  expect_no_error(bkg <- cardFromMd(tfile))
  expect_true(inherits(bkg, "bslib_fragment"))
  expect_snapshot(bkg |> as.character() |> cat())

  expect_warning(nobkg <- cardFromMd("not file"))
  expect_identical(nobkg, bslib::card())

  unlink(tfile)
})
