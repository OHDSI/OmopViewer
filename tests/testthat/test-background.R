test_that("background", {
  #tdir <- here::here()
  tdir <- tempdir()
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(),
    directory = tdir,
    background = TRUE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)
  expect_no_error(exportStaticApp(
    result = emptySummarisedResult(),
    directory = tdir,
    background = FALSE
  ))
  unlink(file.path(tdir, "shiny"), recursive = TRUE)

  expect_snapshot(createBackground(TRUE) |> cat(sep = "\n"))

  expect_snapshot(createBackground(FALSE) |> cat(sep = "\n"))
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
