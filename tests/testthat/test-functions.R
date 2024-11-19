
test_that("test functions.R is correctly copied", {
  skip("manual test")

  fileInst <- system.file("functions.R", package = "OmopViewer")
  fileR <- file.path(find.package("OmopViewer"), "R", "functions.R")

  functionsInst <- readLines(fileInst)
  functionsR <- readLines(fileR)

  expect_identical(functionsInst, functionsR)

  expect_identical(functionsInst, styleCode(functionsR))

  # code to copy
  # file.copy(from = here::here("R", "functions.R"), to = here::here("inst", "functions.R"), overwrite = TRUE)
})

