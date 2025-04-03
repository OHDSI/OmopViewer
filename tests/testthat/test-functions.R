
test_that("test functions.R is correctly copied", {
  skip("manual test")
  fileInst <- file.path(here::here(), "inst", "functions.R")
  fileR <- file.path(here::here(), "R", "functions.R")

  functionsInst <- readLines(fileInst)
  functionsR <- readLines(fileR)

  expect_identical(functionsInst, functionsR)

  # code to copy
  #file.copy(from = here::here("R", "functions.R"), to = here::here("inst", "functions.R"), overwrite = TRUE)
})

