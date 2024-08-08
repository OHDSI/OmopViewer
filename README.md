
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omopViewer

<!-- badges: start -->

[![R-CMD-check](https://github.com/oxford-pharmacoepi/omopViewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oxford-pharmacoepi/omopViewer/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/omopViewer)](https://CRAN.R-project.org/package=omopViewer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/oxford-pharmacoepi/omopViewer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/oxford-pharmacoepi/omopViewer?branch=main)
<!-- badges: end -->

The goal of omopViewer is to allow the user to easily create interactive
Shiny Apps.

## Installation

You can install the development version of omopViewer from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/omopViewer")
```

## Main functionalities

``` r
library(omopViewer)
```

The package can be divided in 3 main functionalities: - Dynamic shiny
app - Static shiny app - Utility functions

## Dynamic shiny app

The dynamic shiny app can be easily launched with `launchDynamicApp()`
function. This function creates a shinyApp where you can upload multiple
results sets and visualise them.

``` r
launchDynamicApp()
```

## Static shiny app

The static shiny app functionality creates a static shiny from a list of
`summarised_result` objects. This shiny is specific to the set of
results and can be modified later locally.

``` r
# lets generate some results
library(CohortCharacteristics)
cdm <- mockCohortCharacteristics()
#> Note: method with signature 'DBIConnection#Id' chosen for function 'dbExistsTable',
#>  target signature 'duckdb_connection#Id'.
#>  "duckdb_connection#ANY" would also be valid
result <- cdm$cohort1 |>
  summariseCharacteristics() |>
  omopgenerics::bind( # should be reexported by CohortCharacteristics
    cdm$cohort1 |>
      summariseCohortAttrition()
  )
#> ℹ adding demographics columns
#> ℹ summarising data
#> ✔ summariseCharacteristics finished!

exportStaticApp(data = result)
#> ℹ Processing data
#> ✔ Data processed: 2 result types idenfied: `summarised_characteristics` and
#>   `cohort_attrition`.
#> ℹ Creating shiny from provided data
#> ✔ Shiny created in: /Users/martics/Documents/GitHub/omopViewer
#> ℹ Launching shiny
```

## Utility functions

These functions are used inside the static shiny app.
