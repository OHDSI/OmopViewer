
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

> [!IMPORTANT] 
> This package is under construction and this is just a first Beta release, 
> please use it carefully and report any issue that you encounter using it.

The goal of omopViewer is to allow the user to easily create Shiny Apps
to visualise study results in `<summarised_result>` format.

## Installation

You can install the development version of omopViewer from
[GitHub](https://github.com/oxford-pharmacoepi/omopViewer) with:

``` r
install.packages("pak")
pak::pkg_install("oxford-pharmacoepi/omopViewer")
```

## Main functionalities

``` r
library(omopViewer)
```

The package can be divided in 3 main functionalities: - Static shiny
app - Dynamic shiny app - Utility functions

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
#> ! cohort columns will be reordered to match the expected order:
#>   cohort_definition_id, subject_id, cohort_start_date, and cohort_end_date.
#> ! cohort columns will be reordered to match the expected order:
#>   cohort_definition_id, subject_id, cohort_start_date, and cohort_end_date.
result <- summariseCharacteristics(cdm$cohort1) |>
  bind(summariseCohortAttrition(cdm$cohort1))
#> ℹ adding demographics columns
#> ℹ summarising data
#> ✔ summariseCharacteristics finished!

exportStaticApp(result = result)
#> ℹ Processing data
#> ✔ Data processed: 2 result types idenfied: `summarise_characteristics` and
#>   `summarise_cohort_attrition`.
#> ℹ Creating shiny from provided data
#> ✔ Shiny created in: /Users/martics/Documents/GitHub/omopViewer/shiny
```

## Dynamic shiny app

The dynamic shiny app can be easily launched with `launchDynamicApp()`
function. This function creates a shinyApp where you can upload multiple
results sets and visualise them.

``` r
launchDynamicApp()
```

## Utility functions

- `tidyData` is an experimental version of the `tidy.summarised_result`
  method defined in **visOmopResults**.
- `omopViewerTable` is an experimental version of the `visOmopTable` function
  defined in **visOmopResults**.
- `filterData` is a function used internally in the package to subset
  the result. It is not meant to be for user use. It is exported because
  it is used in the *exportStaticApp()* function.
