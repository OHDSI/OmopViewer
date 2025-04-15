
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OmopViewer

<!-- badges: start -->

[![R-CMD-check](https://github.com/OHDSI/OmopViewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OHDSI/OmopViewer/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/OmopViewer)](https://CRAN.R-project.org/package=OmopViewer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/OHDSI/OmopViewer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OHDSI/OmopViewer?branch=main)
<!-- badges: end -->

The goal of OmopViewer is to allow the user to easily create Shiny Apps
to visualise study results in `<summarised_result>` format.

## Installation

Install it from cran:

``` r
install.packages("OmopViewer")
```

Or you can install the development version of OmopViewer from
[GitHub](https://github.com/OHDSI/OmopViewer) with:

``` r
install.packages("pak")
pak::pkg_install("OHDSI/OmopViewer")
```

## Main functionalities

``` r
library(OmopViewer)
```

The package has two functionalities:

- Static app
- Dynamic app

## Static shiny app

The static shiny app functionality creates a static shiny from a list of
`summarised_result` objects. This shiny is specific to the set of
results and can be modified later locally.

``` r
# lets generate some results
library(CohortCharacteristics)
cdm <- mockCohortCharacteristics()
result <- summariseCharacteristics(cdm$cohort1) |>
  bind(summariseCohortAttrition(cdm$cohort1))
#> ℹ adding demographics columns
#> ℹ summarising data
#> ℹ summarising cohort cohort_1
#> ℹ summarising cohort cohort_2
#> ℹ summarising cohort cohort_3
#> ✔ summariseCharacteristics finished!

exportStaticApp(result = result, directory = tempdir())
#> ℹ Processing data
#> ✔ Data processed: 2 panels idenfied: `summarise_cohort_attrition` and
#>   `summarise_characteristics`.
#> ℹ Creating shiny from provided data
#> ✔ Shiny created in:
#>   /var/folders/pl/k11lm9710hlgl02nvzx4z9wr0000gp/T//RtmpBLLZg0/shiny
```

This function allow some customisation of the shiny with the arguments:

- `theme` (to choose a pre-built theme or a bslib one).
- `logo` (you can point to one of the pre-builr logos or to a local
  image).
- `title`
- `background` whether to allow for an .md file for customisation of a
  background panel.
- `summary` whether to include or not a summary panel.
- `panelStructure` allows you to structure the different panels in
  dropdown menus.
- `panelDetails` allows you to create panels at result_id level and
  assign which are the outputs that we want to include in each panel.

The shiny generated will have the following structure:

- `global.R` loads the data.
- `ui.R` with all the ui code. You can edit there the buttons and its
  default values.
- `server.R` server logic, you can edit that file to change some of the
  displays.
- `functions.R` some utility functions that are used in the shiny app.
- `data/result.csv` the original summarised_result provided.
- `data/ShinyData.RData` the .RData file that contains the data used in
  the shiny.
- `data/preprocess.R` the file to generate ShinyData.RData from
  results.csv

## Dynamic shiny app

The dynamic shiny app can be easily launched with `launchDynamicApp()`
function. This function creates a shinyApp where you can upload multiple
results sets and visualise them.

``` r
launchDynamicApp()
```

By default the shiny generated will have no data, you have to upload
data from a csv or zip file that you have it locally. The
summarised_results will be processed and you will be allowed to choose
which results to visualise.

## Example shiny

An example shiny can be found in:
<https://dpa-pde-oxford.shinyapps.io/OmopViewerExample/>. This
`shinyApp` is automatically build from `main` using the latest versions
of `omopViewerResults` dataset and `omopViewerPanels` panels
definitions.
