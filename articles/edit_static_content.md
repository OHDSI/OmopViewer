# Customise your static shiny

## Introduction

In this vignette we will see how to edit the panelDetails object to have
one more level of customisation.

### Load packages

``` r

library(OmopViewer)
library(dplyr, warn.conflicts = FALSE)
library(omopgenerics, warn.conflicts = FALSE)
```

### Mock data

We will use the incidence results obtained by the package, we can subset
our reuslts using the following command:

``` r

result <- omopViewerResults |> 
  filterSettings(result_type == "incidence")
```

As we can see it contains data from different settings:

``` r

result |>
  settings() |>
  glimpse()
#> Rows: 18
#> Columns: 20
#> $ result_id                            <int> 17, 18, 19, 20, 21, 22, 23, 24, 2…
#> $ result_type                          <chr> "incidence", "incidence", "incide…
#> $ package_name                         <chr> "IncidencePrevalence", "Incidence…
#> $ package_version                      <chr> "1.2.1", "1.2.1", "1.2.1", "1.2.1…
#> $ group                                <chr> "denominator_cohort_name &&& outc…
#> $ strata                               <chr> "", "", "", "", "", "", "", "", "…
#> $ additional                           <chr> "incidence_start_date &&& inciden…
#> $ min_cell_count                       <chr> "5", "5", "5", "5", "5", "5", "5"…
#> $ analysis_censor_cohort_name          <chr> "None", "None", "None", "None", "…
#> $ analysis_complete_database_intervals <chr> "TRUE", "TRUE", "TRUE", "TRUE", "…
#> $ analysis_outcome_washout             <chr> "365", "365", "365", "365", "365"…
#> $ analysis_repeated_events             <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ denominator_age_group                <chr> "0 to 150", "0 to 150", "0 to 150…
#> $ denominator_days_prior_observation   <chr> "365", "365", "365", "365", "365"…
#> $ denominator_end_date                 <chr> "2009-12-31", "2009-12-31", "2009…
#> $ denominator_requirements_at_entry    <chr> "FALSE", "FALSE", "FALSE", "FALSE…
#> $ denominator_sex                      <chr> "Both", "Female", "Male", "Both",…
#> $ denominator_start_date               <chr> "2000-01-01", "2000-01-01", "2000…
#> $ denominator_target_cohort_name       <chr> "None", "None", "None", "None", "…
#> $ denominator_time_at_risk             <chr> "0 to Inf", "0 to Inf", "0 to Inf…
```

In this case we will create two incidence tabs one for the `Male` data
and another one for the `Female` data, the starting point will be the
default incidence tab, we can ibtain it using:

``` r

getPanel("incidence")
#> Incidence (OmopViewer panel)
#> •  icon: chart-line
#> •  data: result_type: <incidence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
```

Or just producing it from the results:

``` r

panelDetailsFromResult(result = result)
#> $incidence
#> Incidence (OmopViewer panel)
#> •  icon: chart-line
#> •  data: result_type: <incidence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
```

We can then define our `panelDetails` object using:

``` r

panelDetails <- list(
  incidence_female = getPanel("incidence"),
  incidence_male = getPanel("incidence")
)
```

For the moment this the only thing that will create is two panels of
incidence, but with the same information, the first thing that we will
do is to restrict to the corresponding data in each one of the panels:

``` r

panelDetails$incidence_female$data <- list(denominator_sex = "Female")
panelDetails$incidence_male$data <- list(denominator_sex = "Male")
```

We can now for example edit the title:

``` r

panelDetails$incidence_female$title <- "Incidence in Female"
panelDetails$incidence_male$title <- "Incidence in Male"
```

And the icons:

``` r

panelDetails$incidence_female$icon <- "venus"
panelDetails$incidence_male$icon <- "mars"
```

Let’s create the shiny and see how it looks like:

``` r

exportStaticApp(result = result, panelDetails = panelDetails, directory = tempdir())
```

We can for example remove the *tidy* panel for the female tab and the
plot panel for the male tab like this:

``` r

panelDetails$incidence_female$content$tidy <- NULL
panelDetails$incidence_male$content$plot <- NULL
```

Finally we will remove the filter for `denominator_sex` for both panels:

``` r

panelDetails$incidence_female$exclude_filters <- c(panelDetails$incidence_female$exclude_filters, "denominator_sex")
panelDetails$incidence_male$exclude_filters <- c(panelDetails$incidence_male$exclude_filters, "denominator_sex")
```

We can now regenerate the shiny and see how our changes were effective:

``` r

exportStaticApp(result = result, panelDetails = panelDetails, directory = tempdir())
```
