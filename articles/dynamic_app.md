# Dynamic app

## Introduction

This vignette shows how to visualise results dynamically in *Shiny apps*
using the `OmopViewer` package. These dynamic shiny apps allow very
little customisation and are not deployable or editable, if you seek a
more flexible approach please see the [Static
app](https://ohdsi.github.io/OmopViewer/articles/export_static_app.Rmd)
vignette.

## Dynamic app

``` r
library(OmopViewer)
```

To launch the dynamic app please use the following command:

``` r
launchDynamicApp()
```

Use the **Upload data** to upload a csv result exported using
[`exportSummarisedResult()`](https://darwin-eu.github.io/omopgenerics/reference/exportSummarisedResult.html)
and then click the button **Bind data and load shiny** to create panels
for the uploaded data. One panel will be created per summarised result.
You can easily navigate thought the different tabs to see whats on your
data.
