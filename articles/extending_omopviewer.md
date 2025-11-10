# Extending OmopViewer

``` r
library(OmopViewer)
```

## Introduction

Do yo want **OmopViewer** to support more packages? This is a developer
focused vignette explaining the process to add new default panels to the
package.

## 1 What lives in *panels.R*

`panels.R` builds **one list** – `omopViewerPanels` – that the package
ships as data. Every element of that list is a *panel definition*. If
you’ve read the other vignette - Panels defined in OmopViewer - you’ve
already seen this structure in action. Here’s a quick recap:

| key                 | purpose                                   |
|---------------------|-------------------------------------------|
| `title`             | label shown in the sidebar                |
| `icon`              | any free FontAwesome icon                 |
| `data$result_type`  | string returned by your ETL / analyser    |
| `automatic_filters` | vector of column groups auto-exposed      |
| `filters`           | named list of picker / switch definitions |
| `content`           | tabs such as `tidy`, `table`, `plot`, …   |

``` r
str(omopViewerPanels$summarise_omop_snapshot, max.level = 2)
#> List of 6
#>  $ title            : chr "Snapshot"
#>  $ icon             : chr "clipboard-list"
#>  $ data             :List of 1
#>   ..$ result_type: chr "summarise_omop_snapshot"
#>  $ automatic_filters: chr "variable_name"
#>  $ filters          :List of 1
#>   ..$ cdm_name:List of 7
#>  $ content          :List of 1
#>   ..$ table:List of 5
#>  - attr(*, "class")= chr "omopviewer_panel"
```

## 2 Add a new panel

Edit *panels.R* **above** the “all panels” block. Here I use
SamplePkg::tablefunction and SamplePkg::plotfunction as a placeholder
for the plot and table functions developer should add.

``` r
## my custom summary ----
myCustomPanel <- list(
  # --- metadata -----------------------------------------------------------
  title  = "My Custom",
  icon   = "wand-magic-sparkles",
  data   = list(result_type = "summarise_my_custom"),
  automatic_filters = c("group", "strata", "variable_name", "settings"),
  filters = list(cdm_name = cdmFilter),
  
  # --- UI tabs ------------------------------------------------------------
  content = list(
    tidy = tidyContent,
    
    table = list(
      title       = "Table Sample",
      output_type = "gt",
      reactive    = "<filtered_data> |> SamplePkg::tablefunction(",
      render      = "<reactive_data>",
      filters = rankTableButton(
        none        = c("<strata>", "estimate_name"),
        header      = "variable_name",
        groupColumn = "cdm_name",
        hide        = "variable_level"
      ),
      download = downloadGtTable("table_sample")
    ),
    
    plot = list(
      title       = "Plot Sample",
      output_type = "ui",
      reactive    = "<filtered_data> |> SamplePkg::plotfunction(facet = input$facet)",
      render      = "renderInteractivePlot(<reactive_data>, input$interactive)",
      filters = list(
        interactive = list(
          button_type = "materialSwitch",
          label = "Interactive",
          value = TRUE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
          ),
          selected = 'cdm_name',
          multiple = TRUE
        )
      ),
      download = downloadPlot("plot_sample.png")
    )
  )
)
```

**Key points for developers**

- **Always add both `table` and `plot` nodes** (unless your package
  really has no plot).
- Use the standard helper objects: `tidyContent`, `rankTableButton()`,
  `downloadGtTable()`, `downloadPlot()`.
- Place the new list *above* the `omopViewerPanels <- list(...)` block,
  then register it:

``` r
omopViewerPanels <- list(
  # existing entries …
  summarise_my_custom = myCustomPanel,
  # default
  default = defaultPanel
) |> purrr::map(\(x) newOmopViewerPanel(x))
```

Run
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html);
the static viewer will now include **My Custom**.

------------------------------------------------------------------------

Commit, tag, release. Done.
