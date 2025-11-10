# Get one of the default pre-built panels.

Get one of the default pre-built panels.

## Usage

``` r
getPanel(panelId)
```

## Arguments

- panelId:

  Name of the one of the default panels. Use
  [`defaultPanels()`](https://ohdsi.github.io/OmopViewer/reference/defaultPanels.md)
  to see the available default panels.

## Value

A panel definition.

## Examples

``` r
getPanel("incidence")
#> Incidence (OmopViewer panel)
#> •  icon: chart-line
#> •  data: result_type: <incidence>
#> •  filters: 1 filters + 6 automatic filters
#> •  content: Table Incidence (gt); Plot Incidence (ui); Plot population (ui)
```
