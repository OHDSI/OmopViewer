# Export and launch a static shiny specific to the provided results.

Export and launch a static shiny specific to the provided results.

## Usage

``` r
exportStaticApp(
  result,
  directory,
  logo = NULL,
  title = "",
  background = TRUE,
  summary = TRUE,
  report = FALSE,
  panelDetails = panelDetailsFromResult(result),
  panelStructure = NULL,
  theme = NULL,
  template = NULL,
  updateButtons = TRUE,
  includeOneChoiceFilters = TRUE,
  open = rlang::is_interactive()
)
```

## Arguments

- result:

  A summarised_result object.

- directory:

  Directory to create the shiny.

- logo:

  Name of a logo or path to a logo. If NULL no logo is obtained from
  theme.

- title:

  title of the shiny

- background:

  Whether to include a background panel. Background panel content will
  be controlled from the generated background.md file.

- summary:

  Whether to include a panel with a summary of content in the `result`.

- report:

  Whether to include a quarto report.

- panelDetails:

  A named list to provide details for each one of the panels, such as:
  result_id, result_type, title, icon, filters and content. By default
  it is created using the
  [`panelDetailsFromResult()`](https://ohdsi.github.io/OmopViewer/reference/panelDetailsFromResult.md)
  function.

- panelStructure:

  A named list of panel identifiers to organise them in drop-down menus.
  Identifiers names are the ones used in `panelDetails`. By default one
  panel per each `panelDetails` element is created.

- theme:

  Specify the theme for the Shiny application. You can either select a
  predefined theme provided by the package (e.g., `"theme1"`), or define
  a custom theme using
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html).
  If using a custom theme, it must be provided as a character string
  (e.g., `"bslib::bs_theme(bg = 'white', fg = 'black')"`).

- template:

  Path to a template `.docx` document to be used for the report.

- updateButtons:

  Whether to include update buttons for visualisations.

- includeOneChoiceFilters:

  Whether to include filter buttons for filters with just one choice.

- open:

  Whether to open the shiny app project.

## Value

The shiny app will be created in directory.

## Examples

``` r
exportStaticApp(
  result = omopgenerics::emptySummarisedResult(),
  directory = tempdir()
)
#> ℹ Processing data
#> ! No panels identified, generated shiny will be empty.
#> ℹ Creating `shiny` from provided data
#> ✔ Shiny created in: /tmp/RtmprYoD8V/shiny
```
