# Package index

### Static app

This function allows to generate local apps that can be later
customised.

- [`exportStaticApp()`](https://ohdsi.github.io/OmopViewer/reference/exportStaticApp.md)
  : Export and launch a static shiny specific to the provided results.

### Dynamic app

This function allows to generate dynamic apps. This app will visualise
any summarised_result that you upload on it.

- [`launchDynamicApp()`](https://ohdsi.github.io/OmopViewer/reference/launchDynamicApp.md)
  **\[experimental\]** : Launch a dynamic shiny app where you can upload
  results.

### Defined panels

Functionalities to explore the default defined panels

- [`defaultPanels()`](https://ohdsi.github.io/OmopViewer/reference/defaultPanels.md)
  : Default panels defined in the package.
- [`getPanel()`](https://ohdsi.github.io/OmopViewer/reference/getPanel.md)
  : Get one of the default pre-built panels.
- [`omopViewerPanels`](https://ohdsi.github.io/OmopViewer/reference/omopViewerPanels.md)
  : Panels defined in the package.

### Utility functions

- [`panelDetailsFromResult()`](https://ohdsi.github.io/OmopViewer/reference/panelDetailsFromResult.md)
  :

  Obtain default panel details from a `<summarised_result>` object.

- [`omopViewerResults`](https://ohdsi.github.io/OmopViewer/reference/omopViewerResults.md)
  :

  Mock results obtained from `GiBleed` dataset.
