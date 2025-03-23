
# NOTES
# load the package before trying to use styleCode()
# project ----
omopViewerProj <- c(
  "Version: 1.0",
  "",
  "RestoreWorkspace: Default",
  "SaveWorkspace: Default",
  "AlwaysSaveHistory: Default",
  "",
  "EnableCodeIndexing: Yes",
  "UseSpacesForTab: Yes",
  "NumSpacesForTab: 2",
  "Encoding: UTF-8",
  "",
  "RnwWeave: Sweave",
  "LaTeX: pdfLaTeX"
)

# preprocess ----
omopViewerPreprocess <- c(
  "",
  "source(file.path(getwd(), \"functions.R\"))",
  "",
  "result <- omopgenerics::importSummarisedResult(file.path(getwd(), \"data\"))",
  "data <- prepareResult(result, resultList)",
  "filterValues <- defaultFilterValues(result, resultList)",
  "",
  "save(data, filterValues, file = file.path(getwd(), \"data\", \"shinyData.RData\"))",
  "",
  "rm(result, filterValues, resultList, data)"
)

# global -----
omopViewerGlobal <- c(
  "# preprocess data if it has not been done",
  "fileData <- file.path(getwd(), \"data\", \"shinyData.RData\")",
  "if (!file.exists(fileData)) {",
  "source(file.path(getwd(), \"data\", \"preprocess.R\"))",
  "}",
  "",
  "# uncomment to load the raw data",
  "# rawData <- omopgenerics::importSummarisedResult(file.path(getwd(), \"data\"))",
  "",
  "# load shiny data",
  "load(fileData)",
  "",
  "# source functions",
  "source(file.path(getwd(), \"functions.R\"))"
) |>
  styleCode()

# logos ----
# TO ADD NEW LOGOS YOU HAVE TO ADD THEM IN THIS LIST AND IN `inst/logos/`
# FOLLOW THIS NAMING: '{keyword}_logo.svg'
# NOTE IT IS NOT CASE SENSITIVE
logoKeywords <- c("hds", "ohdsi") |>
  stringr::str_to_lower()

# background keywords ----
# IT HAS TO BE EDITED HERE AND IN `functions.R`!!
backgroundKeywords <- dplyr::tribble(
  ~keyword, ~fun, ~link,
  "header", "bslib::card_header", "https://rstudio.github.io/bslib//reference/card_body.html",
  "footer", "bslib::card_footer", "https://rstudio.github.io/bslib//reference/card_body.html"
)

# themes ----
omopViewerThemes <- list(
  default = "bslib::bs_theme()",
  theme1 = "bslib::bs_theme(
    bootswatch = 'sandstone',
    primary = '#605ca8',
    bg = 'white',
    fg = 'black',
    success = '#3B9AB2',
  )"
)

# add internal data ----
usethis::use_data(
  omopViewerProj, omopViewerGlobal, omopViewerPreprocess, logoKeywords,
  backgroundKeywords, omopViewerThemes,
  overwrite = TRUE, internal = TRUE
)
