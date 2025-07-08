library(shiny)
library(bslib)

themes <- OmopViewer:::omopViewerThemes |>
  purrr::map(\(x) rlang::eval_tidy(rlang::parse_expr(x)))
