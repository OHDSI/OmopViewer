summaryTab <- function(summary) {
  if (!summary) return(character())
  'bslib::nav_panel(
    title = "Summary",
    icon = shiny::icon("file-alt"),
    bslib::card(
      bslib::card_header("Explore results"),
      shiny::tags$div(
        class = "mb-2",
        shinyTree::shinyTree("summary_cdm_name", theme = "proton", themeIcons = FALSE),
        shinyTree::shinyTree("summary_packages", theme = "proton", themeIcons = FALSE),
        shinyTree::shinyTree("summary_min_cell_count", theme = "proton", themeIcons = FALSE),
        shinyTree::shinyTree("summary_panels", theme = "proton", themeIcons = FALSE)
      ),
      full_screen = TRUE
    )
  )'
}

