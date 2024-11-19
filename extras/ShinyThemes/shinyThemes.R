library(shiny)
library(bslib)

themes <- OmopViewer:::omopViewerThemes |>
  purrr::map(\(x) rlang::eval_tidy(rlang::parse_expr(x)))

ui <- page_sidebar(
  theme = themes[[1]],
  title = "OmopViewer themes",
  sidebar = bslib::sidebar(
    shinyWidgets::pickerInput(
      inputId = "select_theme",
      label = "Select theme:",
      selected = names(themes)[1],
      choices = names(themes)
    ),
    position = "left",
    open = "always"
  ),
  shiny::p("This is a bslib theme")
)

server <- function(input, output, session) {
  # Update theme dynamically based on user selection
  observeEvent(input$select_theme, {
    session$setCurrentTheme(themes[[input$select_theme]])
  })
}

shinyApp(ui, server)
