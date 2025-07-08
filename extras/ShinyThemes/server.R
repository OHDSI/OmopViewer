
server <- function(input, output, session) {
  # Update theme dynamically based on user selection
  observeEvent(input$select_theme, {
    session$setCurrentTheme(themes[[input$select_theme]])
  })
}
