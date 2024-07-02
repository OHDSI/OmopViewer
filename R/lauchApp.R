#' Launch the Shiny Application
#'
#' This function sets up and runs a Shiny application using pre-defined modules.
#' @export
launchApp <- function() {
  # Define UI using your modular functions
  ui <- fluidPage(
    # Use a function from your package that sets up the UI
    app_ui()
  )

  # Server logic also modularized
  server <- function(input, output, session) {
    app_server(input, output, session)
  }

  # Run the app
  shinyApp(ui = ui, server = server)
}
