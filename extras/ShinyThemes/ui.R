
ui <- bslib::page_navbar(
  theme = themes[[1]],
  title = "OmopViewer themes",
  bslib::nav_panel(
    title = "My panel",
    bslib::layout_sidebar(
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
  )
)
