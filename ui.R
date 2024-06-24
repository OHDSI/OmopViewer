CONSTS <- use("constants/constants.R")
dashboardPage(
  dashboardHeader(
    # App title visible in browser tab
    title = CONSTS$APP_TITLE,
    # App title visible
    tags$li(class = "dropdown title", tags$h1(CONSTS$APP_TITLE)),
    # App current version
    tags$li(class = "dropdown version", tags$p(CONSTS$APP_VERSION)),
    # App time range
    tags$li(class = "dropdown time-range", tags$p(CONSTS$APP_TIME_RANGE)),
    # App logo
    tags$li(class = "dropdown logo", CONSTS$hds_logo)
  ),
  dashboardSidebar(
    uiOutput("dynamic_sidebar")  # Changed from uiOutput
  ),
  dashboardBody(
      tags$head(
        # Reset favicon
        tags$link(rel = "shortcut icon", href = "#"),
        # Compiled css file
        tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css")
      ),
    uiOutput("dynamic_tabs_output")
  )
)
