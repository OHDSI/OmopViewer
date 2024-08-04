
#' Provides the dynamic UI of the shiny app.
#'
#' @return The ui of interest.
#' @export
#'
uiDynamic <- function() {
  CONSTS <- modules::use("extras/constants/constants.R")
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      # App title visible in browser tab
      title = CONSTS$APP_TITLE,
      # App title visible
      shiny::tags$li(class = "dropdown title", shiny::tags$h1(CONSTS$APP_TITLE)),
      # App current version
      shiny::tags$li(class = "dropdown version", shiny::tags$p(CONSTS$APP_VERSION)),
      # App time range
      shiny::tags$li(class = "dropdown time-range", shiny::tags$p(CONSTS$APP_TIME_RANGE)),
      # App logo
      shiny::tags$li(class = "dropdown logo", CONSTS$hds_logo)
    ),
    shinydashboard::dashboardSidebar(
      shiny::uiOutput("dynamic_sidebar") # Changed from uiOutput
    ),
    shinydashboard::dashboardBody(
      shiny::tags$head(
        # Reset favicon
        shiny::tags$link(rel = "shortcut icon", href = "#"),
        # Compiled css file
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = system.file("www/css/sass.min.css", package = "omopViewer"))
      ),
      shiny::uiOutput("dynamic_tabs_output")
    )
  )
}


#' Provides the static UI of the shiny app for a given set of resultType(s).
#'
#' @param resultType Character vector indicating the result_type of interest.
#' @param asText Whether to output a text object or to eval it.
#'
#' @return The ui of interest.
#' @export
#'
uiStatic <- function(resultType = character(),
                     asText = FALSE) {
  # initial checks
  omopgenerics::assertCharacter(resultType, unique = TRUE)
  omopgenerics::assertLogical(asText, length = 1)

  sidebar <- character()
  body <- character()
  for (rt in resultType) {
    tit <- getTitle(rt)
    iconId <- getIcon(rt)
    sidebar <- '{sidebar}, shinydashboard::menuItem(text = "{tit}", tabName = "{rt}", icon = shiny::icon("{iconId}"))' |>
      glue::glue()
  }

  # create ui
  x <- paste0(
    'shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "omopViewer"),
    shinydashboard::dashboardSidebar(
      shinydashboard::menuItem(
        text = "About", tabName = "about", icon = shiny::icon("circle-info"))',
    sidebar,
    '),
    shinydashboard::dashboardBody(
      shiny::tags$head(
        # Reset favicon
        shiny::tags$link(rel = "shortcut icon", href = "#"),
        # Compiled css file
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = system.file("www/css/sass.min.css", package = "omopViewer"))
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "about", omopViewer::aboutTab())',
    body,
    ')
    )
  )'
  )

  if (asText) {
    x <- x |>
      styler::style_text()
  } else {
    x <- x |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
  }
  return(x)
}

getTitle <- function(resultType) {
  x <- resultTypeTabs$title[resultTypeTabs$result_type == resultType]
  if (length(x) == 1 && !is.na(x)) return(x)
  resultType |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    stringr::str_to_sentence()
}
getIcon <- function(resultType) {
  x <- resultTypeTabs$icon[resultTypeTabs$result_type == resultType]
  if (length(x) == 1 && !is.na(x)) return(x)
  "table"
}
