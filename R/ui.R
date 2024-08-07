
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
#' @param data Summarised_result to build the shiny.
#' @param asText Whether to output a text object or to eval it.
#'
#' @return The ui of interest.
#' @export
#'
uiStatic <- function(data = omopgenerics::emptySummarisedResult(),
                     asText = FALSE) {
  # initial checks
  data <- omopgenerics::validateResultArguemnt(data)
  omopgenerics::assertLogical(asText, length = 1)

  set <- omopgenerics::settings(data)
  resultType <- unique(set$result_type)
  groupping <- data |>
    visOmopResults::splitAll(fill = NA_character_) |>
    dplyr::select(!c("variable_name", "variable_level", "estimate_name",
                     "estimate_type", "estimate_value")) |>
    dplyr::distinct()
  variables <- data |>
    dplyr::select("result_id", "variable_name", "estimate_name") |>
    dplyr::distinct()

  # create sidebar
  sidebar <- ""
  for (rt in resultType) {
    tit <- getTitle(rt)
    iconId <- getIcon(rt)
    sidebar <- '{sidebar},
      shinydashboard::menuItem(
        text = "{tit}", tabName = "{rt}", icon = shiny::icon("{iconId}"))' |>
      glue::glue()
  }

  # create body
  body <- ""
  for (rt in resultType) {
    setOpts <- set |>
      dplyr::filter(.data$result_type == .env$rt)
    resId <- setOpts$result_id |> unique()
    setOpts <- setOpts |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(!"result_id") |>
      dplyr::filter(!is.na(.data$value), !.data$name %in% c(
        "package_name", "package_version", "result_type")) |>
      dplyr::select(!"result_id") |>
      dplyr::distinct()
    grOpts <- groupping |>
      dplyr::filter(.data$result_id %in% .env$resId) |>
      dplyr::select(!"result_id") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::distinct()
    vrOpts <- variables |>
      dplyr::filter(.data$result_id %in% .env$resId) |>
      dplyr::select(!"result_id") |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::distinct()
    body <- '{body}, shinydashboard::tabItem(
      tabName = "{rt}",
      {getFilters(setOpts, rt, "Settings")},
      {getFilters(grOpts, rt, "Groupping")},
      {getFilters(vrOpts, rt, "Variables and estimates")},
      {getTabsetPanel(rt)}
    )' |>
      glue::glue()
  }

  # create ui
  x <- paste0(
    'shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "My study"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "About", tabName = "about", icon = shiny::icon("circle-info")),
        shinydashboard::menuItem(
          text = "Background", tabName = "background", icon = shiny::icon("magnifying-glass"))',
        sidebar,
     ')
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
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "about", omopViewer::aboutTab()),
        shinydashboard::tabItem(
          tabName = "background",
          shiny::h4("Study background"),
          shiny::p("You can use this section to add some background of your study")
        )',
        body,
     ')
    )
  )'
  )

  if (asText) {
    x <- paste0("ui <- ", x) |>
      styler::style_text()
  } else {
    x <- x |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
  }
  return(x)
}

getInfo <- function(rt, info, def) {
  x <- resultTypeTabs[[info]][resultTypeTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
}
formatTit <- function(x) {
  x |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    stringr::str_to_sentence()
}
cast <- function(x) {
  if (length(x) == 0) return(character())
  paste0("c('", paste0(x, collapse = "', '"), "')")
}
getTitle <- function(resultType) {
  getInfo(resultType, "title", formatTit(resultType))
}
getIcon <- function(resultType) {
  getInfo(resultType, "icon", "table")
}
getRaw <- function(resultType) {
  getInfo(resultType, "raw", TRUE)
}
getFormatted <- function(resultType) {
  getInfo(resultType, "formatted", TRUE)
}
getPlot <- function(resultType) {
  getInfo(resultType, "plot", TRUE)
}
getFilters <- function(opts, rt, tit) {
  if (nrow(opts) == 0) return("shiny::p()")
  res <- "shiny::h4('{tit}') " |>
    glue::glue()
  nms <- opts$name |> unique()
  for (nm in nms) {
    op <- opts$value[opts$name == nm]
    res <- "{res}, shinyWidgets::pickerInput(
      inputId = '{rt}_{omopgenerics::toSnakeCase(tit)}_{nm}',
      label = '{formatTit(nm)}',
      choices = {cast(op)},
      selected = {cast(op)},
      width = '160px',
      multiple = TRUE,
      inline = TRUE)" |>
      glue::glue()
  }
  return(res)
}
getTabsetPanel <- function(rt) {
  raw <- getRaw(rt)
  if (raw) {
    raw <- ",
    shiny::tabPanel(
      title = 'Raw table',
      shiny::checkboxInput(
        inputId = '{rt}_pivot_estimates',
        label = 'Pivot estimates',
        value = FALSE
      ),
      shiny::checkboxInput(
        inputId = '{rt}_show_settings',
        label = 'Show settings',
        value = FALSE
      ),
      shiny::downloadButton(outputId = '{rt}_raw_download', label = 'Download as csv'),
      DT::DTOutput(outputId = '{rt}_raw_table') |>
        shinycssloaders::withSpinner()
    )" |>
      glue::glue()
  } else {
    raw <- ""
  }
  formatted <- getFormatted(rt)
  formatted <- ""
  plot <- getPlot(rt)
  plot <- ""
  x <- "shiny::tabsetPanel(type = 'tabs'{raw}{formatted}{plot})" |>
    glue::glue()
}
