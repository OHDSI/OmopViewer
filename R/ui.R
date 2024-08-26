
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
#' @param result A summarised_result object.
#' @param asText Whether to output a text object or to eval it.
#'
#' @return The ui of interest.
#' @export
#'
uiStatic <- function(result = omopgenerics::emptySummarisedResult(),
                     asText = FALSE) {
  # initial checks
  result <- omopgenerics::validateResultArguemnt(result)
  omopgenerics::assertLogical(asText, length = 1)

  set <- getPossibleSettings(result)
  groupping <-getPossibleGroupping(result)
  variables <- getPossibleVariables(result)

  # create sidebar
  sidebar <- createSidebar(names(set))

  # create body
  body <- createBody(set, groupping, variables)

  # create ui
  x <- paste0(
    'shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "My study"),
    # sidebar ----
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "About", tabName = "about", icon = shiny::icon("circle-info")),
        shinydashboard::menuItem(
          text = "Background", tabName = "background", icon = shiny::icon("magnifying-glass"))',
        sidebar,
     ')
    ),
    # body ----
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
        ## about ----
        shinydashboard::tabItem(tabName = "about", omopViewer::aboutTab()),
        ## background ----
        shinydashboard::tabItem(
          tabName = "background",
          shiny::h4("Study background"),
          shiny::p("You can use this section to add some background of your study")
        )',
        body,
        '\n## end ----\n',
     ')
    )
  )'
  )

  if (asText) {
    x <- paste0("ui <- ", x) |>
      styleCode()
  } else {
    x <- x |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
  }
  return(x)
}

# styling functions ----
formatTit <- function(x) {
  x |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    stringr::str_to_sentence()
}
formatCamel <- function(x) {
  x |>
    snakecase::to_any_case(case = "upper_camel", numerals = "asis")
}
formatSnake <- function(x) {
  x |>
    snakecase::to_any_case(case = "snake", numerals = "asis")
}
cast <- function(x) {
  if (length(x) == 0) return(character())
  paste0("c('", paste0(x, collapse = "', '"), "')")
}
subs <- function(x, pat, subst) {
  id <- which(x == pat)
  if (length(id) == 1) {
    n <- length(x)
    if (id == 1) {
      x <- c(subst, x[-1])
    } else if (id == n) {
      x <- c(x[-n], subst)
    } else {
      x <- c(x[1:(id-1)], subst, x[(id+1):n])
    }
  }
  return(x)
}
writeVect <- function(x) {
  if (is.character(x)) {
    x <- paste0('c("', paste0(x, collapse = '", "'), '")')
  } else if (is.null(x)) {
    x <- "NULL"
  } else {
    x <- paste0('c(', paste0(x, collapse = ', '), ')')
  }
  return(x)
}
# get possible options for the different tabs split by result_type ----
getPossibleSettings <- function(result) {
  omopgenerics::settings(result) |>
    dplyr::select(!c("result_id", "package_name", "package_version")) |>
    getPossibilities()
}
getPossibleGroupping <- function(result) {
  result |>
    visOmopResults::addSettings(columns = "result_type") |>
    dplyr::select(c(
      "result_type", "cdm_name", "group_name", "group_level", "strata_name",
      "strata_level", "additional_name", "additional_level")) |>
    dplyr::distinct() |>
    getPossibilities(split = TRUE)
}
getPossibleVariables <- function(result) {
  result |>
    visOmopResults::addSettings(columns = "result_type") |>
    dplyr::select(c("result_type", "variable_name", "estimate_name")) |>
    dplyr::distinct() |>
    getPossibilities()
}
getPossibilities <- function(x, split = FALSE) {
  x <- x |>
    dplyr::group_by(.data$result_type) |>
    dplyr::group_split() |>
    as.list()
  names(x) <- purrr::map_chr(x, \(x) unique(x$result_type))
  uniquePos <- function(xx) {
    xx <- unique(xx)
    xx[!is.na(xx)]
  }
  getPos <- function(xx, split = FALSE) {
    xx <- xx |>
      dplyr::select(!"result_type")
    if (split) xx <- visOmopResults::splitAll(xx)
    xx |>
      as.list() |>
      purrr::map(uniquePos) |>
      vctrs::list_drop_empty()
  }
  x <- x |>
    purrr::map(getPos, split = split)
  return(x)
}
# create sidebar ----
createSidebar <- function(resultType) {
  sidebar <- ""
  for (rt in resultType) {
    tit <- getTitle(rt)
    iconId <- getIcon(rt)
    sidebar <- '{sidebar},
      shinydashboard::menuItem(
        text = "{tit}",
        tabName = "{rt}",
        icon = shiny::icon("{iconId}")
      )' |>
      glue::glue()
  }
  return(sidebar)
}
getInfo <- function(rt, info, def) {
  x <- omopViewerTabs[[info]][omopViewerTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
}
getTitle <- function(resultType) {
  getInfo(resultType, "title", formatTit(resultType))
}
getIcon <- function(resultType) {
  getInfo(resultType, "icon", "table")
}
# create body ----
createBody <- function(set, groupping, variables) {
  body <- ""
  for (rt in names(set)) {
    body <- '{body},
    ## {rt} ----
    shinydashboard::tabItem(
      tabName = "{rt}",
      {getFilters(rt, set[[rt]], "Settings")},
      {getFilters(rt, groupping[[rt]], "Groupping")},
      {getFilters(rt, variables[[rt]], "Variables and estimates")},
      {getTabsetPanel(rt, names(set[[rt]]), names(groupping[[rt]]))}
    )' |>
      glue::glue()
  }
  return(body)
}
getTabsetPanel <- function(rt, setNms, grouppingNms) {
  raw <- rawPanel(rt)
  formatted <- formattedPanel(rt, setNms, grouppingNms)
  plot <- plotPanel(rt, setNms, grouppingNms)
  x <- "shiny::tabsetPanel(type = 'tabs'{raw}{formatted}{plot})" |>
    glue::glue()
  return(x)
}
## to create the filters ----
getFilters <- function(rt, opts, tit) {
  if (length(opts) == 0) return("shiny::p()")
  res <- "shiny::h4('{tit}') " |>
    glue::glue()
  for (nm in names(opts)) {
    op <- opts[[nm]]
    res <- "{res},
    shinyWidgets::pickerInput(
      inputId = '{rt}_{formatSnake(tit)}_{nm}',
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
## to create the raw panel ----
rawPanel <- function(rt) {
  ",
    shiny::tabPanel(
      title = 'Raw table',
      shiny::checkboxInput(
        inputId = '{rt}_show_groupping',
        label = 'Show groupping',
        value = TRUE
      ),
      shiny::checkboxInput(
        inputId = '{rt}_show_settings',
        label = 'Show settings',
        value = FALSE
      ),
      shiny::checkboxInput(
        inputId = '{rt}_pivot_estimates',
        label = 'Pivot estimates',
        value = FALSE
      ),
      shiny::downloadButton(outputId = '{rt}_raw_download', label = 'Download as csv'),
      DT::DTOutput(outputId = '{rt}_raw_table') |>
        shinycssloaders::withSpinner()
    )" |>
    glue::glue()
}
## to create the table panel ----
formattedPanel <- function(rt, setCols, groupCols) {
  op <- c(groupCols, "variable_name", "variable_level", "estimate_name", setCols)
  if ("cohort_name" %in% op) {
    group <- "'cohort_name'"
  } else {
    group <- "NULL"
  }
  if (length(setCols) > 0) {
    set <- cast(setCols)
  } else {
    set <- "NULL"
  }
  ",
    shiny::tabPanel(
      title = 'Formatted table',
      shinyWidgets::pickerInput(
        inputId = '{rt}_header',
        label = 'Header',
        choices = {cast(op)},
        selected = 'cdm_name',
        width = '160px',
        multiple = TRUE,
        inline = TRUE
      ),
      shinyWidgets::pickerInput(
        inputId = '{rt}_group',
        label = 'Group',
        choices = {cast(op)},
        selected = {group},
        width = '160px',
        multiple = TRUE,
        inline = TRUE
      ),
      shinyWidgets::pickerInput(
        inputId = '{rt}_hide',
        label = 'Hide',
        choices = {cast(op)},
        selected = {set},
        width = '160px',
        multiple = TRUE,
        inline = TRUE
      ),
      shiny::downloadButton(outputId = '{rt}_formatted_download', label = 'Download as word'),
      gt::gt_output(outputId = '{rt}_formatted_table') |>
        shinycssloaders::withSpinner()
    )" |>
    glue::glue()
}
## to create the plot panel(s) ----
plotPanel <- function(rt, setCols, groupCols) {
  varCols <- c("variable_name", "variable_level", "estimate_name")
  plots <- getPlots(rt)
  panel <- ""
  for (id in plots) {
    tit <- getPlotTitle(id)
    buttons <- getPlotButtons(rt, id, setCols, groupCols, varCols)
    out <- getPlotOutput(id)
    panel <- "{panel},
      shiny::tabPanel(
        title = '{tit}',
        {buttons}
        shiny::downloadButton(outputId = '{rt}_plot_{id}_download', label = 'Download'),
        {out}(outputId = '{rt}_plot_{id}') |>
          shinycssloaders::withSpinner()
      )" |>
      glue::glue()
  }
  return(panel)
}
getRtId <- function(rt) {
  omopViewerTabs |>
    dplyr::filter(.data$result_type == .env$rt) |>
    dplyr::pull("result_tab_id")
}
getPlots <- function(rt) {
  id <- getRtId(rt)
  if (length(id) == 0) return(integer())
  omopViewerPlots |>
    dplyr::filter(.data$result_tab_id == .env$id) |>
    dplyr::pull("plot_id")
}
getPlotTitle <- function(id) {
  omopViewerPlots$title[omopViewerPlots$plot_id == id]
}
getPlotOutput <- function(id) {
  output <- omopViewerPlots$output[omopViewerPlots$plot_id == id]
  switch(output,
         "ggplot2" = "shiny::plotOutput",
         "grViz" = "DiagrammeR::grVizOutput")
}
getPlotButtons <- function(rt, plotId, setCols, groupCols, varCols) {
  buts <- omopViewerPlotArguments |>
    dplyr::filter(.data$plot_id == .env$plotId)
  but <- ""
  args <- omopViewerPlots |>
    dplyr::filter(.data$plot_id == .env$plotId) |>
    dplyr::select("result_tab_id", "fun") |>
    dplyr::inner_join(
      omopViewerTabs |>
        dplyr::select("result_tab_id", "package"),
      by = "result_tab_id"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(x = paste0(.data$package, "::", .data$fun)) |>
    dplyr::pull("x") |>
    rlang::parse_expr() |>
    eval() |>
    formals()
  for (k in seq_len(nrow(buts))) {
    arg <- buts$argument[k]
    type <- buts$type[k]
    opts <- getButtonOpts(buts$opts[k], setCols, groupCols, varCols) |>
      writeVect()
    def <- writeVect(args[[arg]])
    multiple <- buts$multiple[k]
    but <- "{but}
      {getButton(type)}," |>
      glue::glue() |>
      glue::glue()
  }
  return(but)
}
getButtonOpts <- function(opts, setCols, groupCols, varCols) {
  stringr::str_split_1(opts, pattern = ", ") |>
    subs("<groupping>", groupCols) |>
    subs("<settings>", setCols) |>
    subs("<variable>", varCols)
}
getButton <- function(type) {
  switch(type,
         "selector" = "shinyWidgets::pickerInput(
           inputId = '{rt}_plot_{plotId}_{formatSnake(arg)}',
           label = '{arg}',
           choices = {opts},
           selected = {def},
           multiple = {multiple},
           inline = TRUE
         )",
         "check" = "shiny::checkboxInput(
           inputId = '{rt}_plot_{plotId}_{formatSnake(arg)}',
           label = '{arg}',
           value = {def}
         )")
}
