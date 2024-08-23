
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
    body <- '{body},
    ## {rt} ----
    shinydashboard::tabItem(
      tabName = "{rt}",
      {getFilters(setOpts, rt, "Settings")},
      {getFilters(grOpts, rt, "Groupping")},
      {getFilters(vrOpts, rt, "Variables and estimates")},
      {getTabsetPanel(rt, unique(setOpts$name), unique(grOpts$name))}
    )' |>
      glue::glue()
  }

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
      styler::style_text()
  } else {
    x <- x |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
  }
  return(x)
}

getInfo <- function(rt, info, def) {
  x <- omopViewerTabs[[info]][omopViewerTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
}
formatTit <- function(x) {
  x |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    stringr::str_to_sentence()
}
formatCamel <- function(x) {
  x |>
    stringr::str_split(pattern = "_") |>
    unlist() |>
    stringr::str_to_sentence() |>
    stringr::str_flatten()
}
formatSnake <- function(x) {
  omopgenerics::toSnakeCase(x)
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
  TRUE
}
getFormatted <- function(resultType) {
  TRUE
}
getPlot <- function(resultType) {
  TRUE
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
getTabsetPanel <- function(rt, setCols, groupCols) {
  raw <- getRaw(rt)
  if (raw) {
    raw <- rawPanel(rt)
  } else {
    raw <- ""
  }
  formatted <- getFormatted(rt)
  if (formatted) {
    formatted <- formattedPanel(rt, setCols, groupCols)
  } else {
    formatted <- ""
  }
  plot <- getPlot(rt)
  if (plot) {
    plot <- plotPanel(rt, setCols, groupCols)
  } else {
    plot <- ""
  }
  x <- "shiny::tabsetPanel(type = 'tabs'{raw}{formatted}{plot})" |>
    glue::glue()
  return(x)
}
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
formattedPanel <- function(rt, setCols, groupCols) {
  op <- c(groupCols, "variable_name", "variable_level", "estimate_name", setCols)
  if ("cohort_name" %in% op) {
    group <- "'cohort_name'"
  } else {
    group <- "character()"
  }
  if (length(setCols) > 0) {
    set <- cast(setCols)
  } else {
    set <- "character()"
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
         "grViz" = "shiny::imageOutput")
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
