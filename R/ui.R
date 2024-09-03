
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

uiStatic <- function(result = emptySummarisedResult(),
                     logo = NULL,
                     title = "My study",
                     background = TRUE) {
  # initial checks
  result <- omopgenerics::validateResultArguemnt(result)
  omopgenerics::assertCharacter(logo, length = 1, null = TRUE)
  omopgenerics::assertCharacter(title, length = 1)
  omopgenerics::assertLogical(background, length = 1)

  elements <- c(
    createTitle(title, logo),
    createBackground(background, title, logo),
    createPanels(result),
    'bslib::nav_spacer()',
    createAbout(),
    'bslib::nav_item(bslib::input_dark_mode(id ="dark_mode", mode = "light"))'
  ) |>
    paste0(collapse = ",\n")

  'ui <- bslib::page_navbar(
  {elements}
  )' |>
    glue::glue() |>
    as.character() |>
    styleCode()
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
  if (is.character(x)) {
    x <- paste0('c("', paste0(x, collapse = '", "'), '")')
  } else if (is.null(x)) {
    x <- "NULL"
  } else if (is.call(x)) {
    x <- deparse(x)
  } else {
    x <- paste0('c(', paste0(x, collapse = ', '), ')')
  }
  return(x)
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
# get choices ----
getChoices <- function(result) {
  settings <- getPossibleSettings(result)
  groupping <-getPossibleGroupping(result)
  variables <- getPossibleVariables(result)
  choices <- names(variables) |>
    purrr::set_names() |>
    purrr::map(\(x) list(
      settings = settings[[x]],
      groupping = groupping[[x]],
      variable_name = variables[[x]]$variable_name,
      estimate_name = variables[[x]]$estimate_name
    ))
  return(choices)
}
getPossibleSettings <- function(result) {
  omopgenerics::settings(result) |>
    dplyr::select(!dplyr::any_of(c(
      "result_id", "package_name", "package_version"))) |>
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
# create title ----
createTitle <- function(title, logo) {
  if (is.null(logo)) {
    x <- 'title = "{title}"'
  } else {
    x <- 'title = shiny::tags$span(
      shiny::tags$img(
        src = "{logo}",
        width = "auto",
        height = "46px",
        class = "me-3",
        alt = "logo"
      ),
      "{title}"
    )'
  }
  x <- glue::glue(x) |> as.character()
  return(x)
}
# create background ----
createBackground <- function(background, title, logo) {
  if (!background) return("")
  if (!is.null(logo)) {
    logoImg <- ',
    shiny::tags$img(
      src = "{logo}",
      width = "auto",
      height = "100px",
      alt = "logo",
      align = "left"
    )' |>
      glue::glue() |>
      as.character()
  } else {
    logoImg <- ""
  }
  'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
    bslib::card(
      bslib::card_header("{title} background"),
      shiny::p("You can use this section to add some background of your study"){logoImg}
    )
  )' |>
    glue::glue() |>
    as.character()
}
# create about ----
createAbout <- function(about = TRUE) {
  if (!about) return("")
  'bslib::nav_item(
    bslib::popover(
      shiny::icon("circle-info"),
      shiny::tags$img(
        src = "hds_logo.svg",
        class = "logo-img",
        alt = "Logo",
        height = "auto",
        width = "30%",
        style = "float:right"
      ),
      "This shiny app was generated with ",
      shiny::a(
        "omopViewer",
        href = "https://github.com/oxford-pharmacoepi/omopViewer",
        target = "_blank"
      ),
      shiny::strong("v{as.character(utils::packageVersion("omopViewer"))}")
    )
  )' |>
    glue::glue() |>
    as.character()
}
# create panels ----
createPanels <- function(result) {
  choices <- getChoices(result)
  tabs <- names(choices)
  panel <- character()
  for (tab in tabs) {
    title <- getPanelTitle(tab)
    icon <- getPanelIcon(tab)
    sidebar <- getPanelSidebar(tab, choices[[tab]])
    content <- c(title, icon, sidebar) |>
      paste0(collapse = ",\n")
    panel <- c(
      panel,
      'bslib::nav_panel(
        {content}
      )' |>
        glue::glue() |>
        as.character()
    )
  }
  panel <- paste0(panel, collapse = ",\n")
  return(panel)
}
getInfo <- function(rt, info, def) {
  x <- omopViewerTabs[[info]][omopViewerTabs$result_type == rt]
  if (length(x) == 1 && !is.na(x)) return(x)
  def
}
getPanelTitle <- function(tab) {
  paste0('title = "', getInfo(tab, "title", formatTit(tab)), '"')
}
getPanelIcon <- function(tab) {
  icon <- getInfo(tab, "icon", NA_character_)
  if (is.na(icon)) return(NULL)
  paste0('icon = shiny::icon("', icon, '")')
}
getPanelSidebar <- function(tab, choic) {
  content <- c(
    getSidebarInformation(tab),
    getSidebarChoices(choic$settings, "Settings", paste0(tab, "_settings")),
    getSidebarChoices(choic$groupping, "Groupping", paste0(tab, "_groupping")),
    getSidebarChoices(choic["variable_name"], "Variables", tab),
    getSidebarChoices(choic["estimate_name"], "Estimates", tab)
  ) |>
    paste0(collapse = ",\n")

  panels <- c(
    getRawPanel(tab),
    getTidyPanel(tab),
    getFormattedPanel(tab, choic),
    getPlotsPanel(tab, choic)
  ) |>
    paste0(collapse = ",\n")

  "bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      bslib::accordion(
        {content}
      )
    ),
    bslib::navset_card_tab(
      {panels}
    )
  )" |>
    glue::glue() |>
    as.character()
}
getSidebarInformation <- function(tab) {
  info <- getInfo(tab, "information", "")
  'bslib::accordion_panel(
    title = "Information",
    icon = shiny::icon("info"),
    shiny::p("{info}")
  )' |>
    glue::glue() |>
    as.character()
}
getSidebarChoices <- function(choi, tit, prefix) {
  if (length(choi) == 0) return(NULL)
  selectors <- purrr::map_chr(names(choi), \(x) selector(
    paste0(prefix, "_", x), formatTit(x), cast(choi[[x]]), cast(choi[[x]]), TRUE
  )) |>
    paste0(collapse = ",\n")
  'bslib::accordion_panel(
    title = "{tit}",
    {selectors}
  )' |>
    glue::glue() |>
    as.character()
}
selector <- function(id, lab, cho, sel, mult) {
  'shiny::selectizeInput(
    inputId = "{id}",
    label = "{lab}",
    choices = {cho},
    selected = {sel},
    multiple = {mult},
    options = list(plugins = "remove_button")
  )' |>
    glue::glue() |>
    as.character()
}
downloadTable <- function(id, lab) {
  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(outputId = "{id}", label = "{lab}")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    as.character()
}
downloadPlot <- function(outputId) {
  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      shiny::numericInput(inputId = "{outputId}_width", label = "width", value = 15),
      shiny::numericInput(inputId = "{outputId}_height", label = "height", value = 10),
      {selector("{outputId}_units", "Units", {cast(c("px", "cm", "inch"))}, {cast("cm")}, FALSE)},
      shiny::numericInput(inputId = "{outputId}_dpi", label = "dpi", value = 300),
      shiny::downloadButton(outputId = "{outputId}", label = "Download png")
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    glue::glue() |>
    as.character()
}
getRawPanel <- function(tab) {
  id <- paste0(tab, "_raw_download")
  'bslib::nav_panel(
    title = "Raw",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download csv")},
      DT::dataTableOutput("{tab}_raw")
    )
  )' |>
    glue::glue() |>
    as.character()
}
getTidyPanel <- function(tab) {
  id <- paste0(tab, "_tidy_download")
  'bslib::nav_panel(
    title = "Tidy",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download csv")},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::checkboxInput(
            inputId = "{tab}_tidy_settings",
            label = "Show settings",
            value = FALSE
          ),
          shiny::checkboxInput(
            inputId = "{tab}_tidy_groupping",
            label = "Show groupping",
            value = TRUE
          ),
          shiny::radioButtons(
            inputId = "{tab}_tidy_pivot",
            label = "Pivot estimates/variables",
            choices = c("none", "estimates", "estimates and variables"),
            selected = "none"
          ),
          position = "right"
        ),
        DT::dataTableOutput("{tab}_tidy")
      )
    )
  )' |>
    glue::glue() |>
    as.character()
}
getFormattedPanel <- function(tab, choic) {
  hide <- names(choic$settings)
  none <- c(names(choic$groupping), "varaible_name", "variable_level",
            "estimate_name")
  header <- "cdm_name"
  header <- header[header %in% none]
  none <- none[!none %in% header]
  group <- "cohort_name"
  group <- group[group %in% none]
  none <- none[!none %in% group]
  id <- paste0(tab, "_formatted_download")
  'bslib::nav_panel(
    title = "Formatted",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download word")},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          sortable::bucket_list(
            header = NULL,
            sortable::add_rank_list(
              text = "None",
              labels = {cast(none)},
              input_id = "{tab}_formatted_none"
            ),
            sortable::add_rank_list(
              text = "Header",
              labels = {cast(header)},
              input_id = "{tab}_formatted_header"
            ),
            sortable::add_rank_list(
              text = "Group",
              labels = {cast(group)},
              input_id = "{tab}_formatted_group"
            ),
            sortable::add_rank_list(
              text = "Hide",
              labels = {cast(hide)},
              input_id = "{tab}_formatted_hide"
            )
          ),
          position = "right"
        ),
        gt::gt_output("{tab}_formatted")
      )
    )
  )' |>
    glue::glue() |>
    as.character()
}
getPlotsPanel <- function(tab, choic) {
  plots <- getPlots(tab)
  panels <- purrr::map_chr(plots, \(x) getPlotPanel(x, tab, choic)) |>
    paste0(collapse = ",\n")
  return(panels)
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
getPlotPanel <- function(id, tab, choic) {
  tit <- getPlotTitle(id)
  buttons <- getPlotButtons(
    tab, id, names(choic$settings), names(choic$groupping),
    c("variable_name", "variable_level", "estimate_name"))
  buttons <- paste0(c(buttons, 'position = "right"'), collapse = ",\n")
  out <- getPlotOutput(id)
  downloadId <- paste0(tab, "_plot_", id, "_download")
  'bslib::nav_panel(
    title = "{tit}",
    bslib::card(
      full_screen = TRUE,
      {downloadPlot(downloadId)},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          {buttons}
        ),
        {out}("{tab}_plot_{id}")
      )
    )
  )' |>
    glue::glue() |>
    as.character()
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
  but <- purrr::map_chr(seq_len(nrow(buts)), \(k) {
    arg <- buts$argument[k]
    type <- buts$type[k]
    opts <- getButtonOpts(buts$opts[k], setCols, groupCols, varCols) |>
      cast()
    def <- cast(args[[arg]])
    multiple <- buts$multiple[k]
    glue::glue(getButton(type))
  })
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
         "selector" = selector(
           id = '{rt}_plot_{plotId}_{formatSnake(arg)}',
           lab = '{arg}',
           cho = '{opts}',
           sel = '{def}',
           mult = '{multiple}'),
         "check" = "shiny::checkboxInput(
           inputId = '{rt}_plot_{plotId}_{formatSnake(arg)}',
           label = '{arg}',
           value = {def}
         )")
}

emptySummarisedResult <- function() {
  omopgenerics::emptySummarisedResult(settings = dplyr::tibble(
    result_id = integer(),
    result_type = character(),
    package_name = character(),
    package_version = character()
  ))
}
