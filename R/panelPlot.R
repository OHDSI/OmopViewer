
# ui -----
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

# server ----
getPlotRt <- function(rt) {
  plots <- getPlots(rt)
  if (length(plots) == 0) return(NULL)

  plotServer <- purrr::map_chr(plots, \(id) {
    c('createPlot[id] <- shiny::reactive({
        result <- data |>
          filterData("[rt]", input)
        [createPlotFunction(id)]
      })',
      'output$[rt]_plot_[id] <- [renderPlotFunction(id)]({
        createPlot[id]()
      })',
      'output$[rt]_plot_[id]_download <- shiny::downloadHandler(
        filename = "plot_[rt].png",
        content = function(file) {
          plt <- createPlot[id]()
          [savePlotFunction(id)]
        }
      )'
    ) |>
      purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]")) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n\n")

  return(plotServer)
}
createPlotFunction <- function(id) {
  fun <- omopViewerPlots$fun[omopViewerPlots$plot_id == id]
  resultTabId <- omopViewerPlots$result_tab_id[omopViewerPlots$plot_id == id]
  pkg <- omopViewerTabs$package[omopViewerTabs$result_tab_id == resultTabId]
  rt <- omopViewerTabs$result_type[omopViewerTabs$result_tab_id == resultTabId]
  arguments <- omopViewerPlotArguments |>
    dplyr::filter(.data$plot_id == .env$id) |>
    dplyr::pull("argument")
  if (length(arguments) == 0) {
    args <- ""
  } else {
    args <- paste0(
      ",\n ", arguments, " = input$", rt, "_plot_", id, "_", formatSnake(arguments), collapse = "")
  }
  "{pkg}::{fun}(
    result{args})" |>
    glue::glue()
}
savePlotFunction <- function(id) {
  output <- omopViewerPlots$output[omopViewerPlots$plot_id == id]
  switch(output,
         "ggplot2" = "ggplot2::ggsave(filename = file, plot = plt)",
         "grViz" = "DiagrammeR::export_graph(graph = plt, file_name = file, fily_type = 'png', width = 800)")
}
renderPlotFunction <- function(id) {
  output <- omopViewerPlots$output[omopViewerPlots$plot_id == id]
  switch(output,
         "ggplot2" = "shiny::renderPlot",
         "grViz" = "DiagrammeR::renderGrViz")
}
