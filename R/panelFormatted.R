
getTableId <- function(resultType) {
  tabId <- omopViewerTabs$result_tab_id[omopViewerTabs$result_type == resultType]
  tableId <- omopViewerTables$table_id[omopViewerTables$result_tab_id == tabId]
  if (length(tableId) == 0) tableId <- 0
  return(tableId)
}
getTableTitle <- function(tableId) {
  omopViewerTables$title[omopViewerTables$table_id == tableId]
}
getTableDownload <- function(tableId, resultType) {
  out <- omopViewerTables$output[omopViewerTables$table_id == tableId]
  id <- paste0(resultType, "_formatted_download_", tableId)
  switch(out,
         "gt" = downloadTable(id, "Download", c("docx", "png", "pdf", "html")),
         cli::cli_abort("provide a supported output format"))
}
getTableSidebar <- function(tableId, resultType, choices) {
  buttons <- omopViewerTableArguments |>
    dplyr::filter(.data$table_id == .env$tableId)

  c(getNoRankButtons(buttons, tableId, resultType),
    getRankButtons(buttons, tableId, resultType, choices)) |>
    paste0(collapse = ",\n")
}
getTableOutput <- function(tableId, resultType) {
  out <- omopViewerTables$output[omopViewerTables$table_id == tableId]
  id <- paste0(resultType, "_formatted_", tableId)
  switch(out,
         "gt" = paste0('gt::gt_output("', id, '")'),
         cli::cli_abort("provide a supported output format"))
}
getTableFunction <- function(tableId) {
  fun <- omopViewerTables$fun[omopViewerTables$table_id == tableId]
  if (!grepl("::", fun, fixed = TRUE)) {
    resultTabId <- omopViewerTables$result_tab_id[omopViewerTables$table_id == tableId]
    pkg <- omopViewerTabs$package[omopViewerTabs$result_tab_id == resultTabId]
    fun <- paste0(pkg, "::", fun)
  }
  return(fun)
}
getTableDefault <- function(rankButtons, buttons, funDefaults) {
  rankButtons |>
    rlang::set_names() |>
    purrr::map(\(x) {
      def <- buttons |>
        dplyr::filter(.data$argument == .env$x, .data$name == "default") |>
        dplyr::pull("value")
      if (length(def) == 0) {
        def <- tryCatch(rlang::eval_tidy(funDefaults[[x]]),
                        error = function(e) character())
      }
      return(def)
    })
}
getRankButtons <- function(buttons, tableId, resultType, choices) {
  # all rank options
  allRankOptions <- buttons$value[buttons$name == "rank_options"]

  # get rank buttons
  args <- buttons |>
    dplyr::filter(.data$name == "type", .data$value == "rank") |>
    dplyr::pull("argument")
  if (length(args) == 0) return(character())
  buttons <- buttons |>
    dplyr::filter(.data$argument %in% .env$args)

  # get function default values
  funDefaults <- getTableFunction(tableId) |>
    rlang::parse_expr() |>
    rlang::eval_tidy() |>
    formals()

  # correct default values
  allRankOptions <- subDefaults(allRankOptions, choices)

  # get default values
  defaults <- getTableDefault(args, buttons, funDefaults) |>
    purrr::map(\(x) if (is.null(x)) character() else x) |>
    purrr::map(\(x) subDefaults(x, choices))

  # buttons values must be in allRankOptions
  defaults <- purrr::map(defaults, \(x) x[x %in% allRankOptions])

  # add none button
  defaults <- c(
    list(none = allRankOptions[!allRankOptions %in% unlist(defaults)]),
    defaults
  )

  # create rank buttons
  ranks <- defaults |>
    purrr::imap_chr(\(x, nm) {
      text <- cast(nm)
      labels <- cast(x)
      id <- paste0(resultType, "_formatted_", tableId, "_", nm)
      'sortable::add_rank_list(
        text = {text},
        labels = {labels},
        input_id = "{id}"
      )' |>
        glue::glue() |>
        as.character()
    }) |>
    paste0(collapse = ",\n")

  # return the bucket list block
  paste('sortable::bucket_list(', 'header = NULL,', ranks, ')', sep = '\n')
}
subDefaults <- function(x, choices) {
  x |>
    strsplit(split = ", ") |>
    unlist() |>
    subs("<settings>", names(choices$settings)) |>
    subs("<grouping>", names(choices$grouping)) |>
    subs("<variable>", c("variable_name", "variable_level", "estimate_name"))
}
getNoRankButtons <- function(buttons, tableId, resultType) {
  args <- buttons |>
    dplyr::filter(.data$name == "type", .data$value != "rank") |>
    dplyr::pull("argument")
  if (nrow(buttons) == 0) return(character())
  unique(args) |>
    rlang::set_names() |>
    purrr::map_chr(\(x) {
      buttons |>
        dplyr::filter(.data$argument == .env$x) |>
        getNoRankButton()
    })
}
getNoRankButton <- function(x) {
  tableId <- unique(x$table_id)
  rt <- omopViewerTabs$result_type[
    omopViewerTabs$result_tab_id == omopViewerTables$result_tab_id[
      omopViewerTables$table_id == tableId]]
  arg <- unique(x$argument)
  def <- getTableFunction(tableId) |>
    rlang::parse_expr() |>
    rlang::eval_tidy() |>
    formals()
  def <- def[[unique(x$argument)]] |> rlang::eval_tidy() |> cast()
  type <- x$value[x$name == "type"]
  if (type == "check") {
    res <- "shiny::checkboxInput(
      inputId = '{rt}_formatted_{tableId}_{arg}',
      label = '{arg}',
      value = {def}
    )"
  } else if (type == "selector") {
    cho <- stringr::str_split_1(x$value[x$name == "options"], ", ")
    mult <- x$value[x$name == "multiple"]
    res <- 'shiny::selectizeInput(
      inputId = "{rt}_formatted_{tableId}_{arg}",
      label = {cast(arg)},
      choices = {cast(cho)},
      selected = {def},
      multiple = {mult}
    )'
  } else {
    cli::cli_abort("provide a valid button type")
  }
  res <- res |>
    glue::glue() |>
    as.character()
  return(res)
}

# ui ----
formattedUi <- function(tab, choic) {
  tableId <- getTableId(tab)
  purrr::map_chr(tableId, \(x) {
    title <- getTableTitle(x)
    download <- getTableDownload(x, tab)
    sidebar <- getTableSidebar(x, tab, choic)
    output <- getTableOutput(x, tab)
    'bslib::nav_panel(
      title = "{title}",
      bslib::card(
        full_screen = TRUE,
        {download},
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            {sidebar},
            position = "right"
          ),
         {output}
        )
      )
    )' |>
      glue::glue() |>
      as.character()
  }) |>
    paste0(collapse = ",\n")
}

# server ----
formattedServer <- function(rt, data) {
  rt |>
    getTableId() |>
    purrr::map_chr(\(x) {
      fun <- getTableFunction(x)
      args <- omopViewerTableArguments$argument[omopViewerTableArguments$table_id == x] |>
        unique() |>
        purrr::discard(is.na)
      args <- paste0(args, " = input$", rt, "_formatted_", x, "_", args, collapse = ",\n")
      out <- omopViewerTables$output[omopViewerTables$table_id == x]
      render <- switch (out,
                        "gt" = "gt::render_gt")
      download <- switch (out,
                          "gt" = "gt::gtsave")
      c(
      'getFormattedData[formatCamel(rt)] <- shiny::reactive({
        [data] |>
          OmopViewer::filterData("[rt]", input) |>
          [fun](
            [args]
          )
       })' |>
        glue::glue(.open = "[", .close = "]"),
      'output$[rt]_formatted_[x] <- [render]({
        getFormattedData[formatCamel(rt)]()
      })' |>
        glue::glue(.open = "[", .close = "]"),
      'output$[rt]_formatted_download_[x] <- shiny::downloadHandler(
        filename = function() {
          paste0("formatted_[rt].", input$[rt]_formatted_download_[x]_type)
        },
        content = function(file) {
          getFormattedData[formatCamel(rt)]() |>
            [download](filename = file)
        }
      )' |>
        glue::glue(.open = "[", .close = "]")
      ) |>
        as.character() |>
        paste0(collapse = "\n")
    }) |>
    paste0(collapse = "\n")
}
