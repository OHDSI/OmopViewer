
# ui ----
getOutputIds <- function(resultType) {
  tabId <- omopViewerTabs$result_tab_id[omopViewerTabs$result_type == resultType]
  output <- omopViewerOutput |>
    dplyr::filter(.data$result_tab_id %in% .env$tabId)
  id <- if ("gt" %in% output$output_type) integer() else 0L
  id <- c(id, output$output_id)
  return(id)
}
getOutputFunction <- function(outputId) {
  omopViewerOutput$output_function[omopViewerOutput$output_id == outputId]
}
getOutputTitle <- function(outputId) {
  omopViewerOutput$output_title[omopViewerOutput$output_id == outputId]
}
getOutputType <- function(outputId) {
  omopViewerOutput$output_type[omopViewerOutput$output_id == outputId]
}
getOutputUi <- function(outputId) {
  type <- getOutputType(outputId)
  switch (type,
          "gt" = 'gt::gt_output("{prefix}")',
          "grViz" = 'DiagrammeR::grVizOutput("{prefix}")',
          "ggplot2" = 'shiny::plotOutput("{prefix}")'
  )
}
getOutputDownload <- function(outputId) {
  type <- getOutputType(outputId)
  extraButtons <- switch (
    type,
    "gt" = 'shinyWidgets::pickerInput(
      inputId = "{prefix}_download_type",
      label = "File type",
      selected = "docx",
      choices = c("docx", "png", "pdf", "html"),
      multiple = FALSE
    )',
    "grViz" = c(
      'shiny::numericInput(
        inputId = "{prefix}_download_width",
        label = "Width (px)",
        value = 15
      )',
      'shiny::numericInput(
        inputId = "{prefix}_download_height",
        label = "Height (px)",
        value = 10
      )'
    ),
    "ggplot2" = c(
      'shiny::numericInput(
        inputId = "{prefix}_download_width",
        label = "Width",
        value = 15
      )',
      'shiny::numericInput(
        inputId = "{prefix}_download_height",
        label = "Height",
        value = 10
      )',
      'shinyWidgets::pickerInput(
        inputId = "{prefix}_download_units",
        label = "Units",
        selected = "cm",
        choices = c("px", "cm", "inch"),
        multiple = FALSE
      )',
      'shiny::numericInput(
        inputId = "{prefix}_download_dpi",
        label = "dpi",
        value = 300
      )'
    )
  )
  x <- c(extraButtons, 'shiny::downloadButton(outputId = "{prefix}_download", label = "Download")') |>
    paste0(collapse = ",\n")
  'bslib::card_header(
    bslib::popover(
      shiny::icon("download"),
      {x}
    ),
    class = "text-end"
  )' |>
    glue::glue() |>
    as.character()
}
getOutputSidebar <- function(outputId, setCols, groupCols, varCols) {
  # get buttons to create
  buttons <- omopViewerOutputArguments |>
    dplyr::filter(.data$output_id == .env$outputId) |>
    substituteOptions(setCols, groupCols, varCols)

  if (nrow(buttons) == 0) return("")

  # defaults
  defaults <- getOutputFunction(outputId) |>
    rlang::parse_expr() |>
    rlang::eval_tidy() |>
    formals()

  # get all buttons
  c(getRankButtons(buttons, defaults), getOutputButtons(buttons, defaults)) |>
    paste0(collapse = ",\n")
}
substituteOptions <- function(buts, setCols, groupCols, varCols) {
  opts <- c("options", "rank_options")
  if (!any(opts %in% buts$name)) return(buts)
  buts|>
    dplyr::left_join(
      buts |>
        dplyr::select("name", "value") |>
        dplyr::filter(.data$name %in% .env$opts) |>
        dplyr::distinct() |>
        dplyr::rowwise() |>
        dplyr::mutate("options" = getButtonOpts(
          .data$value, setCols, groupCols, varCols)) |>
        dplyr::ungroup(),
      by = c("name", "value")
    ) |>
    dplyr::mutate("value" = dplyr::if_else(
      .data$name %in% .env$opts, .data$options, .data$value
    )) |>
    dplyr::select(-"options")
}
getButtonOpts <- function(opts, setCols, groupCols, varCols) {
  stringr::str_split_1(opts, pattern = ", ") |>
    subs("<grouping>", groupCols) |>
    subs("<settings>", setCols) |>
    subs("<variable>", varCols) |>
    stringr::str_flatten_comma()
}
getRankButtons <- function(buttons, defaults) {
  # get rank buttons
  args <- buttons |>
    dplyr::filter(.data$name == "type", .data$value == "rank") |>
    dplyr::pull("argument")
  if (length(args) == 0) return(character())

  # all rank options
  allRankOptions <- buttons$value[buttons$name == "rank_options"] |>
    stringr::str_split_1(", ")

  buttons <- buttons |>
    dplyr::filter(.data$argument %in% .env$args)

  # get default values
  defaults <- args |>
    rlang::set_names() |>
    purrr::map(\(x) {
      def <- buttons$value[buttons$argument == x & buttons$name == "default"]
      if (length(def) == 0) {
        def <- tryCatch(
          defaults[[x]] |> rlang::eval_tidy(),
          error = function(e) character()
        )
      }
      return(def)
    })

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
      id <- paste0("{prefix}_", nm)
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
getOutputButtons <- function(buttons, defaults) {
  args <- buttons |>
    dplyr::filter(.data$name == "type", .data$value != "rank") |>
    dplyr::pull("argument")
  if (length(args) == 0) return(character())
  args |>
    purrr::map_chr(\(x) {
      buttons |>
        dplyr::filter(.data$argument == .env$x) |>
        createButton(defaults, prefix = "{prefix}")
    })
}
getOutputPrefix <- function(x, rt) {
  paste0(rt, "_", getOutputType(x), "_", x)
}

outputUi <- function(tab, choic) {
  setCols <- choic[startsWith(x = choic, prefix = "settings_")]
  setCols <- substr(setCols, 10, nchar(setCols))
  groupCols <- choic[startsWith(x = choic, prefix = "grouping_")]
  groupCols <- substr(groupCols, 10, nchar(groupCols))
  varCols <- c("variable_name", "variable_level", "estimate_name")
  tab |>
    getOutputIds() |>
    purrr::map_chr(\(x) {
      title <- getOutputTitle(x)
      download <- getOutputDownload(x)
      sidebar <- getOutputSidebar(x, setCols, groupCols, varCols)
      output <- getOutputUi(x)
      prefix <- getOutputPrefix(x, tab)
      if (sidebar == "") {
        res <- 'bslib::nav_panel(
          title = "{title}",
          bslib::card(
            full_screen = TRUE,
            {download},
            {output}
          )
        )'
      } else {
        res <- 'bslib::nav_panel(
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
        )'
      }
      res |>
        glue::glue() |>
        glue::glue() |>
        as.character()
    }) |>
    paste0(collapse = ",\n")
}

# server ----
saveOutput <- function(id, rt) {
  type <- getOutputType(id)
  savefun <- switch(
    type,
    "gt" = "gt::gtsave(data = obj, filename = file)",
    "ggplot2" = "ggplot2::ggsave(
      filename = file,
      plot = obj,
      width = as.numeric(input$[prefix]_download_width),
      height = as.numeric(input$[prefix]_download_height),
      units = input$[prefix]_download_units,
      dpi = as.numeric(input$[prefix]_download_dpi)
    )",
    "grViz" = "DiagrammeR::export_graph(
      graph = obj,
      file_name = file,
      fily_type = 'png',
      width = as.numeric(input$[prefix]_download_width),
      height = as.numeric(input$[prefix]_download_height)
    )"
  )
  out <- switch(
    type,
    "gt" = "input$[prefix]_download_type",
    "ggplot2" = '"png"',
    "grViz" = '"png"'
  )
  prefix <- getOutputPrefix(id, rt)
  'output$[prefix]_download <- shiny::downloadHandler(
    filename = paste0("output_[type]_[rt].", [out]),
    content = function(file) {
      obj <- createOutput[id]()
      [savefun]
    }
  )' |>
    glue::glue(.open = "[", .close = "]") |>
    as.character()
}
renderOutput <- function(id) {
  output <- omopViewerOutput$output_type[omopViewerOutput$output_id == id]
  switch(output,
         "gt" = "gt::render_gt",
         "ggplot2" = "shiny::renderPlot",
         "grViz" = "DiagrammeR::renderGrViz")
}

outputServer <- function(rt, outputId, data) {
  if (length(outputId) == 0) return(NULL)

  purrr::map_chr(outputId, \(id) {
    prefix <- getOutputPrefix(id, rt)
    args <- omopViewerOutputArguments |>
      dplyr::filter(.data$output_id == .env$id & !is.na(.data$argument)) |>
      dplyr::pull("argument") |>
      unique()
    inputs <- c(
      "result",
      purrr::map_chr(args, \(x) paste0(x, " = input$", prefix, "_", x))
    ) |>
      paste0(collapse = ",\n")
    c("## output [id] -----",
      'createOutput[id] <- shiny::reactive({
        result <- [data] |>
          filterData("[rt]", input)
        [getOutputFunction(id)](
          [inputs]
        )
      })',
      'output$[prefix] <- [renderOutput(id)]({
        createOutput[id]()
      })',
      saveOutput(id, rt)
    ) |>
      purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]")) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n\n")
}
