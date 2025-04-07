
#' Launch a dynamic shiny app where you can upload results.
#'
#' @return Launches the shiny app.
#' @export
#'
launchDynamicApp <- function() {
  ui <- shiny::uiOutput("ui")
  shiny::shinyApp(ui = ui, server = serverDynamic)
}

serverDynamic <- function(input, output, session) {

  local(options(shiny.maxRequestSize = 10 * 1024^3))

  # reactive data
  panels <- shiny::reactiveVal(list())
  workingData <- shiny::reactiveVal(list())
  uploadedData <- shiny::reactiveVal(dplyr::tibble(
    id = integer(), name_export = character(), upload_datetime = character(),
    number_rows = integer(), content = list(), is_empty = logical()
  ))

  # render ui
  output$ui <- shiny::renderUI(
    createDynamicUi(panels(), input$configuration_summary, workingData(), input$configuration_theme)
  )

  # change the theme of the shiny
  shiny::observeEvent(input$configuration_theme, {
    theme <- omopViewerThemes[[input$configuration_theme]] |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
    session$setCurrentTheme(theme)
  })

  # upload data to shiny
  shiny::observeEvent(input$upload_data_content, {
    dataList <- processFiles(input$upload_data_content)
    uploadedData(mergeData(dataList$data, uploadedData()))
    output$upload_data_message <- shiny::renderText(dataList$message)
  })

  # show loaded data in shiny
  output$upload_data_uploaded <- DT::renderDataTable(
    uploadedDatatable(uploadedData())
  )

  # update shiny with the new data
  shiny::observeEvent(input$load_data_go, {
    # bind all data together
    dataToUpload <- uploadedData() |>
      dplyr::slice(input$upload_data_uploaded_rows_selected) |>
      dplyr::pull("content") |>
      omopgenerics::bind()
    if (is.null(dataToUpload)) {
      dataToUpload <- omopgenerics::emptySummarisedResult()
    }

    # update panels
    panels(panelsUi(dataToUpload))

    # panelDetails
    panelDetails <- panelDetailsFromResult(dataToUpload)

    # create the new workingData()
    resultList <- resultListFromPanelDetails(panelDetails)
    workingData(prepareResult(dataToUpload, resultList))

    # add server modules
    serverModule <- paste0(c(
      "function(input, output, session) {",
      createSummaryServer(summary = input$configuration_summary, data = "workingData()"),
      createServer(panelDetails, data = "workingData()"),
      "}"
    ), collapse = "\n") |>
      rlang::parse_expr() |>
      rlang::eval_tidy()
    shiny::moduleServer(id = NULL, module = serverModule)
  })

}
createDynamicUi <- function(panels, summary, data, theme) {
  logo <- "https://raw.githubusercontent.com/OHDSI/OmopViewer/12fbe3ad94529a91f46f3652e47b417e9a7f4bb6/inst/logos/hds_logo.svg"

  if (isTRUE(summary)) {
    panels <- c(
      list(rlang::eval_tidy(rlang::parse_exprs(summaryTab(TRUE)))),
      panels
    )
    summary <- TRUE
  } else {
    summary <- FALSE
  }

  if (!isTRUE(theme %in% names(omopViewerThemes))) {
    theme <- "default"
  }

  panels <- unname(panels)
  bslib::page_navbar(
    # title
    title = shiny::tags$span(
      shiny::tags$img(
        src = logo, width = "auto", height = "46px", class = "me-3",alt = "logo"
      ),
      "OmopViewer App"
    ),
    # load ui
    bslib::nav_panel(
      title = "Load data",
      icon = shiny::icon("upload"),
      bslib::card(
        bslib::card_header("Upload data to the server"),
        shiny::fileInput(
          inputId = "upload_data_content",
          label = "Choose zip or csv file",
          accept = c(".zip", ".csv"),
          multiple = TRUE
        ),
        shiny::textOutput(outputId = "upload_data_message")
      ),
      bslib::card(
        bslib::card_header("Select data to load"),
        fill = FALSE,
        shiny::p("Select the data do you want to load:"),
        DT::dataTableOutput(outputId = "upload_data_uploaded"),
        shiny::actionButton(
          inputId = "load_data_go",
          label = "Bind data and load to shiny",
          icon = shiny::icon("gears")
        ),
        shiny::textOutput(outputId = "load_data_message")
      )
    ),
    # ui created by server
    !!!panels,
    # separator
    bslib::nav_spacer(),
    # flag that it was created by OmopViewer
    createAbout(logo) |>
      rlang::parse_expr() |>
      rlang::eval_tidy(),
    # configuration
    bslib::nav_item(
      bslib::popover(
        trigger = shiny::icon("gear"),
        title = "Configuration",
        shinyWidgets::pickerInput(
          inputId = "configuration_theme",
          label = "Choose theme",
          choices = names(omopViewerThemes),
          selected = theme,
          multiple = FALSE
        ),
        shiny::checkboxInput(
          inputId = "configuration_summary",
          label = "Summary",
          value = summary
        )
      )
    ),
    # dark mode
    bslib::nav_item(bslib::input_dark_mode(id ="dark_mode", mode = "light"))
  )
}
panelsUi <- function(result) {
  # create panelDetails
  panelDetails <- panelDetailsFromResult(result) |>
    populatePanelDetailsOptions(result)
  # create panels
  panels <- writeUiPanels(panelDetails)

  # resultList from panelDetails
  resultList <- resultListFromPanelDetails(panelDetails)

  # filterValues from resultList
  values <- getValues(result, resultList)
  choices <- values
  selected <- values

  # evaluate the panels
  purrr::map(panels, \(x) rlang::eval_tidy(rlang::parse_expr(x)))
}
processFiles <- function(content) {
  if (is.null(content)) return(list(message = "No data selected!"))
  files <- readFiles(content$datapath)
  list(
    data = files,
    message = messageProcessFiles(files)
  )
}
captureMessage <- function(x) {
  x <- rlang::catch_cnd(x, classes = "message")
  cli::ansi_strip(x$message) |>
    stringr::str_replace("\n ", "")
}
readFiles <- function(datapath) {
  x <- purrr::map_df(datapath, \(x) {
    ext <- tools::file_ext(x)
    nm <- basename(x)
    if (ext == "csv") {
      x <- dplyr::tibble(
        file_name = x,
        name_export = nm,
        folder_delete = NA_character_
      )
    } else if (ext == "zip") {
      tempDir <- tempdir()
      zip::unzip(x, exdir = tempDir)
      x <- dplyr::tibble(
        file_name = list.files(tempDir, pattern = "\\.csv$", full.names = TRUE),
        name_export = paste0(nm, "/", basename(.data$file_name)),
        folder_delete = tempDir
      )
    }
    return(x)
  }) |>
    dplyr::rowwise() |>
    dplyr::mutate(content = list(suppressWarnings(
      omopgenerics::importSummarisedResult(.data$file_name)
    ))) |>
    dplyr::mutate(number_rows = nrow(.data$content)) |>
    dplyr::ungroup() |>
    dplyr::mutate(is_empty = .data$number_rows == 0)
  unlink(unique(x$folder_delete))
  x |>
    dplyr::select(!c("folder_delete", "file_name"))
}
messageProcessFiles <- function(x) {
  if (sum(x$is_empty) > 0) {
    err <- c("x" = "No content found in {sum(x$is_empty)} file{?s}: {.var {x$name_export[x$is_empty]}}.
    Inputs must be {.cls summarised_result} as defined in {.pkg omopgenerics}.") |>
      cli::cli_inform() |>
      captureMessage()
  } else {
    err <- NULL
  }
  if (sum(!x$is_empty) > 0) {
    goo <- c("v" = "{sum(!x$is_empty)} file{?s} processed correctly.") |>
      cli::cli_inform() |>
      captureMessage()
  } else {
    goo <- NULL
  }
  mes <- c("i" = "{nrow(x)} files processed:") |>
    cli::cli_inform() |>
    captureMessage()
  c(mes, err, goo) |>
    paste0(collapse = "<br>")
}
mergeData <- function(newData, originalData) {
  if (is.null(newData)) return(originalData)
  id0 <- getOption("OmopViewer.number_ids", 0L)
  newData <- newData |>
    dplyr::mutate(
      id = .env$id0 + dplyr::row_number(),
      upload_datetime = Sys.time() |> format("%Y-%m-%d %H:%M:%S")
    )
  options("OmopViewer.number_ids" = id0 + nrow(newData))
  if (is.null(originalData)) {
    return(newData)
  } else {
    return(originalData |> dplyr::union_all(newData))
  }
}
uploadedDatatable <- function(x) {
  x <- x |>
    dplyr::mutate(result_type = purrr::map_chr(x$content, \(x) {
      x <- omopgenerics::settings(x)
      if ("result_type" %in% colnames(x)) {
        return(paste0(unique(x$result_type), collapse = "; "))
      }
      return("")
    })) |>
    dplyr::select(
      "file_id" = "id", "file_name" = "name_export", "upload_datetime",
      "result_type", "number_rows"
    )
  DT::datatable(
    x,
    selection = list(mode = "multiple"),
    options = list(paging = FALSE, info = FALSE, scrollX = TRUE, scrollY = "200px")
  )
}
