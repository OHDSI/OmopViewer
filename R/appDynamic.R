
#' Launch a dynamic shiny app where you can upload results.
#'
#' @return Launches the shiny app.
#' @export
#'
launchDynamicApp <- function() {
  ui <- bslib::page_fluid(shiny::uiOutput("dynamic_tabs_output"))
  shiny::shinyApp(ui = ui, server = serverDynamic)
}

createDynamicUi <- function(result) {
  logo <- "https://OHDSI.github.io/Oxinfer/images/hds_logo_noline.svg"
  panelDetails <- panelDetailsFromResult(result)
  panelStructure <- panelStructureFromResult(result)
  panels <- createUiPanels(panelDetails) |>
    structurePanels(panelStructure)
  c(
    'bslib::page_navbar(',
    c(
      pageTitle("OmopViewer App", logo),
      loadDataUi(),
      panels,
      'bslib::nav_spacer()',
      createAbout(logo),
      'bslib::nav_item(bslib::input_dark_mode(id ="dark_mode", mode = "light"))'
    ) |>
      paste0(collapse = ",\n"),
    ')'
  ) |>
    paste0(collapse = "\n") |>
    rlang::parse_expr() |>
    rlang::eval_tidy()
}
loadDataUi <- function() {
  'bslib::nav_panel(
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
  )'
}

serverDynamic <- function(input, output, session) {

  output$dynamic_tabs_output <- shiny::renderUI({
    createDynamicUi(omopgenerics::emptySummarisedResult())
  })

  uploadedData <- shiny::reactiveVal(dplyr::tibble(
    id = integer(),
    name_export = character(),
    upload_datetime = character(),
    number_rows = integer(),
    content = list(),
    is_empty = logical()
  ))

  workingData <- shiny::reactiveVal(prepareShinyData(omopgenerics::emptySummarisedResult()))

  shiny::observeEvent(input$upload_data_content, {
    dataList <- processFiles(input$upload_data_content)
    uploadedData(mergeData(dataList$data, uploadedData()))
    output$upload_data_message <- shiny::renderText(dataList$message)
  })

  output$upload_data_uploaded <- DT::renderDataTable({
    uploadedDatatable(uploadedData())
  })

  shiny::observeEvent(input$load_data_go, {
    dataToUpload <- uploadedData() |>
      dplyr::slice(input$upload_data_uploaded_rows_selected) |>
      dplyr::pull("content") |>
      omopgenerics::bind()
    workingData(prepareShinyData(dataToUpload))
  })

  shiny::observeEvent(workingData(), {
    # choices <- getChoices(workingData())
    # output$dynamic_tabs_output <- shiny::renderUI(
    #   createDynamicUi(choices)
    # )
    # for (rt in names(choices)) {
    #   serverResultType <- paste0(
    #     "function(input, output, session) {", createServer(rt, data = "workingData()"), "}") |>
    #     rlang::parse_expr() |>
    #     rlang::eval_tidy()
    #   shiny::moduleServer(id = NULL, module = serverResultType)
    # }
  })

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
