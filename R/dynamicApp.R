
#' Launch a dynamic shiny app wher you can upload results.
#'
#' @return Launches the shiny app.
#' @export
#'
launchDynamicApp <- function() {
  shiny::shinyApp(ui = uiDynamic(), server = serverDynamic)
}

launchDynamicApp2 <- function() {
  ui <- bslib::page_fluid(shiny::uiOutput("dynamic_tabs_output"))
  shiny::shinyApp(ui = ui, server = serverDynamic2)
}

serverDynamic2 <- function(input, output, session) {

  output$dynamic_tabs_output <- shiny::renderUI(createDynamicUi(list()))

  uploadedData <- shiny::reactiveVal(dplyr::tibble(
    id = integer(),
    name_export = character(),
    upload_datetime = character(),
    number_rows = integer(),
    content = list(),
    is_empty = logical()
  ))

  workingData <- shiny::reactiveVal(emptySummarisedResult())

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
    workingData(dataToUpload)
  })

  shiny::observeEvent(workingData(), {
    choices <- getChoices(workingData())
    output$dynamic_tabs_output <- shiny::renderUI(
      createDynamicUi(choices)
    )
  })

}

createDynamicUi <- function(choices) {
  c(
    'bslib::page_navbar(',
    c(
      pageTitle("omopViewer App", base::system.file("www/images/hds_logo.svg")),
      loadDataUi(),
      purrr::map_chr(names(choices), \(x) createUiResultType(x, choices[[x]])),
      'bslib::nav_spacer()',
      createAbout(),
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
captureMessage <- function(x) {
  x <- rlang::catch_cnd(x, classes = "message")
  cli::ansi_strip(x$message) |>
    stringr::str_replace("\n ", "")
}
processFiles <- function(content) {
  if (is.null(content)) return(list(message = "No data selected!"))
  files <- readFiles(content$datapath)
  list(
    data = files,
    message = messageProcessFiles(files)
  )
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
      unzip(x, exdir = tempDir)
      x <- dplyr::tibble(
        file_name = list.files(tempDir, pattern = "\\.csv$", full.names = TRUE),
        name_export = paste0(nm, "/", basename(file_name)),
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
  id0 <- getOption("omopViewer.number_ids", 0L)
  newData <- newData |>
    dplyr::mutate(
      id = .env$id0 + dplyr::row_number(),
      upload_datetime = Sys.time() |> format("%Y-%m-%d %H:%M:%S")
    )
  options("omopViewer.number_ids" = id0 + nrow(newData))
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

#' Server Function for Shiny Application
#'
#' This function contains the server logic for a Shiny application designed to manage and
#' display data studies. It handles file uploads, data processing, and UI updates based
#' on the data state.
#'
#' @param input Standard Shiny server input parameter
#' @param output Standard Shiny server output parameter
#' @param session Standard Shiny session parameter
#'
#' @return The function sets up a series of reactive expressions and
#'   observers to manage app state.
#' @export
#'
serverDynamic <- function(input, output, session) {
  # Configuration for plot types and their corresponding modules
  plot_config <- list(
    summarised_large_scale_characteristics = list(
      name = "summarised_large_scale_characteristics",
      plotFunc = CohortCharacteristics::plotLargeScaleCharacteristics,
      uiElements = c("facet", "colour", "style", "strata", "table_filter", "var_level", "variable"),
      plotParams = list(
        facet = list(id = "lsc_plot_facet", multiple = TRUE),
        colour = list(id = "lsc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "lsc_plot_style", multiple = FALSE, choices = c("horizontal", "vertical"), selected = "horizontal"),
        splitStrata = list(id = "lsc_plot_strata", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        table_filter = list(id = "lsc_plot_table_filter", multiple = FALSE),
        variable_level = list(id = "lsc_plot_var_level", multiple = TRUE),
        variable_name = list(id = "lsc_plot_variable", multiple = TRUE)
      ),
      dataArg = "data",
      updatePickerInputIDs = list(
        variable_name = "lsc_plot_variable",
        variable_level = "lsc_plot_var_level"
      )
    ),
    cohort_overlap = list(
      name = "cohort_overlap",
      plotFunc = CohortCharacteristics::plotCohortOverlap,
      uiElements = c("facet", "uniqueCombinations"),
      plotParams = list(
        facet = list(id = "co_plot_facet", multiple = TRUE),
        uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),
      plotButtons = list(
        facet = list(id = "co_plot_facet", multiple = TRUE),
        uniqueCombinations = list(id = "co_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),

      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "co_plot_facet"
      )
    ),
    summarised_characteristics = list(
      name = "summarised_characteristics",
      plotFunc = CohortCharacteristics::plotCharacteristics,
      uiElements = c("facet", "colour", "style", "x", "variable_name"),
      plotButtons= list(
        facet = list(id = "sc_plot_facet", multiple = TRUE),
        colour = list(id = "sc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                         choices = c("boxplot", "barplot"), selected = "barplot"),
        x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                             "variable_level", "estimate_type", "group_name",
                                                                             "group_level"),
                 selected = "variable_name"),
        variable_name = list(id = "sc_plot_variable", multiple = FALSE)
      ),
      plotParams= list(
        facet = list(id = "sc_plot_facet", multiple = TRUE),
        colour = list(id = "sc_plot_colour", multiple = TRUE),
        plotStyle = list(id = "sc_plot_style", multiple = FALSE,
                         choices = c("boxplot", "barplot"), selected = "barplot"),
        x = list(id = "sc_plot_xaxis", multiple = FALSE,         choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                                                                             "variable_level", "estimate_type", "group_name",
                                                                             "group_level"),
                 selected = "variable_name")
      ),
      dataArg = "data",
      updatePickerInputIDs = list(
        variable_name = "sc_plot_variable",
        facet = "sc_plot_facet",
        colour = "sc_plot_colour"
      )
    ),
    cohort_timing = list(
      name = "cohort_timing",
      plotFunc = CohortCharacteristics::plotCohortTiming,
      uiElements = c("facet", "colour", "uniqueCombinations", "plotType"),
      plotParams = list(
        facet = list(id = "ct_plot_facet", multiple = TRUE),
        colour = list(id = "ct_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ct_plot_style", multiple = FALSE,
                        choices = c("boxplot"), selected = "boxplot")
      ),
      plotButtons = list(
        facet = list(id = "ct_plot_facet", multiple = TRUE),
        colour = list(id = "ct_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ct_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE)
      ),

      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "ct_plot_facet",
        colour = "ct_plot_colour"
      )
    ),
    cohort_timing_density = list(
      name = "cohort_timing_density",
      plotFunc = CohortCharacteristics::plotCohortTiming,
      uiElements = c("facet", "colour", "uniqueCombinations", "plotType"),
      plotParams = list(
        facet = list(id = "ctd_plot_facet", multiple = TRUE),
        colour = list(id = "ctd_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ctd_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ctd_plot_style", multiple = FALSE,
                        choices = c("density"), selected = "density")
      ),
      plotButtons = list(
        facet = list(id = "ctd_plot_facet", multiple = TRUE),
        colour = list(id = "ctd_plot_colour", multiple = TRUE),
        uniqueCombinations = list(id = "ctd_unique_comb", multiple = FALSE, choices = c(TRUE, FALSE), selected = TRUE),
        plotType = list(id = "ctd_plot_style", multiple = FALSE,
                        choices = c("density"), selected = "density")
      ),
      dataArg = "result",
      updatePickerInputIDs = list(
        facet = "ctd_plot_facet",
        colour = "ctd_plot_colour"
      )
    )
  )


  options(shiny.maxRequestSize=30*1024^3)
  dataFolder <- "extras/data"
  summarisedResultColumns <- omopgenerics::resultColumns(
    table = "summarised_result")

  # Initialize the uploadData module
  uploadDataInitServer("uploadData", dataFolder)

  # Monitor changes in the studies.rds file
  studies_path <- file.path(here::here(dataFolder), "studies.rds")
  studies <- shiny::reactiveFileReader(1000, session, studies_path, readRDS)

  # Function to create dynamic sidebar menu based on cohort_data


  # Refresh modules when studies.rds changes
  shiny::observe({
    # print("Studies updated")  # Debug print
    studies()
    session$userData$studySelect_upload <- studySelectInitServer("available_studies_upload")
    session$userData$studySelect <- studySelectInitServer("available_studies")
    session$userData$app_data <- dataLoad_myModuleServer("dataLoad", dataFolder)
  })

  cohort_data <- shiny::reactive({
    session$userData$app_data$loaded_data_value()
  })

  process_data <- shiny::reactive({

    datasets <- cohort_data()

    # Check if datasets are defined and not empty
    if (!is.null(datasets) && length(datasets) > 0) {

      # Initialize lists for storing datasets
      missing_required_columns <- list()
      complete_datasets <- list()
      unique_result_ids <- numeric()

      # Iterate over each dataset to classify
      names <- names(datasets)

      for (i in seq_along(names)) {
        data <- datasets[[i]]
        if (all(summarisedResultColumns %in% colnames(data))) {
          data <- omopgenerics::newSummarisedResult(data)
          complete_datasets[[length(complete_datasets) + 1]] <- data
        } else {
          missing_required_columns[[names[i]]] <- data  # Store with dataset name as key
        }
      }

      # Combine datasets with all required columns into one dataframe
      combined_data <- do.call(omopgenerics::bind, complete_datasets) #why is this changin the result id????
      # Return both results as a list
      return(list(complete = combined_data, incomplete = missing_required_columns))

    } else {
      # Return NULL or an empty list if datasets are not defined or empty
      return(NULL)
    }
  })


  shiny::observe({
    # # Dynamic sidebar
    output$dynamic_sidebar <- shinydashboard::renderMenu({
      sidebarMenuDynamic(process_data()$complete, process_data()$incomplete)
    })

    # Dynamic tabs
    output$dynamic_tabs_output <- shiny::renderUI({
      static_tabs <- list(
        shinydashboard::tabItem(
          "about",
          aboutTab() |>
            rlang::parse_expr() |>
            rlang::eval_tidy()
        ),
        shinydashboard::tabItem("UploadData", UploadDataTab()),
        shinydashboard::tabItem("LoadData", LoadDataTab()),
        shinydashboard::tabItem("contact", contactTab())
      )

      # print(process_data()$complete)
      all_tabs <-createDynamicTabs(process_data()$complete, process_data()$incomplete, plot_config, session)

      # Combine static and dynamic tabs
      do.call(shinydashboard::tabItems, c(static_tabs, all_tabs))
    })




  })
}
