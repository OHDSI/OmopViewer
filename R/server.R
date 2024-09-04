
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

serverStatic <- function(result = emptySummarisedResult()) {
  # initial checks
  result <- omopgenerics::validateResultArguemnt(result)

  resultType <- omopgenerics::settings(result) |>
    dplyr::select("result_type") |>
    dplyr::distinct() |>
    dplyr::pull()

  serv <- purrr::map_chr(resultType, \(x) {
    c(glue::glue("# {x} -----"),
      glue::glue("## raw {x} -----"),
      getRawRt(x),
      glue::glue("## tidy {x} -----"),
      getTidyRt(x),
      glue::glue("## formatted {x} -----"),
      getFormattedRt(x),
      glue::glue("## plot {x} -----"),
      getPlotRt(x),
      "\n"
    ) |>
      paste0(collapse = "\n")
  }) |>
    paste0(collapse = "\n")

  serv <- paste0(
    c("server <- function(input, output, session) {", serv, "}"),
    collapse = "\n"
  ) |>
    styleCode()

  return(serv)
}

# get server for raw panel ----
getRawRt <- function(rt) {
  c('getRawData[formatCamel(rt)] <- shiny::reactive({
      filterData(data, "[rt]", input)
    })',
    'output$[rt]_raw <- DT::renderDT({
      DT::datatable(getRawData[formatCamel(rt)](), options = list(scrollX = TRUE))
    })',
    'output$[rt]_raw_download <- shiny::downloadHandler(
      filename = "raw_[rt].csv",
      content = function(file) {
        getRawData[formatCamel(rt)]() |>
          readr::write_csv(file = file)
          # TBR by exportSummarisedResult
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
# get server for tidy panel ----
getTidyRt <- function(rt) {
  c('getTidyData[formatCamel(rt)] <- shiny::reactive({
      data |>
        filterData("[rt]", input) |>
        tidyData(
          prefixSet = "set:",
          prefixGroup = "group: ",
          showSettings = input$[rt]_tidy_settings,
          showGroupping = input$[rt]_tidy_groupping,
          pivot = input$[rt]_tidy_pivot
        )
    })',
    'output$[rt]_tidy <- DT::renderDT({
      DT::datatable(getTidyData[formatCamel(rt)](), options = list(scrollX = TRUE))
    })',
    'output$[rt]_tidy_download <- shiny::downloadHandler(
      filename = "tidy_[rt].csv",
      content = function(file) {
        getTidyData[formatCamel(rt)]() |>
          readr::write_csv(file = file)
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
# get server for formatted panel ----
getFormattedRt <- function(rt) {
  c('getFormattedData[formatCamel(rt)] <- shiny::reactive({
      data |>
        filterData("[rt]", input) |>
        visTable(
          header = input$[rt]_formatted_header,
          group = input$[rt]_formatted_group,
          hide = input$[rt]_formatted_hide
        )
    })',
    'output$[rt]_formatted <- gt::render_gt({
      getFormattedData[formatCamel(rt)]()
    })',
    'output$[rt]_formatted_download <- shiny::downloadHandler(
      filename = "formatted_[rt].docx",
      content = function(file) {
        getFormattedData[formatCamel(rt)]() |>
          gt::gtsave(filename = file)
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
# get server for plot panel(s) ----
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
