#' Generic server function that uses the plot_config to handle plotting and UI updates
#' @param id A string that uniquely identifies the module instance.
#' @param moduleType A string specifying the type of module, which corresponds to keys
#'        in the `plot_config` that define how plots and UI elements are handled.
#' @param dataset A reactive expression or function that returns the dataset to be used.
#'        This dataset is expected to be processed dynamically based on user input.
#' @param filter_input A reactive expression or function that returns user-selected
#'        filters to apply to the dataset.
#'
#' @details
#' The `genericServer` function facilitates the creation of modular plot components in
#' a Shiny application, allowing for scalable and maintainable code. It processes the
#' data using predefined settings, applies user filters, updates the UI dynamically
#' based on available data attributes, and manages the rendering of plots and downloading
#' of plot images.
#'
#' The function is highly dependent on the `plot_config` configuration list, which must be
#' predefined and should include details such as the plotting function to use ,
#' UI elements to dynamically generate, and the specific IDs for updating
#' UI components (`updatePickerInputIDs`).
#' @return A Shiny module server function, which can be use in omopViewer
#' to add the functionality of specified module types to the shiny
#' @export
#'
genericServer <- function(id, moduleType, dataset, filter_input) {
  shiny::moduleServer(id, function(input, output, session) {


    ns <- shiny::NS(id)

    # Process and filter data
    processed_data <- shiny::reactive({
      dataset() |> omopgenerics::newSummarisedResult() |> visOmopResults::addSettings()
    })

    filtered_data <- shiny::reactive({
      df <- processed_data()
      flt <- filter_input()
      if (!is.null(flt)) {
        for (filter in flt) {
          df <- df[df[[filter$column]] %in% filter$values, ]
        }
      }
      df
    })

    shiny::observe({
      shiny::req(filtered_data())
      for (key in names(plot_config[[moduleType]]$updatePickerInputIDs)) {
        id <- plot_config[[moduleType]]$updatePickerInputIDs[[key]]
        if (key %in% c("facet", "colour", "colorVars")) {
          valid_cols <- sapply(filtered_data(), function(x) length(unique(stats::na.omit(x))) > 1)
          choices <- names(valid_cols)
          #please review these
          choices <- choices[!(choices %in% c("estimate_value", "estimate_name",
                                              "variable_level", "result_id",
                                              "cohort_definition_id", "cohort_name"))]
          shinyWidgets::updatePickerInput(session, inputId = id,
                                          choices = choices,
                                          selected = choices[1])
        }  else if (key == "variable_name") {
          excluded_values <- c("Age group", "Cohort start date", "Cohort end date")
          filtered_values <- unique(filtered_data()$variable_name)[!unique(filtered_data()$variable_name) %in% excluded_values]
          shinyWidgets::updatePickerInput(session, inputId = id,
                                          choices = filtered_values,
                                          selected = filtered_values[1])
        } else if (key == "variable_level") {
          df_1 <- filtered_data() |>
            dplyr::filter(!is.na(.data$estimate_value))  # Use the reactive dataset

          filtered_values_1 <- df_1 |>
            dplyr::pull(
              "variable_level"
            ) |> unique()
          shinyWidgets::updatePickerInput(session, inputId = id,
                                          choices = filtered_values_1,
                                          selected = filtered_values_1[1])
        } else {
          filtered_values <- unique(filtered_data()[[key]])
          shinyWidgets::updatePickerInput(session, inputId = id,
                                          choices = filtered_values,
                                          selected = filtered_values[1])
        }
      }
    })


    # Prepare data specifically for the plot, based on user interaction with UI elements
    prepared_plot_data <- shiny::reactive({

      relevant_filters <- plot_config[[moduleType]]$updatePickerInputIDs
      filter_columns <- setdiff(names(relevant_filters), c("facet", "colour",
                                                           "plotStyle", "x",
                                                           "plotType",
                                                           "colorVars"))
      filtered_df <- filtered_data()

      for (filter in filter_columns) {
        input_id <- relevant_filters[[filter]]
        filtered_df <- filtered_df[filtered_df[[filter]] %in% input[[input_id]], ]
      }
      result_ids <- filtered_df |>
        dplyr::pull("result_id") |>
        unique()

      # Use the filtered result IDs to fetch and summarize the data again
      if(any(filter_columns %in% c("variable_name", "variable_level"))){
        return(filtered_df |>
                 omopgenerics::newSummarisedResult())
      }
      else{
      return(dataset() |>
        # omopgenerics::newSummarisedResult() |>
        visOmopResults::filterSettings(.data$result_id %in% result_ids))
        }

    })



    # Handle plot outputs
    output[[paste0(moduleType, "_plot")]] <- plotly::renderPlotly({
      plot_args <- list()
      prepared_data <- prepared_plot_data() |>
        dplyr::filter(!is.na(.data$estimate_value)) |>
        dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_type == "integer",
                                                     "numeric", .data$estimate_type)) |>
        omopgenerics::newSummarisedResult()
      plot_args[[plot_config[[moduleType]]$dataArg]] <- prepared_data


      # Fetch input values using namespaced IDs
      for (param in names(plot_config[[moduleType]]$plotParams)) {
        input_id <- plot_config[[moduleType]]$plotParams[[param]]$id
        if (param == "uniqueCombinations") {
          plot_args[[param]] <- as.logical(input[[input_id]])
        } else {
          plot_args[[param]] <- input[[input_id]]
        }
        }

      do.call(plot_config[[moduleType]]$plotFunc, plot_args)
    })


    # Download handling
    output[[paste0(moduleType, "_download_png")]] <- shiny::downloadHandler(
      filename = function() { paste0(moduleType, "_", Sys.Date(), ".png") },
      content = function(file) {
        plot <- do.call(plot_config[[moduleType]]$plotFunc, c(list(data = filtered_data()), plot_args))
        ggplot2::ggsave(file, plot)
      }
    )
  })
}

