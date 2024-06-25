import("shiny")
import("plotly")
import("modules")
import("shinydashboard")
import("ggplot2")
import("DT")
import("utils")
import("dplyr")
import("shinyWidgets")
import("shinycssloaders")
import("CohortCharacteristics")
import("omopgenerics")
export("ui")
export("init_server")

CONSTS <- use("constants/constants.R")


# Module UI Function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      pickerInput(
        ns("sc_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("sc_plot_color"),
        label = "Color by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("sc_plot_style"),
        label = "Plot Style",
        choices = c("boxplot", "barplot"),
        selected = "barplot",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      pickerInput(
        ns("sc_plot_xaxis"),
        label = "X Axis",
        choices = c("strata_level", "strata_name", "cdm_name", "variable_name",
                    "variable_level", "estimate_type", "group_name",
                    "group_level"),
        selected = "variable_name",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      pickerInput(
        ns("sc_plot_variable"),
        label = "variable_name",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      )
    ),
    downloadButton(ns("sc_plot_download_png"), "Download PNG"),
    plotlyOutput(ns("sc_plot")) %>% withSpinner()
  )
}

# Module Server Function
init_server <- function(id, dataset, filter_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    transformed_data <- reactive({
      df <- dataset()  # Use the reactive dataset
      df %>%
        omopgenerics::newSummarisedResult() %>%
        visOmopResults::addSettings()
    })
    
    # Reactive for handling filtered data
    filtered_data <- reactive({
      df <- transformed_data()  # Start with the transformed dataset
      flt <- filter_input()  # Get the current filters
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }
      df
    })
    
    observeEvent(filtered_data(), {
      req(filtered_data())
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      print(valid_cols)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "result_id",
                                          "variable_level", "additional_level"))]
      #### to be checked, shd not allow by these???
      updatePickerInput(session, inputId = "sc_plot_facet", choices = choices,
                        selected = choices[1])
      updatePickerInput(session, inputId = "sc_plot_color", choices = choices,
                        selected = choices[2])
      # Exclude specific unwanted values
      excluded_values <- c("Age group", "Cohort start date", "Cohort end date") #
      filtered_values <- unique(filtered_data()$variable_name)[!unique(filtered_data()$variable_name) %in% excluded_values]
      
      updatePickerInput(session, inputId = "sc_plot_variable", choices = filtered_values)
    })
    
    prepared_plot_data <- reactive({
      # Get result IDs from filtered data to include only relevant settings
      result_ids <- filtered_data() %>%
        filter(variable_name == input$sc_plot_variable) %>%
        pull("result_id") %>% unique()
      
      # Get settings for included result_ids
      inc_setting <- dataset() %>% filter(result_id %in% result_ids, variable_name == "settings")
      
      # Combine settings with filtered data and reapply summarization
      rbind(inc_setting, filtered_data() %>%
              filter(variable_name == input$sc_plot_variable) %>%
              omopgenerics::newSummarisedResult()) %>%
        omopgenerics::newSummarisedResult()
    })
    
    output$sc_plot <- renderPlotly({
      # Check if the input style is 'barplot' and handle accordingly
      # inc <- filtered_data() %>%
      #   filter(variable_name == input$sc_plot_variable)
      #
      # result_ids <- filtered_data() %>% pull("result_id") %>% unique()
      #
      # inc_setting <- dataset() %>% filter(result_id %in% result_ids) %>% filter(variable_name == "settings")
      # inc_tbl <- rbind(inc_setting, filtered_data()%>% omopgenerics::newSummarisedResult())
      p <- plotCharacteristics(data = prepared_plot_data(),
                               facet = input$sc_plot_facet,
                               colour = input$sc_plot_color,
                               plotStyle = input$sc_plot_style,
                               x = input$sc_plot_xaxis)
      
      # Check if the input style is 'boxplot' and handle accordingly
      p
    })
    
    output$sc_plot_download_png <- downloadHandler(
      filename = function() { paste0("summarised_characteristics_", Sys.Date(), ".png") },
      content = function(file) {
        inc <- filtered_data() %>%
          filter(variable_name == input$sc_plot_variable) %>%
          mutate(estimate_type = if_else(estimate_type == "integer", "numeric", estimate_type)) %>%
          newSummarisedResult()
        p <- plotCharacteristics(data = inc,
                                 facet = input$sc_plot_facet,
                                 colour = input$sc_plot_color,
                                 plotStyle = input$sc_plot_style,
                                 x = input$sc_plot_xaxis)
        ggsave(filename = file, plot = p)
      }
    )
    
  })
}
