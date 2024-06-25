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
        ns("co_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("co_unique_comb"),
        label = "Unique Combination",
        choices = c(TRUE, FALSE),
        selected = TRUE,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
    ),
    downloadButton(ns("co_plot_download_png"), "Download PNG"),
    plotlyOutput(ns("co_plot")) %>% withSpinner()
  )
}

# Module Server Function
init_server <- function(id, dataset, filter_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    transformed_data <- reactive({
      df <- dataset() # Use the reactive dataset
      df %>%
        omopgenerics::newSummarisedResult() %>%
        visOmopResults::addSettings()
    })
    
    # Reactive for handling filtered data
    filtered_data <- reactive({
      df <- transformed_data() # Start with the transformed dataset
      flt <- filter_input() # Get the current filters
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }
      df
    })
    
    
    observe({
      req(filtered_data())
      # Calculate columns with more than one unique value
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      print(valid_cols)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "estimate_name", 
                                          "variable_level", "result_id"))]
      #### to be checked, shd not allow by these???
      updatePickerInput(session, inputId = "co_plot_facet", choices = choices,
                        selected = choices[1])
    })
    
    prepared_plot_data <- reactive({
      # Get result IDs from filtered data to include only relevant settings
      result_ids <- filtered_data() %>%
        # filter(variable_name == input$sc_plot_variable) %>%
        pull("result_id") %>% unique()
      
      # Get settings for included result_ids
      inc_setting <- dataset() %>% filter(result_id %in% result_ids, variable_name == "settings")
      write.csv(inc_setting,"test_setting.csv")
      
      # Combine settings with filtered data and reapply summarization
      rbind(inc_setting, filtered_data() %>%
              # filter(variable_name == input$sc_plot_variable) %>%
              omopgenerics::newSummarisedResult()) %>%
        mutate(estimate_type = if_else(estimate_type == "integer", "numeric", estimate_type)) %>%
        omopgenerics::newSummarisedResult()
    })
    
    
    output$co_plot <- renderPlotly({
      
      write.csv(prepared_plot_data(),"test.csv")
      p <- plotCohortOverlap(
        result = prepared_plot_data(),
        facet = input$co_plot_facet,
        uniqueCombinations = as.logical(input$co_unique_comb)
      )
      
      # Check if the input style is 'boxplot' and handle accordingly
      p
    })
    
    output$co_plot_download_png <- downloadHandler(
      filename = function() {
        paste0("cohort_overlap_", Sys.Date(), ".png")
      },
      content = function(file) {
        inc <- filtered_data() %>%
          mutate(estimate_type = if_else(estimate_type == "integer", "numeric", estimate_type)) %>%
          newSummarisedResult()
        p <- plotCohortTiming(
          data = prepared_plot_data(),
          facet = input$co_plot_facet,
          uniqueCombinations = as.logical(input$co_unique_comb)
        )
        ggsave(filename = file, plot = p)
      }
    )
  })
}
