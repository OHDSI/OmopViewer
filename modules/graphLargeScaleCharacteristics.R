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
# future::plan(future::multisession)


# Module UI Function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "display: inline-block; vertical-align: top; width: 150px;",
      pickerInput(
        ns("lsc_plot_facet"),
        label = "Facet by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("lsc_plot_color"),
        label = "Color by",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("lsc_plot_style"),
        label = "Position",
        choices = c("horizontal", "vertical"),
        selected = "horizontal",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      pickerInput(
        ns("lsc_plot_strata"),
        label = "Split strata",
        choices = c(TRUE, FALSE),
        selected = TRUE,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      pickerInput(
        ns("lsc_plot_table_filter"),
        label = "Table name",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = FALSE
      ),
      pickerInput(
        ns("lsc_plot_var_level"),
        label = "Variable level",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      ),
      pickerInput(
        ns("lsc_plot_variable"),
        label = "variable_name",
        choices = NULL,
        selected = NULL,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
      # pickerInput(
      #   ns("lsc_plot_add_level"),
      #   label = "Additional level",
      #   choices = NULL,
      #   selected = NULL,
      #   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #   multiple = TRUE
      # ),
      # pickerInput(
      #   ns("lsc_plot_type"),
      #   label = "Type",
      #   choices = NULL,
      #   selected = NULL,
      #   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #   multiple = TRUE
      # )
    ),
    downloadButton(ns("lsc_plot_download_png"), "Download PNG"),
    plotlyOutput(ns("lsc_plot"), width = "100%", height = "800px") %>% withSpinner()
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
    
    
    observe({
      req(filtered_data())
      # Calculate columns with more than one unique value
      valid_cols <- sapply(filtered_data(), function(x) length(unique(x)) > 1)
      print(valid_cols)
      choices <- names(valid_cols)[valid_cols]
      # Update picker inputs
      choices <- choices[!(choices %in% c("estimate_value", "estimate_name",
                                          "variable_level", "result_id",
                                          "variable_name", "group_name",
                                          "estimate_type", "additional_level"))]
      #### to be checked, shd not allow by these???
      updatePickerInput(session, inputId = "lsc_plot_facet", choices = choices,
                        selected = choices[2])
      
      updatePickerInput(session, inputId = "lsc_plot_color", choices = choices,
                        selected = choices[1])
      
      
      df_1 <- filtered_data() %>% filter(!is.na(estimate_value))  # Use the reactive dataset
      # filtered_values <- df_1 %>% pull(
      #   "table_name"
      # ) %>% unique()
      # updatePickerInput(session, inputId = "lsc_plot_table_filter", choices = filtered_values,
      #                   selected = filtered_values[1])
      # 
      
      filtered_values_1 <- df_1 %>% pull(
        "variable_level"
      ) %>% unique()
      updatePickerInput(session, inputId = "lsc_plot_var_level", choices = filtered_values_1,
                        selected = filtered_values_1[1])
      
      
      filtered_values_2 <- df_1 %>% pull(
        "additional_level"
      ) %>% unique()
      updatePickerInput(session, inputId = "lsc_plot_add_level", choices = filtered_values_2,
                        selected = filtered_values_2[1])
      
      #
      #       filtered_values_3 <- df_1 %>% pull(
      #         "type"
      #       ) %>% unique()
      #       print(filtered_values_3)
      #       updatePickerInput(session, inputId = "lsc_plot_type", choices = filtered_values_3,
      #                         selected = filtered_values_3[1])
      
      filtered_values_4 <- unique(df_1$variable_name)
      
      updatePickerInput(session, inputId = "lsc_plot_variable", choices = filtered_values_4,
                        selected = filtered_values_4[1])
      
    })
    
    
    prepared_plot_data <- reactive({
      
      current_filtered_data <- filtered_data() %>% filter(!is.na(estimate_value))
      
      # Find relevant result IDs based on current table input
      result_ids <- current_filtered_data %>%
        filter(variable_name %in% input$lsc_plot_variable) %>%
        # filter(table_name %in% input$lsc_plot_table_filter) %>%
        filter(variable_level %in% input$lsc_plot_var_level
               # additional_level %in% input$lsc_plot_add_level,type %in% input$lsc_plot_type
        ) %>%
        pull("result_id")
      
      # Fetch settings only once based on the relevant result IDs
      inc_setting <- dataset() %>%
        filter(result_id %in% result_ids, variable_name == "settings")
      
      # Combine and re-summarize the data only once
      combined_data <- rbind(
        inc_setting,
        current_filtered_data %>%
          # filter(table_name == input$lsc_plot_table_filter)%>%
          filter(variable_name %in% input$lsc_plot_variable) %>%
          filter(variable_level %in% input$lsc_plot_var_level
                 # additional_level %in% input$lsc_plot_add_level, type %in% input$lsc_plot_type
          ) %>%
          filter(!is.na(estimate_value)) %>%
          omopgenerics::newSummarisedResult()
      )
      
      omopgenerics::newSummarisedResult(combined_data)
      
    })
    
    
    
    
    output$lsc_plot <- renderPlotly({
      
      p <- plotLargeScaleCharacteristics(
        data = prepared_plot_data(),
        facet = input$lsc_plot_facet,
        colorVars = input$lsc_plot_color,
        position = input$lsc_plot_style,
        splitStrata = as.logical(input$lsc_plot_strata)
      )
      p <- p +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 5)  # Adjusting facet title font size
          
        )
      
      ggplotly(p)
    })
    
    output$lsc_plot_download_png <- downloadHandler(
      filename = function() {
        paste0("summarised_large_scale_characteristics_", Sys.Date(), ".png")
      },
      content = function(file) {
        inc <- filtered_data() %>%
          newSummarisedResult()
        p <- plotLargeScaleCharacteristics(
          data = inc,
          facet = input$lsc_plot_facet,
          colorVars = input$lsc_plot_color,
          position = input$lsc_plot_style,
          splitStrata = as.logical(input$lsc_plot_strata)
        )
        ggsave(filename = file, plot = p)
      }
    )
  })
}
