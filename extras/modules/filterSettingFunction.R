
import("shiny")
import("plotly")
import("modules")
import("shinydashboard")
import("ggplot2")
import("DT")
import("utils")
import("dplyr")

# Helper function to apply the filters to the data frame
# Helper function to apply the filters to the data frame
apply_filters <- function(df, input, ns, cols_to_filter) {
  for (col in cols_to_filter) {
    filter_values <- input[[paste0(col, "_filter")]]
    if (!is.null(filter_values) && length(filter_values) > 0) {
      df <- df |> filter((!!sym(col)) %in% filter_values)
    }
  }
  df
}
# Server logic
init_server <- function(id, dataset, global_store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Identify columns that have multiple unique values and exclude 'result_id'
    multi_value_cols <- reactive({
      df <- dataset()
      # Calculate which columns have more than one unique value
      valid_cols <- names(df)[sapply(df, function(x) length(unique(x)) > 1)]
      valid_cols[!(valid_cols %in% c("result_id", "cohort_definition_id"))]
    })
    
    # Render dynamic filter UI based on the dataset columns with more than one unique value
    output$filter_ui <- renderUI({
      df <- dataset()  # Load data to generate filters
      fluidRow(
        lapply(multi_value_cols(), function(col) {
          selectInput(
            inputId = ns(paste0(col, "_filter")),
            label = paste("Filter by", col),
            choices = c("", unique(df[[col]])),  # Include an option to select no filter
            selected = unique(df[[col]])[1],
            multiple = TRUE
          )
        })
      )
    })
    
    # Reactive data based on input filters
    reactive_data <- reactive({
      df <- dataset()
      # Apply filters dynamically based on input selections
      apply_filters(df, input, ns, multi_value_cols())
    })
    
    # Store unique result_ids for later use
    unique_result_ids <- reactive({
      unique(reactive_data()$result_id)
    })
    
    # Save unique_result_ids in the global store
    observe({
      global_store(unique_result_ids())
    })
    
    # Display DataTable with all or filtered data
    output$statetable_setting <- renderDT({
      datatable(reactive_data(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      ))
    })
  })
}
# Table UI Function
ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns("filter_ui")),
    DTOutput(ns("statetable_setting"))
  )
}
