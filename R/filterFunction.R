
# Filter Module UI
filter_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    class = "filter-row",  # a class for styling
    column(3, uiOutput(ns("filter_col_ui"))),
    column(9, uiOutput(ns("value_filters")))
  )
}
# Filter Module Server
filter_module_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define reactive for character columns based on the chosen dataset
    character_columns <- shiny::reactive({
      df <- dataset()  # Use the reactive dataset
      if (is.null(df) || ncol(df) == 0 ) {
        print("Data is NULL or has no columns")
        return(character(0))
      }
      # Find character columns
      char_cols <- names(df)[sapply(df, is.character)]
      if (length(char_cols) == 0) {
        print("No column with multiple values to filter")
        return(character(0))  # Return empty if no character columns found
      }
      # Filter columns where the number of unique values is greater than 1
      char_uniq_cols <- char_cols[sapply(df[char_cols], function(x) {length(unique(x)) > 1})]
      # Exclude the "markdown" and "estimate_value" column
      char_uniq_cols <- char_uniq_cols[!(char_uniq_cols %in% c("markdown", "estimate_value"))]
      char_uniq_cols
    })

    # UI for selecting filter column
    output$filter_col_ui <- shiny::renderUI({
      char_cols <- character_columns()
      shiny::selectInput(ns("filter_col"), "Select column to filter:", choices = char_cols, multiple = TRUE)
    })

    # UI for selecting values based on the column selected
    output$value_filters <- shiny::renderUI({
      shiny::req(input$filter_col)
      lapply(input$filter_col, function(col) {
        data <- dataset()  # Use the reactive data source
        shiny::selectInput(ns(paste0("filter_values_", col)), sprintf("Values for %s:", col),
                    choices = unique(data[[col]]), selected = NULL, multiple = TRUE)
      })
    })

    # Reactive to collect filter settings
    filters <- shiny::reactive({
      lapply(input$filter_col, function(col) {
        values = input[[paste0("filter_values_", col)]]
        if (is.null(values) || length(values) == 0) {
          print(names(input))
          print(col)
          if ("filter_values_cohort_name" %in% names(input)) {
            print(input$filter_values_cohort_name)
          }
          print(paste("No values selected for column", col))
        } else {
          print(paste("Values selected for", col, ":", paste(values, collapse = ", ")))
        }
        list(column = col, values = values)
      })
    })

    return(filters)
  })
}
