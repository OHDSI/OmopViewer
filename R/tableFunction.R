# Table UI Function
table_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "table",
    DT::DTOutput(ns("statetable"))
  )
}

# Table Server Function
table_init_server <- function(id, dataset, filter_input) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filtered_data <- shiny::reactive({
      df <- dataset() # Use the reactive dataset
      flt <- filter_input()
      if (length(flt) > 0) {
        for (filter in flt) {
          if (!is.null(filter$values) && length(filter$values) > 0) {
            df <- df[df[[filter$column]] %in% filter$values, ]
          }
        }
      }

      df
    })

    non_unique_columns <- shiny::reactive({
      df <- filtered_data() %>% omopgenerics::newSummarisedResult()
      non_unique_cols <- names(df)[sapply(df, function(x) length(unique(x)) > 1)]
      # no need to be non unique value for estimate_value column
      if ("estimate_value" %in% names(df)) {
        non_unique_cols <- union(non_unique_cols, "estimate_value")
      }
      non_unique_cols
    })

    # Render table using only non-unique columns
    output$statetable <- DT::renderDT({
      df <- filtered_data() %>% omopgenerics::newSummarisedResult() # Get the full filtered dataset
      df_display <- df[, non_unique_columns(), drop = FALSE] # Only display non-unique columns
      DT::datatable(
        df_display,
        extensions = "Buttons", # Enable buttons extension for DataTables
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          search = list(regex = TRUE, caseInsensitive = TRUE),
          pageLength = 10,
          pagingType = "full_numbers",
          ordering = TRUE,
          stateSave = TRUE
        )
      )
    })

    return(list(state_selected = shiny::reactive({
      state_data <- filtered_data() # Get the current state of filtered data
      selected_data <- state_data[input$statetable_rows_selected, ] # Get selected rows
      # print("Selected data rows:")
      # print(selected_data)
      selected_data
    })))
  })
}
