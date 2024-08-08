import("shiny")
import("plotly")
import("modules")
import("shinydashboard")
import("ggplot2")
import("DT")
import("utils")
import("dplyr")

export("ui")
export("init_server")

CONSTS <- use("constants/constants.R")


# Table UI Function
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "table",
    DTOutput(ns("statetable"))
  )
}

# Table Server Function
init_server <- function(id, dataset, filter_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filtered_data <- reactive({
      df <- dataset()  # Use the reactive dataset
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

    non_unique_columns <- reactive({
      df <- filtered_data() |> omopgenerics::newSummarisedResult()
      non_unique_cols <- names(df)[sapply(df, function(x) length(unique(x)) > 1)]
      non_unique_cols
    })

    # Render the DataTable using only non-unique columns
    output$statetable <- renderDT({
      df <- filtered_data() |> omopgenerics::newSummarisedResult()  # Get the full filtered dataset
      df_display <- df[, non_unique_columns(), drop = FALSE]  # Only display non-unique columns
      datatable(
        df_display,
        extensions = 'Buttons',  # Enable buttons extension for DataTables
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          search = list(regex = TRUE, caseInsensitive = TRUE),
          pageLength = 10,
          pagingType = "full_numbers",
          ordering = TRUE,
          stateSave = TRUE
        )
      )
    })
    # # Download Handler for CSV
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste("data-", Sys.Date(), ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(filtered_data(), file, row.names = FALSE)
    #   }
    # )
    #
    # Return selected row data if needed outside this module

    return(list(state_selected = reactive({
      state_data <- filtered_data()  # Get the current state of filtered data
      selected_data <- state_data[input$statetable_rows_selected, ]  # Get selected rows
      print("Selected data rows:")
      print(selected_data)
      selected_data
    })))
  })
}
