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

ui <- function(id) {
  ns <- NS(id)
  
  box(
    title = "Data Already Loaded",
    status = "primary",
    collapsible = TRUE,
    solidHeader = FALSE,
    width = 12,
    DTOutput(ns("available_studies"))
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns
  
  studies <- reactive({
    readRDS("data/studies.rds")
  })
  
  # DataTable object
  output$available_studies <- DT::renderDT({
    DT::datatable(
      studies() |> select(study_name,uploaded_by, date_uploaded),
      rownames = FALSE,
      style = "bootstrap",
      selection = 'single',
      caption = "Select a study to load into the app",
      #filter = c("top")
      # ,
      options = list(
        dom = 'ftp',
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 5,
        ordering = TRUE #,
        #    stateSave = TRUE ,
        #columnDefs = list(list(visible = FALSE, targets = c(1)))
      )
    ) |>  formatDate(columns = 3, method =  "toLocaleDateString", 
                      params = 'en-GB')
  })
  
  # DataTable proxy object
  DTproxy <- DT::dataTableProxy("studytable")
  
  
  return(list(studySelected = reactive({
    req(input$available_studies_rows_selected)
    studies()[input$available_studies_rows_selected, ]
  })))  
  
}
