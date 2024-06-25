import("shiny")
import("DT")
import("plotly")
import("modules")
import("shinydashboard")
import("ggplot2")
import("dplyr")
import("utils")
import("htmlwidgets")
import("here")
export("ui")
export("myModuleServer")


CONSTS <- use("constants/constants.R")

ui <- function(id) {
  ns <- NS(id)
  
  box(
    title = "Data selected: ",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 9,
    textOutput(ns("selected_study")),
    actionButton(ns("load_data_button"), "Load Data"),
    textOutput(ns("selected_study2"))
    
  )
  
  
}

myModuleServer <- function(id, dataFolder) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      loaded_data_value <- reactiveValues()
      
      
      #$available_studies$studySelected
      #cat(file=stderr(), "drawing histogram with",paste0(studySelected_data),  "\n")
      
      output$selected_study <- renderPrint({
        studySelected_data <- session$userData$studySelect$studySelected()
        studySelected = studySelected_data[,1] 
        file_selected = studySelected_data[,4] 
        
        cat(  "You have selected this file: " )
        #print(  length(studySelected_data[,6])==0  )
        #print(  " " )
        cat(  studySelected )
        #print(  " " )
        #cat(  file_selected )
      })
      
      observeEvent(input$load_data_button, {
        studySelected_data <- session$userData$studySelect$studySelected()
        studySelected = studySelected_data[,1] 
        file_selected = studySelected_data[,4] 
        
        if ( length(studySelected_data[,4])==0 ) {
          output$selected_study2 <- renderPrint({  cat(  " no study selected!")  })
          
        } else{
          
          loaded_data_value$data <- readRDS(file.path(here(dataFolder), file_selected))
          output$selected_study2 <- renderPrint({  cat(  paste0(file_selected, " loaded!")) })
        }
        
      })
      
      return(
        list(
          loaded_data_value = reactive({loaded_data_value$data})
        ))
      
      
      
      
      
      
    }
  )
}

