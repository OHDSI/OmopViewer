#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
dataLoad_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    title = "Data selected: ",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 9,
    shiny::textOutput(ns("selected_study")),
    shiny::actionButton(ns("load_data_button"), "Load Data"),
    shiny::textOutput(ns("selected_study2"))

  )


}
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
dataLoad_myModuleServer <- function(id, dataFolder) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      loaded_data_value <- shiny::reactiveValues()


      output$selected_study <- shiny::renderPrint({
        studySelected_data <- session$userData$studySelect$studySelected()
        studySelected = studySelected_data[,1]
        file_selected = studySelected_data[,4]

        cat(  "You have selected this file: " )

        cat(  studySelected )
      })

      shiny::observeEvent(input$load_data_button, {
        studySelected_data <- session$userData$studySelect$studySelected()
        studySelected = studySelected_data[,1]
        file_selected = studySelected_data[,4]

        if ( length(studySelected_data[,4])==0 ) {
          output$selected_study2 <- shiny::renderPrint({  cat(  "no study selected!")  })

        } else{

          loaded_data_value$data <- readRDS(file.path(here::here(dataFolder), file_selected))
          output$selected_study2 <- shiny::renderPrint({  cat(  paste0(file_selected, " loaded!")) })
        }

      })

      return(
        list(
          loaded_data_value = shiny::reactive({loaded_data_value$data})
        ))






    }
  )
}

