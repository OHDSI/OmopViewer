#' UI function for the Study Selection Module
#'
#' This function creates the UI components for the study selection module.
#'
#' @param id A string. The namespace identifier for the module.
#' @return A UI definition to select uploaded study
#' @export
studySelectUi <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    title = "Data Already Loaded",
    status = "primary",
    collapsible = TRUE,
    solidHeader = FALSE,
    width = 12,
    DT::DTOutput(ns("available_studies"))
  )
}

#' Initialize the Study Selection Module Server
#'
#' This function initializes the server-side logic for the study selection module.
#'
#' @param id A string. The namespace identifier for the module.
#' @return Initialise the study select module
#' @export
studySelectInitServer <- function(id) {
  shiny::callModule(studySelectServer, id)
}
#' Server function for the Study Selection Module
#'
#' This function contains the server-side logic for the study selection module.
#'
#' @param input Shiny server input.
#' @param output Shiny server output.
#' @param session Shiny server session.
#'
#' @return A list with a reactive expression that returns the selected study.
#' @export
studySelectServer <- function(input, output, session) {
  ns <- session$ns

  studies <- shiny::reactive({
    readRDS(here::here("extras/data/studies.rds"))
  })

  # DataTable object
  output$available_studies <- DT::renderDT({
    DT::datatable(
      studies() |>
        dplyr::select("study_name", "uploaded_by", "date_uploaded"),
      rownames = FALSE,
      style = "bootstrap",
      selection = "single",
      caption = "Select a study to load into the app",
      # filter = c("top")
      # ,
      options = list(
        dom = "ftp",
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 5,
        ordering = TRUE # ,
        #    stateSave = TRUE ,
        # columnDefs = list(list(visible = FALSE, targets = c(1)))
      )
    ) |> DT::formatDate(
      columns = 3, method = "toLocaleDateString",
      params = "en-GB"
    )
  })

  # DataTable proxy object
  DTproxy <- DT::dataTableProxy("studytable")


  return(list(studySelected = shiny::reactive({
    req(input$available_studies_rows_selected)
    studies()[input$available_studies_rows_selected, ]
  })))
}
