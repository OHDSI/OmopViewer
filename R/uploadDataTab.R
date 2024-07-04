UploadDataTab <- function(){
  shiny::tags$div(
  class = "load-data-grid",
  shiny::div(
    class = "data-load-wrapper",
    shinydashboard::box(
      title = "Upload Data",
      status = "primary",
      collapsible = FALSE,
      solidHeader = FALSE,
      width = 12,
      uploadData_ui("uploadData")  # Include uploadData UI here
    )
  )
)}
