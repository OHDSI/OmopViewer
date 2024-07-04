# LoadDataTab containing tools to select data to view
LoadDataTab <- function() {
  shiny::tags$div(
    class = "load-data-grid",
    shiny::div(
      class = "study-select-wrapper",
      studySelect_ui("available_studies")
    ),
    shiny::div(
      class = "data-load-wrapper",
      dataLoad_ui("dataLoad")
    )
  )
}
