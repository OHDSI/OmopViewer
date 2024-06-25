# LoadDataTab containing tools to select data to view
# 
# LoadDataTab <- tags$div(
#   class = "main-content-grid simple-grid",
#   #global_metrics_view$ui("global_metrics_simple_view"),
#   div(
#     class = "table-grid-wrapper",
#     studySelect$ui("available_studies")
#    ) ,
#    div(
#      class = "barchart-grid-wrapper",
#      dataLoad$ui("dataLoad")
#    )
# )
# 

LoadDataTab <- tags$div(
  class = "load-data-grid",
  div(
    class = "study-select-wrapper",
    studySelect$ui("available_studies")
  ),
  div(
    class = "data-load-wrapper",
    dataLoad$ui("dataLoad")
  )
)
