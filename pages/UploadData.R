# UploadDataTab containing tools to upload data

# UploadDataTab <- tags$div(
#   class = "main-content-grid simple-grid",
#   #global_metrics_view$ui("global_metrics_simple_view"),
#   div(
#     class = "table-grid-wrapper",
#     study_select$ui("available_studies_upload")
#    ) 
#   ,
#   div(
#     class = "barchart-grid-wrapper",
#     box(title =   "In construction" ,
#     status = "primary",
#     collapsible = FALSE,
#     solidHeader = FALSE,
#     width = 12,
#     span( icon("person-digging", , style = "font-size: 200px;"))
#     
#   )
# )
# )

UploadDataTab <- tags$div(
  class = "load-data-grid",
  div(
    class = "data-load-wrapper",
    box(
      title = "Upload Data",
      status = "primary",
      collapsible = FALSE,
      solidHeader = FALSE,
      width = 12,
      uploadData$ui("uploadData")  # Include uploadData UI here
    )
  )
)

# UploadDataTab <- tags$div(
#   class = "load-data-grid",
#   div(
#     class = "study-select-wrapper",
#     study_select$ui("available_studies_upload")
#   ),
#   div(
#     class = "data-load-wrapper",
#     box(title =   "In construction" ,
#         status = "primary",
#         collapsible = FALSE,
#         solidHeader = FALSE,
#         width = 12,
#         span( icon("person-digging", , style = "font-size: 200px;"))
#         
#     )
#     )
# )