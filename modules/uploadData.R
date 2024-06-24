# Ensure all necessary libraries are imported
import("shiny")
import("plotly")
import("modules")
import("shinydashboard")
import("DT")
import("utils")
import("dplyr")
import("here")


initialize_studies_rds <- function(dataFolder) {
  studies_path <- file.path(here(dataFolder), "studies.rds")
  if (!file.exists(studies_path)) {
    initial_data <- data.frame(
      study_name = character(),
      uploaded_by = character(),
      date_uploaded = character(),
      file = character(),
      stringsAsFactors = FALSE
    )
    saveRDS(initial_data, studies_path)
    print("Initialized studies.rds.")
  }
}

process_zip_file <- function(zip_path, zip_name, dataFolder, existing_studies) {
  print(paste("Processing file:", zip_path))
  
  unzip_dir <- file.path(dataFolder, "unzipped_files", tools::file_path_sans_ext(zip_name))
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(zip_path, exdir = unzip_dir)
  print(paste("Unzipped the file to", unzip_dir))
  
  study_name <- tools::file_path_sans_ext(zip_name)
  study_metadata <- data.frame(
    study_name = study_name,
    uploaded_by = "Web User",
    date_uploaded = Sys.time(),
    file = paste0(study_name, ".rds"),
    stringsAsFactors = FALSE
  )
  
  # Filter out directories and ensure only processing files
  unzip_files <- list.files(unzip_dir, full.names = TRUE)
  for (file in unzip_files) {
    print(file)
  }
  unzip_files <- Filter(function(f) { file.info(f)$isdir == FALSE && basename(f) != "__MACOSX"}, unzip_files)
  print(paste("Found", length(unzip_files), "files in the ZIP."))

    
  data_list <- list()
  for (file in unzip_files) {
    print(grepl("\\.csv$", file))
    if (grepl("\\.csv$", file)) {  # Ensure only CSV files are read
      csv_name <- tools::file_path_sans_ext(basename(file))
      data_list[[csv_name]] <- read.csv(file, stringsAsFactors = FALSE)
    }
  }
  
  # Save the list of data frames to an RDS file
  rds_path <- file.path(dataFolder, paste0(study_name, ".rds"))
  saveRDS(data_list, rds_path)
  
  # Update existing studies reactive value
  new_studies <- rbind(existing_studies(), study_metadata)
  existing_studies(new_studies)
  
  # Save the updated studies data frame to RDS
  saveRDS(new_studies, file.path(here(dataFolder), "studies.rds"))
  
  # Clean up the unzip directory
  unlink(unzip_dir, recursive = TRUE)
}

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("fileUpload"), "Choose ZIP File", accept = ".zip"),
    actionButton(ns("uploadButton"), "Upload and Process File", icon = icon("upload")),
    DTOutput(ns("available_studies")),
    textOutput(ns("uploadStatus"))
  )
}

init_server <- function(id, dataFolder) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure studies.rds is initialized
    initialize_studies_rds(dataFolder)
    
    # Load or initialize existing studies metadata
    existing_studies_path <- file.path(here(dataFolder), "studies.rds")
    existing_studies <- reactiveVal({
      readRDS(existing_studies_path)
    })
    
    # Process user uploaded files
    observeEvent(input$uploadButton, {
      req(input$fileUpload)
      
      file_path <- input$fileUpload$datapath
      file_name <- input$fileUpload$name
      print(paste("Received file:", file_path, "with name:", file_name))
      process_zip_file(file_path, file_name, dataFolder, existing_studies)
      
      # Update the status text upon file processing
      output$uploadStatus <- renderText({
        paste("File processed and data updated:", file_name)
      })
    })
    
    # Display the list of processed studies
    output$available_studies <- renderDT({
      datatable(existing_studies(), options = list(pageLength = 5, searching = FALSE))
    })
  })
}