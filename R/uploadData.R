initialize_studies_rds <- function(dataFolder) {
  studies_path <- file.path(here::here(dataFolder), "studies.rds")
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

  unzip_dir <- file.path(dataFolder, "unzipped_files",
                         tools::file_path_sans_ext(zip_name))
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  zip::unzip(zip_path, exdir = unzip_dir)
  print(paste("Unzipped the file to", unzip_dir))

  study_name <- tools::file_path_sans_ext(zip_name)
  study_metadata <- data.frame(
    study_name = study_name,
    uploaded_by = "Web User",
    date_uploaded = Sys.time(),
    file = paste0(study_name, ".rds"),
    stringsAsFactors = FALSE
  )

  # List all files recursively, include their paths
  unzip_files <- list.files(unzip_dir, full.names = TRUE, recursive = TRUE)
  print("Files and directories extracted:")
  print(unzip_files)
  # Filter out directories and specific Mac artifacts like __MACOSX or .DS_Store
  unzip_files <- Filter(function(f) {
    file.info(f)$isdir == FALSE && !grepl("__MACOSX|\\.DS_Store", f)
  }, unzip_files)
  # print("Filtered files to process:")
  # print(unzip_files)

  # # Filter out directories and ensure only processing files
  # unzip_files <- list.files(unzip_dir, full.names = TRUE)
  # for (file in unzip_files) {
  #   print(file)
  # }
  # unzip_files <- Filter(function(f) { file.info(f)$isdir == FALSE && !grepl("__MACOSX|\\.DS_Store$", basename(f)) }, unzip_files)
  # print(paste("Found", length(unzip_files), "files in the ZIP."))
  #

  data_list <- list()
  for (file in unzip_files) {
    print(grepl("\\.csv$", file))
    if (grepl("\\.csv$", file)) {  # Ensure only CSV files are read
      csv_name <- tools::file_path_sans_ext(basename(file))
      data_list[[csv_name]] <- utils::read.csv(file, stringsAsFactors = FALSE)
    }
  }

  # Save the list of data frames to an RDS file
  rds_path <- file.path(dataFolder, paste0(study_name, ".rds"))
  saveRDS(data_list, rds_path)

  # Update existing studies reactive value
  new_studies <- rbind(existing_studies(), study_metadata)
  existing_studies(new_studies)

  # Save the updated studies data frame to RDS
  saveRDS(new_studies, file.path(here::here(dataFolder), "studies.rds"))

  # Clean up the unzip directory
  unlink(unzip_dir, recursive = TRUE)
}


#' Upload Data UI Component
#'
#' This function creates a UI component for uploading data files.
#' @param id A unique identifier for the Shiny module
#' @return A UI definition for use in a Shiny application.
#' @export

uploadData_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fileInput(ns("fileUpload"), "Choose ZIP File", accept = ".zip"),
    shiny::actionButton(ns("uploadButton"), "Upload and Process File",
                        icon = shiny::icon("upload")),
    DT::DTOutput(ns("available_studies")),
    shiny::textOutput(ns("uploadStatus"))
  )
}

#' Upload Data Server Logic
#'
#' This function contains the server-side logic for processing uploads.
#' @param id A unique identifier for the Shiny module
#' @param dataFolder The folder path to save uploaded data
#' @export

uploadData_init_server <- function(id, dataFolder) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ensure studies.rds is initialized
    initialize_studies_rds(dataFolder)

    # Load or initialize existing studies metadata
    existing_studies_path <- file.path(here::here(dataFolder), "studies.rds")
    existing_studies <- shiny::reactiveVal({
      readRDS(existing_studies_path)
    })

    # Process user uploaded files
    shiny::observeEvent(input$uploadButton, {
      shiny::req(input$fileUpload)

      file_path <- input$fileUpload$datapath
      file_name <- input$fileUpload$name
      print(paste("Received file:", file_path, "with name:", file_name))
      print(file_path)
      process_zip_file(file_path, file_name, dataFolder, existing_studies)

      # Update the status text upon file processing
      output$uploadStatus <- shiny::renderText({
        paste("File processed and data updated:", file_name)
      })
    })

    # Display the list of processed studies
    output$available_studies <- DT::renderDT({
      DT::datatable(existing_studies(),
                    options = list(pageLength = 5, searching = FALSE))
    })
  })
}
