sidebarMenuDynamic <- function(complete_data, incomplet_data) {

  menu_items <- NULL
  if (!is.null(complete_data)) {
    result_tables <- omopgenerics::settings(complete_data)
    complete_tables_name <- unique(result_tables$result_type)
    incomplete_tables_name <- names(incomplet_data)

    menu_items <- lapply(c(complete_tables_name, incomplete_tables_name), function(table_name) {
      shinydashboard::menuItem(gsub("_", " ", table_name), tabName = paste0("tab_", table_name), icon = shiny::icon("table"))
    })
  }

  sidebar <- do.call(shinydashboard::sidebarMenu, c(
    list(
      shinydashboard::menuItem("About", tabName = "About", icon = shiny::icon("circle-info")),
      shinydashboard::menuItem("Upload Data", tabName = "UploadData", icon = shiny::icon("upload")),
      shinydashboard::menuItem("Load Data", tabName = "LoadData", icon = shiny::icon("bars-progress"))
    ),
    menu_items,  # Unlist menu_items to pass them as individual arguments
    list(
      shinydashboard::menuItem("Contact", tabName = "contact", icon = shiny::icon("id-card"))
    )
  ))

  sidebar
}
