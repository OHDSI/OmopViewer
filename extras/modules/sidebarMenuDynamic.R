sidebarMenuDynamic <- function(complete_data, incomplet_data) {

  
  result_tables <- complete_data[complete_data$variable_name == "settings" & complete_data$estimate_name == "result_type", ]
  complete_tables_name <- unique(result_tables$estimate_value)
  incomplete_tables_name <- names(incomplet_data)

  
  
  menu_items <- lapply(c(complete_tables_name, incomplete_tables_name), function(table_name) {
    menuItem(gsub("_", " ", table_name), tabName = paste0("tab_", table_name), icon = icon("table"))
  })
  
  sidebar <- do.call(sidebarMenu, list(
    menuItem("About", tabName = "About", icon = icon("circle-info")),
    menuItem("Upload Data", tabName = "UploadData", icon = icon("upload")),
    menuItem("Load Data", tabName = "LoadData", icon = icon("bars-progress")),
    menu_items,
    menuItem("Contact", tabName = "contact", icon = icon("id-card"))
  ))
  
  sidebar
}
