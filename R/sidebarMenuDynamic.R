#' Generate a Dynamic Sidebar Menu for a Shiny Dashboard
#'
#' This function creates a dynamic sidebar menu for a Shiny dashboard application. It includes static menu items and dynamically generated menu items based on the provided complete and incomplete data sets.
#'
#' @param complete_data A data frame containing the complete data. It should have columns named `variable_name`, `estimate_name`, and `estimate_value`.
#' @param incomplet_data A list or data frame containing the incomplete data.
#'
#' @return A `sidebarMenu` object for a Shiny dashboard.
#'
#' @import shiny
#'
#' @examples
#' \dontrun{
#' complete_data <- data.frame(
#'   variable_name = c("settings", "settings", "other"),
#'   estimate_name = c("result_type", "result_type", "other"),
#'   estimate_value = c("table1", "table2", "other_value")
#' )
#'
#' incomplet_data <- list(table3 = data.frame(), table4 = data.frame())
#'
#' sidebar <- sidebarMenuDynamic(complete_data, incomplet_data)
#' }
sidebarMenuDynamic <- function(complete_data, incomplet_data) {


  result_tables <- complete_data[complete_data$variable_name == "settings" & complete_data$estimate_name == "result_type", ]
  complete_tables_name <- unique(result_tables$estimate_value)
  incomplete_tables_name <- names(incomplet_data)



  menu_items <- lapply(c(complete_tables_name), function(table_name) {#incomplete_tables_name
    shinydashboard::menuItem(gsub("_", " ", table_name), tabName = paste0("tab_", table_name), icon = shiny::icon("table"))
  })

  sidebar <- do.call(shinydashboard::sidebarMenu, list(
    shinydashboard::menuItem("About", tabName = "About", icon = shiny::icon("circle-info")),
    shinydashboard::menuItem("Upload Data", tabName = "UploadData", icon = shiny::icon("upload")),
    shinydashboard::menuItem("Load Data", tabName = "LoadData", icon = shiny::icon("bars-progress")),
    menu_items,
    shinydashboard::menuItem("Contact", tabName = "contact", icon = icon("id-card"))
  ))

  sidebar
}
