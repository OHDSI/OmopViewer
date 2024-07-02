#' Create a Bootstrap Card with Collapsible Content for a Shiny Dashboard
#'
#' This function generates a Bootstrap card with a collapsible header and content. The content
#' includes UI elements for settings, filters, and tables, with an optional extra UI element
#' that can be displayed in a separate tab.
#'
#' @param id The ID for the collapsible card element.
#' @param title The title to be displayed on the card header.
#' @param setting_filter_ui UI element for the settings filter.
#' @param filter_ui UI element for the filter.
#' @param table_ui UI element for the table.
#' @param extra_ui Optional UI element for additional content, such as a plot (default is NULL).
#'
#' @return A `div` element containing the card structure for use in a Shiny UI.
#'
#' @import shiny
#'
#' @examples
#' \dontrun{
#' createCard(
#'   id = "example_card",
#'   title = "Example Card",
#'   setting_filter_ui = selectInput("setting", "Setting", choices = c("A", "B")),
#'   filter_ui = selectInput("filter", "Filter", choices = c("X", "Y")),
#'   table_ui = DT::dataTableOutput("example_table"),
#'   extra_ui = plotOutput("example_plot")
#' )
#' }
#' @export
createCard <- function(id, title, setting_filter_ui, filter_ui, table_ui, extra_ui = NULL) {
  shiny::div(
    class = "card",
    shiny::div(
      class = "card-header",
      `data-toggle` = "collapse",
      `data-target` = paste0("#", id),
      title,
      tags$i(class = "fas fa-chevron-down collapse-toggle")
    ),
    shiny::div(
      id = id,
      class = "card-body collapse in",
      setting_filter_ui,
      style = "margin-bottom: 5px;",
      filter_ui,
      style = "margin-bottom: 5px;",
      # Use tabsetPanel to organize table_ui and extra_ui into separate tabs
      shiny::tabsetPanel(
        shiny::tabPanel("Table", table_ui),
        if (!is.null(extra_ui)) shiny::tabPanel("Plot", extra_ui),
        id = paste0(id, "_tabs")
      )
    ),
    options = list(handles = 's, e, se', containment = "parent")
  )
}
