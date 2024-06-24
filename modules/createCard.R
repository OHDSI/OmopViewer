createCard <- function(id, title, setting_filter_ui, filter_ui, table_ui, extra_ui = NULL) {
  div(
    class = "card",
    div(
      class = "card-header",
      `data-toggle` = "collapse",
      `data-target` = paste0("#", id),
      title,
      tags$i(class = "fas fa-chevron-down collapse-toggle")
    ),
    div(
      id = id,
      class = "card-body collapse in",
      setting_filter_ui,
      style = "margin-bottom: 5px;",
      filter_ui,
      style = "margin-bottom: 5px;",
      # Use tabsetPanel to organize table_ui and extra_ui into separate tabs
      tabsetPanel(
        tabPanel("Table", table_ui),
        if (!is.null(extra_ui)) tabPanel("Plot", extra_ui),
        id = paste0(id, "_tabs")
      )
    ),
    options = list(handles = 's, e, se', containment = "parent")
  )
}
