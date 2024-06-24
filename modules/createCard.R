# Helper function to create cards
createCard <- function(id, title, setting_filter_ui, filter_ui, table_ui,extra_ui = NULL) {
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
      table_ui,
      style = "margin-top: 5px;",
      extra_ui
    ),
    options = list(handles = 's, e, se', containment = "parent")
  )
}
