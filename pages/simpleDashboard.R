library(shiny)
library(shinyjqui)  # Required for interactive UI features

simpleDashboardTab <- div(
  class = "container-fluid",
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        window.toggleCollapse = function(id) {
          $('#' + id).collapse('toggle');
          var icon = $('#' + id).prev().find('.collapse-toggle');
          icon.toggleClass('fa-chevron-down fa-chevron-up');
        }
      });
    ")),
    tags$style(HTML("
      .card {
        background-color: white;
        margin-bottom: 20px; /* Spacing between cards */
        border: 1px solid #ddd; /* Adds a border to the card for better visibility */
      }
      .card-header {
        background-color: #f7f7f7; /* Slightly different background for the header */
        cursor: pointer; /* Indicates the header is clickable */
        padding: 10px 15px; /* Proper padding for the header */
      }
      .card-body {
        padding: 20px;
        overflow: hidden; /* Prevent content from spilling outside */
      }
      .collapse-toggle {
        float: right; /* Position the toggle button to the right */
        color: #337ab7; /* Bootstrap's default link color for visibility */
        font-size: 18px; /* Larger icon size for better accessibility */
      }
    "))
  ),
  div(
    class = "row",
    div(
      class = "col-md-12",
      # jqui_resizable(
      #   jqui_draggable(
          div(
            class = "card",
            div(
              class = "card-header",
              onClick = "toggleCollapse('cohortContent')",  # JavaScript function to toggle collapse
              "Cohort Counts",
              tags$i(class = "fas fa-chevron-down collapse-toggle")  # Chevron icon
            ),
            div(
              id = "cohortContent",
              class = "card-body collapse in",
              div(
                filter_function$filter_module_ui("cohort_filter_simple"),
                style = "margin-bottom: 5px;"
              ),
              div(
                table_function$ui("cohort_count_view_simple"),
                style = "margin-top: 5px;"
              )
            ),
            options = list(handles = 's, e, se', containment = "parent")
          )
      #   )
      # )
    ),
    div(
      class = "col-md-12",
      # jqui_resizable(
        # jqui_draggable(
          div(
            class = "card",
            div(
              class = "card-header",
              onClick = "toggleCollapse('incidenceContent')",
              "Incidence",
              tags$i(class = "fas fa-chevron-down collapse-toggle")
            ),
            div(
              id = "incidenceContent",
              class = "card-body collapse in",
              div(
                filter_function$filter_module_ui("incidence_filter_simple"),
                style = "margin-bottom: 5px;"
              ),
              div(
                table_function$ui("incidence_view_simple"),
                style = "margin-top: 5px;"
              ),
              div(
                graph_incidence$ui("incidence_plot_view_simple"),
                style = "margin-top: 5px;"
              )
            ),
            options = list(handles = 's, e, se', containment = "parent")
          )
        # )
      # )
    )
  )
)

# # Custom JavaScript to toggle the collapse state
# tags$script(HTML("
#   function toggleCollapse(id) {
#     $('#' + id).collapse('toggle');
#     var icon = $('#' + id).prev().find('.collapse-toggle');
#     icon.toggleClass('fa-chevron-down fa-chevron-up');  // Toggle the direction of the chevron icon
#   }
# "))
