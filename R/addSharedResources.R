# Helper function to include shared styles and scripts
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
addSharedResources <- function() {
  shiny::tagList(
    shiny::tags$head(
      tags$script(HTML("
        $(document).ready(function() {
          $('.card-header').click(function() {
            var icon = $(this).find('.collapse-toggle');
            var content = $(this).next('.card-body');
            content.collapse('toggle');
            icon.toggleClass('fa-chevron-down fa-chevron-up');
          });
        });
      ")),
      shiny::tags$style(shiny::HTML("
        .card {
          background-color: white;
          margin-bottom: 20px; /* Spacing between cards */
          border: 1px solid #ddd; /* Adds a border to the card for better visibility */
        }
        .card-header {
          background-color: #f7f7f7; /* Slightly different background for the header */
          cursor: pointer; /* Indicates the header is clickable */
          padding: 10px 15px; /* Proper padding for the header */
          display: flex;
          justify-content: space-between;
          align-items: center;
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
    )
  )
}
