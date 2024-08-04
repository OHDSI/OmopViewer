#' Generic UI Function for Dynamic Plot Modules
#'
#' @param id A string that uniquely identifies the UI instance for namespacing elements.
#' @param moduleType A string specifying the type of module, which dictates the UI elements
#'        to generate as defined in `plot_config`.
#' @details
#' The `genericUI` function supports creating a flexible, dynamic user interface in a
#' Shiny application, aligning with the modular design dictated by `plot_config`. It
#' automatically generates UI components such as picker inputs for facets, colours, and
#' other configurable options, as well as output slots for plots and download buttons.
#' @return A Shiny UI definition that can be integrated into the application layout.
#' @export
genericUI <- function(id, moduleType) {
  ns <- shiny::NS(id)
  uiComponents <- list()
  # Create UI components dynamically based on the configuration in plotParams
  for (paramName in names(plot_config[[moduleType]]$plotButtons)) {
    # Extracting details for each UI element
    paramDetails <- plot_config[[moduleType]]$plotButtons[[paramName]]

    # Determining the label based on the parameter name
    label <- switch(paramName,
                    facet = "Facet by",
                    colour = "Colour by",
                    colourVars =  "Colour by",
                    position = "Position",
                    plotStyle = "Plot style",
                    plotType = "Plot type",
                    splitStrata = "Split strata",
                    x = "X Axis",
                    variable_name = "Variable name",
                    uniqueCombinations = "Unique combinations",
                    variable_level = "Variable level"
                    # default = paramName  # Default case to handle other unlisted parameters
    )

    # Initialize pickerInput with predefined choices if available, otherwise with NULL
    if ("choices" %in% names(paramDetails)) {
      uiComponents[[paramName]] <- shinyWidgets::pickerInput(
        ns(paramDetails$id),
        label,
        choices = paramDetails$choices,
        selected = paramDetails$selected,
        multiple = paramDetails$multiple,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
      )
    } else {
      uiComponents[[paramName]] <- shinyWidgets::pickerInput(
        ns(paramDetails$id),
        label,
        choices = NULL,
        selected = NULL,
        multiple = paramDetails$multiple,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
      )
    }
  }

  # Add a unique plot output and download button for each module
  uiComponents[[paste0(moduleType, "_plot")]] = plotly::plotlyOutput(ns(paste0(moduleType, "_plot")), height = "400px")
  uiComponents[[paste0(moduleType, "_download")]] = shiny::downloadButton(ns(paste0(moduleType, "_download_png")), "Download PNG")

  # Return the assembled UI
  return(shiny::fluidPage(
    shiny::div(style = "margin: 20px;", uiComponents)
  ))
}
