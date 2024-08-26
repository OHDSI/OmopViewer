# multiplication works

    Code
      cat(uiStatic(result = result, asText = TRUE), sep = "\n")
    Output
      ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "My study"),
        # sidebar ----
        shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem(
              text = "About", tabName = "about", icon = shiny::icon("circle-info")
            ),
            shinydashboard::menuItem(
              text = "Background", tabName = "background", icon = shiny::icon("magnifying-glass")
            ),
            shinydashboard::menuItem(
              text = "Cohort Attrition",
              tabName = "cohort_attrition",
              icon = shiny::icon("person")
            ),
            shinydashboard::menuItem(
              text = "Cohort characteristics",
              tabName = "summarised_characteristics",
              icon = shiny::icon("people-group")
            )
          )
        ),
        # body ----
        shinydashboard::dashboardBody(
          shiny::tags$head(
            # Reset favicon
            shiny::tags$link(rel = "shortcut icon", href = "#"),
            # Compiled css file
            shiny::tags$link(
              rel = "stylesheet",
              type = "text/css",
              href = system.file("www/css/sass.min.css", package = "omopViewer")
            )
          ),
          shinydashboard::tabItems(
            ## about ----
            shinydashboard::tabItem(tabName = "about", omopViewer::aboutTab()),
            ## background ----
            shinydashboard::tabItem(
              tabName = "background",
              shiny::h4("Study background"),
              shiny::p("You can use this section to add some background of your study")
            ),
            ## cohort_attrition ----
            shinydashboard::tabItem(
              tabName = "cohort_attrition",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_settings_cohort_definition_id",
                label = "Cohort definition id",
                choices = c("1", "2", "3"),
                selected = c("1", "2", "3"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_settings_table_name",
                label = "Table name",
                choices = c("cohort1"),
                selected = c("cohort1"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_cdm_name",
                label = "Cdm name",
                choices = c("PP_MOCK"),
                selected = c("PP_MOCK"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_cohort_name",
                label = "Cohort name",
                choices = c("cohort_1", "cohort_2", "cohort_3"),
                selected = c("cohort_1", "cohort_2", "cohort_3"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_reason",
                label = "Reason",
                choices = c("Initial qualifying events"),
                selected = c("Initial qualifying events"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_reason_id",
                label = "Reason id",
                choices = c("1"),
                selected = c("1"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("number_records", "number_subjects", "excluded_records", "excluded_subjects"),
                selected = c("number_records", "number_subjects", "excluded_records", "excluded_subjects"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count"),
                selected = c("count"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  title = "Raw table",
                  shiny::checkboxInput(
                    inputId = "cohort_attrition_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_attrition_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_attrition_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "cohort_attrition_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "cohort_attrition_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "cohort_attrition_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name", "reason", "reason_id", "variable_name", "variable_level", "estimate_name", "cohort_definition_id", "table_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_attrition_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name", "reason", "reason_id", "variable_name", "variable_level", "estimate_name", "cohort_definition_id", "table_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_attrition_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "reason", "reason_id", "variable_name", "variable_level", "estimate_name", "cohort_definition_id", "table_name"),
                    selected = c("cohort_definition_id", "table_name"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "cohort_attrition_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "cohort_attrition_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Diagram",
                  shiny::downloadButton(outputId = "cohort_attrition_plot_2_download", label = "Download"),
                  DiagrammeR::grVizOutput(outputId = "cohort_attrition_plot_2") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarised_characteristics ----
            shinydashboard::tabItem(
              tabName = "summarised_characteristics",
              shiny::p(),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_groupping_cdm_name",
                label = "Cdm name",
                choices = c("PP_MOCK"),
                selected = c("PP_MOCK"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_groupping_cohort_name",
                label = "Cohort name",
                choices = c("cohort_3", "cohort_1", "cohort_2"),
                selected = c("cohort_3", "cohort_1", "cohort_2"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation", "Future observation"),
                selected = c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation", "Future observation"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count", "min", "q25", "median", "q75", "max", "mean", "sd", "percentage"),
                selected = c("count", "min", "q25", "median", "q75", "max", "mean", "sd", "percentage"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  title = "Raw table",
                  shiny::checkboxInput(
                    inputId = "summarised_characteristics_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarised_characteristics_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarised_characteristics_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarised_characteristics_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarised_characteristics_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarised_characteristics_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarised_characteristics_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot characteristics",
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_plot_4_x",
                    label = "x",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = c("variable_name"),
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_plot_4_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_plot_4_colour",
                    label = "colour",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_plot_4_plot_style",
                    label = "plotStyle",
                    choices = c("boxplot", "barplot"),
                    selected = c("barplot"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarised_characteristics_plot_4_download", label = "Download"),
                  shiny::plotOutput(outputId = "summarised_characteristics_plot_4") |>
                    shinycssloaders::withSpinner()
                )
              )
            )
            ## end ----
          )
        )
      )

