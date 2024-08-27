# empty shiny

    Code
      cat(uiStatic(asText = TRUE), sep = "\n")
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
            shinydashboard::tabItem(
              tabName = "about",
              shiny::div(
                class = "about",
                shiny::tags$h2(shiny::tagList(shiny::strong("omopViewer"), "shiny app")),
                shiny::tags$h4(shiny::tagList(
                  "This shiny app was generated with ",
                  shiny::a(
                    "omopViewer",
                    href = "https://github.com/oxford-pharmacoepi/omopViewer",
                    target = "_blank"
                  ),
                  shiny::strong("v0.0.0.900")
                )),
                shiny::tags$h5("omopViewer works only with `summarised_result` objects as
        defined in omopgenerics package."),
                shiny::tags$img(
                  src = system.file("www/images/hds_logo.svg", package = "omopViewer"),
                  class = "logo-img",
                  alt = "HDS Logo",
                  height = "10%",
                  width = "10%",
                  style = "float:right"
                )
              )
            ),
            ## background ----
            shinydashboard::tabItem(
              tabName = "background",
              shiny::h4("Study background"),
              shiny::p("You can use this section to add some background of your study")
            )
            ## end ----
          )
        )
      )

---

    Code
      cat(serverStatic(asText = TRUE), sep = "\n")
    Output
      server <- function(input, output, session) {
        # end -----
      }

# CohortCharacteristics shiny

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
              text = "Cohort count",
              tabName = "cohort_overlap",
              icon = shiny::icon("person")
            ),
            shinydashboard::menuItem(
              text = "Cohort timing",
              tabName = "cohort_timing",
              icon = shiny::icon("person")
            ),
            shinydashboard::menuItem(
              text = "Cohort characteristics",
              tabName = "summarised_characteristics",
              icon = shiny::icon("people-group")
            ),
            shinydashboard::menuItem(
              text = "Summarised large scale characteristics",
              tabName = "summarised_large_scale_characteristics",
              icon = shiny::icon("table")
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
            shinydashboard::tabItem(
              tabName = "about",
              shiny::div(
                class = "about",
                shiny::tags$h2(shiny::tagList(shiny::strong("omopViewer"), "shiny app")),
                shiny::tags$h4(shiny::tagList(
                  "This shiny app was generated with ",
                  shiny::a(
                    "omopViewer",
                    href = "https://github.com/oxford-pharmacoepi/omopViewer",
                    target = "_blank"
                  ),
                  shiny::strong("v0.0.0.900")
                )),
                shiny::tags$h5("omopViewer works only with `summarised_result` objects as
        defined in omopgenerics package."),
                shiny::tags$img(
                  src = system.file("www/images/hds_logo.svg", package = "omopViewer"),
                  class = "logo-img",
                  alt = "HDS Logo",
                  height = "10%",
                  width = "10%",
                  style = "float:right"
                )
              )
            ),
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
                choices = c("cohort"),
                selected = c("cohort"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_attrition_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
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
                choices = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                selected = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
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
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_attrition_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_attrition_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
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
            ## cohort_overlap ----
            shinydashboard::tabItem(
              tabName = "cohort_overlap",
              shiny::p(),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "cohort_overlap_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_overlap_groupping_cohort_name_reference",
                label = "Cohort name reference",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_overlap_groupping_cohort_name_comparator",
                label = "Cohort name comparator",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "cohort_overlap_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("comparator", "overlap", "reference"),
                selected = c("comparator", "overlap", "reference"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_overlap_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count", "percentage"),
                selected = c("count", "percentage"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  title = "Raw table",
                  shiny::checkboxInput(
                    inputId = "cohort_overlap_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_overlap_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_overlap_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "cohort_overlap_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "cohort_overlap_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "cohort_overlap_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_overlap_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_overlap_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "cohort_overlap_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "cohort_overlap_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot cohort overlap",
                  shinyWidgets::pickerInput(
                    inputId = "cohort_overlap_plot_1_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_overlap_plot_1_unique_combinations",
                    label = "uniqueCombinations",
                    value = c(TRUE)
                  ),
                  shiny::downloadButton(outputId = "cohort_overlap_plot_1_download", label = "Download"),
                  shiny::plotOutput(outputId = "cohort_overlap_plot_1") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## cohort_timing ----
            shinydashboard::tabItem(
              tabName = "cohort_timing",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_settings_restrict_to_first_entry",
                label = "Restrict to first entry",
                choices = c("TRUE"),
                selected = c("TRUE"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_groupping_cohort_name_reference",
                label = "Cohort name reference",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_groupping_cohort_name_comparator",
                label = "Cohort name comparator",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("days_between_cohort_entries", "number records", "number subjects"),
                selected = c("days_between_cohort_entries", "number records", "number subjects"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "cohort_timing_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count", "max", "median", "min", "q25", "q75"),
                selected = c("count", "max", "median", "min", "q25", "q75"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  title = "Raw table",
                  shiny::checkboxInput(
                    inputId = "cohort_timing_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_timing_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_timing_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "cohort_timing_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "cohort_timing_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = c("restrict_to_first_entry"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "cohort_timing_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "cohort_timing_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "plot cohort timing",
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_plot_3_plot_type",
                    label = "plotType",
                    choices = c("boxplot", "density"),
                    selected = c("boxplot"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_plot_3_time_scale",
                    label = "timeScale",
                    choices = c("days", "years"),
                    selected = c("days"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_plot_3_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "cohort_timing_plot_3_colour",
                    label = "colour",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "cohort_timing_plot_3_unique_combinations",
                    label = "uniqueCombinations",
                    value = c(TRUE)
                  ),
                  shiny::downloadButton(outputId = "cohort_timing_plot_3_download", label = "Download"),
                  shiny::plotOutput(outputId = "cohort_timing_plot_3") |>
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
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                selected = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_characteristics_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                selected = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
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
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_characteristics_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
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
            ),
            ## summarised_large_scale_characteristics ----
            shinydashboard::tabItem(
              tabName = "summarised_large_scale_characteristics",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_settings_table_name",
                label = "Table name",
                choices = c("condition_occurrence", "drug_exposure"),
                selected = c("condition_occurrence", "drug_exposure"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_settings_type",
                label = "Type",
                choices = c("episode", "event"),
                selected = c("episode", "event"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_settings_analysis",
                label = "Analysis",
                choices = c("standard"),
                selected = c("standard"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_groupping_sex",
                label = "Sex",
                choices = c("Female", "Male", "overall"),
                selected = c("Female", "Male", "overall"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_groupping_age_group",
                label = "Age group",
                choices = c("0 to 44", "45 or above", "overall"),
                selected = c("0 to 44", "45 or above", "overall"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_groupping_concept_id",
                label = "Concept id",
                choices = c("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "3", "4", "5", "6", "7", "8"),
                selected = c("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "3", "4", "5", "6", "7", "8"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("ALIMENTARY TRACT AND METABOLISM", "Adalimumab", "Arthritis", "Arthropathies", "Degenerative arthropathy", "Descendant drug", "Diseases of the musculoskeletal system and connective tissue", "Injectable", "Injection", "Knee osteoarthritis", "Musculoskeletal disorder", "OA", "Osteoarthritis of hip", "Osteoarthritis of knee", "Osteoarthrosis", "Osteonecrosis", "Other ingredient"),
                selected = c("ALIMENTARY TRACT AND METABOLISM", "Adalimumab", "Arthritis", "Arthropathies", "Degenerative arthropathy", "Descendant drug", "Diseases of the musculoskeletal system and connective tissue", "Injectable", "Injection", "Knee osteoarthritis", "Musculoskeletal disorder", "OA", "Osteoarthritis of hip", "Osteoarthritis of knee", "Osteoarthrosis", "Osteonecrosis", "Other ingredient"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarised_large_scale_characteristics_variables_and_estimates_estimate_name",
                label = "Estimate name",
                choices = c("count", "percentage"),
                selected = c("count", "percentage"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  title = "Raw table",
                  shiny::checkboxInput(
                    inputId = "summarised_large_scale_characteristics_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarised_large_scale_characteristics_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarised_large_scale_characteristics_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarised_large_scale_characteristics_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarised_large_scale_characteristics_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarised_large_scale_characteristics_header",
                    label = "Header",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_large_scale_characteristics_group",
                    label = "Group",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarised_large_scale_characteristics_hide",
                    label = "Hide",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = c("table_name", "type", "analysis"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarised_large_scale_characteristics_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarised_large_scale_characteristics_formatted_table") |>
                    shinycssloaders::withSpinner()
                )
              )
            )
            ## end ----
          )
        )
      )

---

    Code
      cat(serverStatic(result = result, asText = TRUE), sep = "\n")
    Output
      server <- function(input, output, session) {
        # summarised_characteristics ----
        getRawDataSummarisedCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarised_characteristics", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarised_characteristics_show_settings,
              showGroupping = input$summarised_characteristics_show_groupping,
              pivotEstimates = input$summarised_characteristics_pivot_estimates
            )
        })
        output$summarised_characteristics_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummarisedCharacteristics(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummarisedCharacteristics <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarised_characteristics", input)
          header <- input$summarised_characteristics_header
          group <- input$summarised_characteristics_group
          hide <- input$summarised_characteristics_hide
          all <- c(header, group, hide)
          shiny::validate(shiny::need(
            length(all) == length(unique(all)),
            "there must not be overlap between `header`, `group` and `hide`"
          ))
          omopViewer::visTable(
            result = x,
            header = header,
            group = group,
            hide = hide
          )
        })
        output$summarised_characteristics_formatted_table <- gt::render_gt({
          getFormattedDataSummarisedCharacteristics()
        })
        output$summarised_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarised_characteristics.docx",
          content = function(file) {
            getFormattedDataSummarisedCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummarisedCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarised_characteristics", input)
        })
      
        createPlot4 <- shiny::reactive({
          result <- getPlotDataSummarisedCharacteristics()
          CohortCharacteristics::plotCharacteristics(
            result,
            x = input$summarised_characteristics_plot_4_x,
            facet = input$summarised_characteristics_plot_4_facet,
            colour = input$summarised_characteristics_plot_4_colour,
            plotStyle = input$summarised_characteristics_plot_4_plot_style
          )
        })
        output$summarised_characteristics_plot_4 <- shiny::renderPlot({
          createPlot4()
        })
        output$summarised_characteristics_plot_4_download <- shiny::downloadHandler(
          filename = "plot_summarised_characteristics.png",
          content = function(file) {
            plt <- createPlot4()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # cohort_attrition ----
        getRawDataCohortAttrition <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_attrition", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_attrition_show_settings,
              showGroupping = input$cohort_attrition_show_groupping,
              pivotEstimates = input$cohort_attrition_pivot_estimates
            )
        })
        output$cohort_attrition_raw_table <- DT::renderDT({
          DT::datatable(getRawDataCohortAttrition(), options = list(scrollX = TRUE))
        })
        getFormattedDataCohortAttrition <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("cohort_attrition", input)
          header <- input$cohort_attrition_header
          group <- input$cohort_attrition_group
          hide <- input$cohort_attrition_hide
          all <- c(header, group, hide)
          shiny::validate(shiny::need(
            length(all) == length(unique(all)),
            "there must not be overlap between `header`, `group` and `hide`"
          ))
          omopViewer::visTable(
            result = x,
            header = header,
            group = group,
            hide = hide
          )
        })
        output$cohort_attrition_formatted_table <- gt::render_gt({
          getFormattedDataCohortAttrition()
        })
        output$cohort_attrition_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_cohort_attrition.docx",
          content = function(file) {
            getFormattedDataCohortAttrition() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataCohortAttrition <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_attrition", input)
        })
      
        createPlot2 <- shiny::reactive({
          result <- getPlotDataCohortAttrition()
          CohortCharacteristics::plotCohortAttrition(
            result
          )
        })
        output$cohort_attrition_plot_2 <- DiagrammeR::renderGrViz({
          createPlot2()
        })
        output$cohort_attrition_plot_2_download <- shiny::downloadHandler(
          filename = "plot_cohort_attrition.png",
          content = function(file) {
            plt <- createPlot2()
            DiagrammeR::export_graph(graph = plt, file_name = file, fily_type = "png", width = 800)
          }
        )
        # cohort_overlap ----
        getRawDataCohortOverlap <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_overlap", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_overlap_show_settings,
              showGroupping = input$cohort_overlap_show_groupping,
              pivotEstimates = input$cohort_overlap_pivot_estimates
            )
        })
        output$cohort_overlap_raw_table <- DT::renderDT({
          DT::datatable(getRawDataCohortOverlap(), options = list(scrollX = TRUE))
        })
        getFormattedDataCohortOverlap <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("cohort_overlap", input)
          header <- input$cohort_overlap_header
          group <- input$cohort_overlap_group
          hide <- input$cohort_overlap_hide
          all <- c(header, group, hide)
          shiny::validate(shiny::need(
            length(all) == length(unique(all)),
            "there must not be overlap between `header`, `group` and `hide`"
          ))
          omopViewer::visTable(
            result = x,
            header = header,
            group = group,
            hide = hide
          )
        })
        output$cohort_overlap_formatted_table <- gt::render_gt({
          getFormattedDataCohortOverlap()
        })
        output$cohort_overlap_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_cohort_overlap.docx",
          content = function(file) {
            getFormattedDataCohortOverlap() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataCohortOverlap <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_overlap", input)
        })
      
        createPlot1 <- shiny::reactive({
          result <- getPlotDataCohortOverlap()
          CohortCharacteristics::plotCohortOverlap(
            result,
            facet = input$cohort_overlap_plot_1_facet,
            uniqueCombinations = input$cohort_overlap_plot_1_unique_combinations
          )
        })
        output$cohort_overlap_plot_1 <- shiny::renderPlot({
          createPlot1()
        })
        output$cohort_overlap_plot_1_download <- shiny::downloadHandler(
          filename = "plot_cohort_overlap.png",
          content = function(file) {
            plt <- createPlot1()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # cohort_timing ----
        getRawDataCohortTiming <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_timing", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_timing_show_settings,
              showGroupping = input$cohort_timing_show_groupping,
              pivotEstimates = input$cohort_timing_pivot_estimates
            )
        })
        output$cohort_timing_raw_table <- DT::renderDT({
          DT::datatable(getRawDataCohortTiming(), options = list(scrollX = TRUE))
        })
        getFormattedDataCohortTiming <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("cohort_timing", input)
          header <- input$cohort_timing_header
          group <- input$cohort_timing_group
          hide <- input$cohort_timing_hide
          all <- c(header, group, hide)
          shiny::validate(shiny::need(
            length(all) == length(unique(all)),
            "there must not be overlap between `header`, `group` and `hide`"
          ))
          omopViewer::visTable(
            result = x,
            header = header,
            group = group,
            hide = hide
          )
        })
        output$cohort_timing_formatted_table <- gt::render_gt({
          getFormattedDataCohortTiming()
        })
        output$cohort_timing_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_cohort_timing.docx",
          content = function(file) {
            getFormattedDataCohortTiming() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataCohortTiming <- shiny::reactive({
          data |>
            omopViewer::filterData("cohort_timing", input)
        })
      
        createPlot3 <- shiny::reactive({
          result <- getPlotDataCohortTiming()
          CohortCharacteristics::plotCohortTiming(
            result,
            plotType = input$cohort_timing_plot_3_plot_type,
            timeScale = input$cohort_timing_plot_3_time_scale,
            facet = input$cohort_timing_plot_3_facet,
            colour = input$cohort_timing_plot_3_colour,
            uniqueCombinations = input$cohort_timing_plot_3_unique_combinations
          )
        })
        output$cohort_timing_plot_3 <- shiny::renderPlot({
          createPlot3()
        })
        output$cohort_timing_plot_3_download <- shiny::downloadHandler(
          filename = "plot_cohort_timing.png",
          content = function(file) {
            plt <- createPlot3()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # summarised_large_scale_characteristics ----
        getRawDataSummarisedLargeScaleCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarised_large_scale_characteristics", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarised_large_scale_characteristics_show_settings,
              showGroupping = input$summarised_large_scale_characteristics_show_groupping,
              pivotEstimates = input$summarised_large_scale_characteristics_pivot_estimates
            )
        })
        output$summarised_large_scale_characteristics_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummarisedLargeScaleCharacteristics(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummarisedLargeScaleCharacteristics <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarised_large_scale_characteristics", input)
          header <- input$summarised_large_scale_characteristics_header
          group <- input$summarised_large_scale_characteristics_group
          hide <- input$summarised_large_scale_characteristics_hide
          all <- c(header, group, hide)
          shiny::validate(shiny::need(
            length(all) == length(unique(all)),
            "there must not be overlap between `header`, `group` and `hide`"
          ))
          omopViewer::visTable(
            result = x,
            header = header,
            group = group,
            hide = hide
          )
        })
        output$summarised_large_scale_characteristics_formatted_table <- gt::render_gt({
          getFormattedDataSummarisedLargeScaleCharacteristics()
        })
        output$summarised_large_scale_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarised_large_scale_characteristics.docx",
          content = function(file) {
            getFormattedDataSummarisedLargeScaleCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        # end -----
      }

