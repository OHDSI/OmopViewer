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
              text = "Background", tabName = "background", icon = shiny::icon("disease")
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
              text = "Background", tabName = "background", icon = shiny::icon("disease")
            ),
            shinydashboard::menuItem(
              text = "Cohort characteristics",
              tabName = "summarise_characteristics",
              icon = shiny::icon("users-gear")
            ),
            shinydashboard::menuItem(
              text = "Cohort Attrition",
              tabName = "summarise_cohort_attrition",
              icon = shiny::icon("layer-group")
            ),
            shinydashboard::menuItem(
              text = "Cohort count",
              tabName = "summarise_cohort_count",
              icon = shiny::icon("users")
            ),
            shinydashboard::menuItem(
              text = "Cohort overlap",
              tabName = "summarise_cohort_overlap",
              icon = shiny::icon("circle-half-stroke")
            ),
            shinydashboard::menuItem(
              text = "Cohort timing",
              tabName = "summarise_cohort_timing",
              icon = shiny::icon("chart-simple")
            ),
            shinydashboard::menuItem(
              text = "Large Scale Characteristics",
              tabName = "summarise_large_scale_characteristics",
              icon = shiny::icon("arrow-up-right-dots")
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
            ## summarise_characteristics ----
            shinydashboard::tabItem(
              tabName = "summarise_characteristics",
              shiny::p(),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_characteristics_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_characteristics_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_characteristics_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                selected = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_characteristics_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_characteristics_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_characteristics_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_characteristics_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_characteristics_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_characteristics_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_characteristics_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot characteristics",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_4_plot_style",
                    label = "plotStyle",
                    choices = c("boxplot", "barplot", "scatterplot"),
                    selected = c("barplot"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_4_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_characteristics_plot_4_colour",
                    label = "colour",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_characteristics_plot_4_download", label = "Download"),
                  shiny::plotOutput(outputId = "summarise_characteristics_plot_4") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarise_cohort_attrition ----
            shinydashboard::tabItem(
              tabName = "summarise_cohort_attrition",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                label = "Cohort definition id",
                choices = c("1", "2", "3"),
                selected = c("1", "2", "3"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_settings_table_name",
                label = "Table name",
                choices = c("cohort"),
                selected = c("cohort"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_groupping_reason",
                label = "Reason",
                choices = c("Initial qualifying events"),
                selected = c("Initial qualifying events"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_groupping_reason_id",
                label = "Reason id",
                choices = c("1"),
                selected = c("1"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                selected = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_attrition_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_cohort_attrition_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_attrition_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_attrition_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_attrition_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_cohort_attrition_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_attrition_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_attrition_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_attrition_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_definition_id", "cohort_name", "estimate_name", "reason", "reason_id", "table_name", "variable_level", "variable_name"),
                    selected = c("cohort_definition_id", "table_name"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_cohort_attrition_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Diagram",
                  shiny::downloadButton(outputId = "summarise_cohort_attrition_plot_2_download", label = "Download"),
                  DiagrammeR::grVizOutput(outputId = "summarise_cohort_attrition_plot_2") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarise_cohort_count ----
            shinydashboard::tabItem(
              tabName = "summarise_cohort_count",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_count_settings_table_name",
                label = "Table name",
                choices = c("cohort"),
                selected = c("cohort"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_count_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_count_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_count_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("Number records", "Number subjects"),
                selected = c("Number records", "Number subjects"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_count_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_cohort_count_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_count_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_count_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_count_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_cohort_count_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "table_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "table_name", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name", "estimate_name", "table_name", "variable_level", "variable_name"),
                    selected = c("table_name"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_cohort_count_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot cohort count",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_plot_5_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = c("cdm_name"),
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_count_plot_5_colour",
                    label = "colour",
                    choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                    selected = NULL,
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_count_plot_5_download", label = "Download"),
                  shiny::plotOutput(outputId = "summarise_cohort_count_plot_5") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarise_cohort_overlap ----
            shinydashboard::tabItem(
              tabName = "summarise_cohort_overlap",
              shiny::p(),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_overlap_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_overlap_groupping_cohort_name_reference",
                label = "Cohort name reference",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_overlap_groupping_cohort_name_comparator",
                label = "Cohort name comparator",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_overlap_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("comparator", "overlap", "reference"),
                selected = c("comparator", "overlap", "reference"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_overlap_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_cohort_overlap_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_overlap_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_overlap_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_overlap_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_cohort_overlap_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_overlap_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_overlap_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_overlap_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "estimate_name", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_cohort_overlap_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot cohort overlap",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_overlap_plot_1_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                    selected = c("cdm_name", "cohort_name_reference"),
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_overlap_plot_1_unique_combinations",
                    label = "uniqueCombinations",
                    value = c(FALSE)
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_overlap_plot_1_download", label = "Download"),
                  shiny::plotOutput(outputId = "summarise_cohort_overlap_plot_1") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarise_cohort_timing ----
            shinydashboard::tabItem(
              tabName = "summarise_cohort_timing",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_settings_restrict_to_first_entry",
                label = "Restrict to first entry",
                choices = c("TRUE"),
                selected = c("TRUE"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_settings_density",
                label = "Density",
                choices = c("FALSE"),
                selected = c("FALSE"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_groupping_cohort_name_reference",
                label = "Cohort name reference",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_groupping_cohort_name_comparator",
                label = "Cohort name comparator",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("days_between_cohort_entries", "number records", "number subjects"),
                selected = c("days_between_cohort_entries", "number records", "number subjects"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_cohort_timing_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_cohort_timing_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_timing_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_timing_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_timing_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_cohort_timing_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_header",
                    label = "Header",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "density", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_group",
                    label = "Group",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "density", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = NULL,
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_hide",
                    label = "Hide",
                    choices = c("cdm_name", "cohort_name_comparator", "cohort_name_reference", "density", "estimate_name", "restrict_to_first_entry", "variable_level", "variable_name"),
                    selected = c("restrict_to_first_entry", "density"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_cohort_timing_formatted_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Plot cohort timing",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_plot_3_plot_type",
                    label = "plotType",
                    choices = c("boxplot", "density"),
                    selected = c("boxplot"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_plot_3_time_scale",
                    label = "timeScale",
                    choices = c("days", "years"),
                    selected = c("days"),
                    multiple = FALSE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_plot_3_facet",
                    label = "facet",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry", "density"),
                    selected = c("cdm_name", "cohort_name_reference"),
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_cohort_timing_plot_3_colour",
                    label = "colour",
                    choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry", "density"),
                    selected = c("cohort_name_comparator"),
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_cohort_timing_plot_3_unique_combinations",
                    label = "uniqueCombinations",
                    value = c(FALSE)
                  ),
                  shiny::downloadButton(outputId = "summarise_cohort_timing_plot_3_download", label = "Download"),
                  shiny::plotOutput(outputId = "summarise_cohort_timing_plot_3") |>
                    shinycssloaders::withSpinner()
                )
              )
            ),
            ## summarise_large_scale_characteristics ----
            shinydashboard::tabItem(
              tabName = "summarise_large_scale_characteristics",
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_settings_table_name",
                label = "Table name",
                choices = c("condition_occurrence", "drug_exposure"),
                selected = c("condition_occurrence", "drug_exposure"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_settings_type",
                label = "Type",
                choices = c("episode", "event"),
                selected = c("episode", "event"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_settings_analysis",
                label = "Analysis",
                choices = c("standard"),
                selected = c("standard"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Groupping"),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_groupping_cdm_name",
                label = "Cdm name",
                choices = c("mock database"),
                selected = c("mock database"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_groupping_cohort_name",
                label = "Cohort name",
                choices = c("asthma", "covid", "tb"),
                selected = c("asthma", "covid", "tb"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_groupping_sex",
                label = "Sex",
                choices = c("Female", "Male", "overall"),
                selected = c("Female", "Male", "overall"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_groupping_age_group",
                label = "Age group",
                choices = c("0 to 44", "45 or above", "overall"),
                selected = c("0 to 44", "45 or above", "overall"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_groupping_concept_id",
                label = "Concept id",
                choices = c("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "3", "4", "5", "6", "7", "8"),
                selected = c("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "3", "4", "5", "6", "7", "8"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shiny::h4("Variables and estimates"),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_variables_and_estimates_variable_name",
                label = "Variable name",
                choices = c("ALIMENTARY TRACT AND METABOLISM", "Adalimumab", "Arthritis", "Arthropathies", "Degenerative arthropathy", "Descendant drug", "Diseases of the musculoskeletal system and connective tissue", "Injectable", "Injection", "Knee osteoarthritis", "Musculoskeletal disorder", "OA", "Osteoarthritis of hip", "Osteoarthritis of knee", "Osteoarthrosis", "Osteonecrosis", "Other ingredient"),
                selected = c("ALIMENTARY TRACT AND METABOLISM", "Adalimumab", "Arthritis", "Arthropathies", "Degenerative arthropathy", "Descendant drug", "Diseases of the musculoskeletal system and connective tissue", "Injectable", "Injection", "Knee osteoarthritis", "Musculoskeletal disorder", "OA", "Osteoarthritis of hip", "Osteoarthritis of knee", "Osteoarthrosis", "Osteonecrosis", "Other ingredient"),
                width = "160px",
                multiple = TRUE,
                inline = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = "summarise_large_scale_characteristics_variables_and_estimates_estimate_name",
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
                    inputId = "summarise_large_scale_characteristics_show_groupping",
                    label = "Show groupping",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_large_scale_characteristics_show_settings",
                    label = "Show settings",
                    value = FALSE
                  ),
                  shiny::checkboxInput(
                    inputId = "summarise_large_scale_characteristics_pivot_estimates",
                    label = "Pivot estimates",
                    value = FALSE
                  ),
                  shiny::downloadButton(outputId = "summarise_large_scale_characteristics_raw_download", label = "Download as csv"),
                  DT::DTOutput(outputId = "summarise_large_scale_characteristics_raw_table") |>
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel(
                  title = "Formatted table",
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_header",
                    label = "Header",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = "cdm_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_group",
                    label = "Group",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = "cohort_name",
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "summarise_large_scale_characteristics_hide",
                    label = "Hide",
                    choices = c("age_group", "analysis", "cdm_name", "cohort_name", "concept_id", "estimate_name", "sex", "table_name", "type", "variable_level", "variable_name"),
                    selected = c("table_name", "type", "analysis"),
                    width = "160px",
                    multiple = TRUE,
                    inline = TRUE
                  ),
                  shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download as word"),
                  gt::gt_output(outputId = "summarise_large_scale_characteristics_formatted_table") |>
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
        # summarise_characteristics ----
        getRawDataSummariseCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_characteristics", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_characteristics_show_settings,
              showGroupping = input$summarise_characteristics_show_groupping,
              pivotEstimates = input$summarise_characteristics_pivot_estimates
            )
        })
        output$summarise_characteristics_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseCharacteristics(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseCharacteristics <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_characteristics", input)
          header <- input$summarise_characteristics_header
          group <- input$summarise_characteristics_group
          hide <- input$summarise_characteristics_hide
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
        output$summarise_characteristics_formatted_table <- gt::render_gt({
          getFormattedDataSummariseCharacteristics()
        })
        output$summarise_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_characteristics.docx",
          content = function(file) {
            getFormattedDataSummariseCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummariseCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_characteristics", input)
        })
      
        createPlot4 <- shiny::reactive({
          result <- getPlotDataSummariseCharacteristics()
          CohortCharacteristics::plotCharacteristics(
            result,
            plotStyle = input$summarise_characteristics_plot_4_plot_style,
            facet = input$summarise_characteristics_plot_4_facet,
            colour = input$summarise_characteristics_plot_4_colour
          )
        })
        output$summarise_characteristics_plot_4 <- shiny::renderPlot({
          createPlot4()
        })
        output$summarise_characteristics_plot_4_download <- shiny::downloadHandler(
          filename = "plot_summarise_characteristics.png",
          content = function(file) {
            plt <- createPlot4()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # summarise_cohort_attrition ----
        getRawDataSummariseCohortAttrition <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_attrition", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_attrition_show_settings,
              showGroupping = input$summarise_cohort_attrition_show_groupping,
              pivotEstimates = input$summarise_cohort_attrition_pivot_estimates
            )
        })
        output$summarise_cohort_attrition_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortAttrition(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseCohortAttrition <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_cohort_attrition", input)
          header <- input$summarise_cohort_attrition_header
          group <- input$summarise_cohort_attrition_group
          hide <- input$summarise_cohort_attrition_hide
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
        output$summarise_cohort_attrition_formatted_table <- gt::render_gt({
          getFormattedDataSummariseCohortAttrition()
        })
        output$summarise_cohort_attrition_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_cohort_attrition.docx",
          content = function(file) {
            getFormattedDataSummariseCohortAttrition() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummariseCohortAttrition <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_attrition", input)
        })
      
        createPlot2 <- shiny::reactive({
          result <- getPlotDataSummariseCohortAttrition()
          CohortCharacteristics::plotCohortAttrition(
            result
          )
        })
        output$summarise_cohort_attrition_plot_2 <- DiagrammeR::renderGrViz({
          createPlot2()
        })
        output$summarise_cohort_attrition_plot_2_download <- shiny::downloadHandler(
          filename = "plot_summarise_cohort_attrition.png",
          content = function(file) {
            plt <- createPlot2()
            DiagrammeR::export_graph(graph = plt, file_name = file, fily_type = "png", width = 800)
          }
        )
        # summarise_cohort_count ----
        getRawDataSummariseCohortCount <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_count", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_count_show_settings,
              showGroupping = input$summarise_cohort_count_show_groupping,
              pivotEstimates = input$summarise_cohort_count_pivot_estimates
            )
        })
        output$summarise_cohort_count_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortCount(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseCohortCount <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_cohort_count", input)
          header <- input$summarise_cohort_count_header
          group <- input$summarise_cohort_count_group
          hide <- input$summarise_cohort_count_hide
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
        output$summarise_cohort_count_formatted_table <- gt::render_gt({
          getFormattedDataSummariseCohortCount()
        })
        output$summarise_cohort_count_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_cohort_count.docx",
          content = function(file) {
            getFormattedDataSummariseCohortCount() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummariseCohortCount <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_count", input)
        })
      
        createPlot5 <- shiny::reactive({
          result <- getPlotDataSummariseCohortCount()
          CohortCharacteristics::plotCohortCount(
            result,
            facet = input$summarise_cohort_count_plot_5_facet,
            colour = input$summarise_cohort_count_plot_5_colour
          )
        })
        output$summarise_cohort_count_plot_5 <- shiny::renderPlot({
          createPlot5()
        })
        output$summarise_cohort_count_plot_5_download <- shiny::downloadHandler(
          filename = "plot_summarise_cohort_count.png",
          content = function(file) {
            plt <- createPlot5()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # summarise_cohort_overlap ----
        getRawDataSummariseCohortOverlap <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_overlap", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_overlap_show_settings,
              showGroupping = input$summarise_cohort_overlap_show_groupping,
              pivotEstimates = input$summarise_cohort_overlap_pivot_estimates
            )
        })
        output$summarise_cohort_overlap_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortOverlap(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseCohortOverlap <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_cohort_overlap", input)
          header <- input$summarise_cohort_overlap_header
          group <- input$summarise_cohort_overlap_group
          hide <- input$summarise_cohort_overlap_hide
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
        output$summarise_cohort_overlap_formatted_table <- gt::render_gt({
          getFormattedDataSummariseCohortOverlap()
        })
        output$summarise_cohort_overlap_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_cohort_overlap.docx",
          content = function(file) {
            getFormattedDataSummariseCohortOverlap() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummariseCohortOverlap <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_overlap", input)
        })
      
        createPlot1 <- shiny::reactive({
          result <- getPlotDataSummariseCohortOverlap()
          CohortCharacteristics::plotCohortOverlap(
            result,
            facet = input$summarise_cohort_overlap_plot_1_facet,
            uniqueCombinations = input$summarise_cohort_overlap_plot_1_unique_combinations
          )
        })
        output$summarise_cohort_overlap_plot_1 <- shiny::renderPlot({
          createPlot1()
        })
        output$summarise_cohort_overlap_plot_1_download <- shiny::downloadHandler(
          filename = "plot_summarise_cohort_overlap.png",
          content = function(file) {
            plt <- createPlot1()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # summarise_cohort_timing ----
        getRawDataSummariseCohortTiming <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_timing", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_timing_show_settings,
              showGroupping = input$summarise_cohort_timing_show_groupping,
              pivotEstimates = input$summarise_cohort_timing_pivot_estimates
            )
        })
        output$summarise_cohort_timing_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortTiming(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseCohortTiming <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_cohort_timing", input)
          header <- input$summarise_cohort_timing_header
          group <- input$summarise_cohort_timing_group
          hide <- input$summarise_cohort_timing_hide
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
        output$summarise_cohort_timing_formatted_table <- gt::render_gt({
          getFormattedDataSummariseCohortTiming()
        })
        output$summarise_cohort_timing_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_cohort_timing.docx",
          content = function(file) {
            getFormattedDataSummariseCohortTiming() |>
              gt::gtsave(filename = file)
          }
        )
        getPlotDataSummariseCohortTiming <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_timing", input)
        })
      
        createPlot3 <- shiny::reactive({
          result <- getPlotDataSummariseCohortTiming()
          CohortCharacteristics::plotCohortTiming(
            result,
            plotType = input$summarise_cohort_timing_plot_3_plot_type,
            timeScale = input$summarise_cohort_timing_plot_3_time_scale,
            facet = input$summarise_cohort_timing_plot_3_facet,
            colour = input$summarise_cohort_timing_plot_3_colour,
            uniqueCombinations = input$summarise_cohort_timing_plot_3_unique_combinations
          )
        })
        output$summarise_cohort_timing_plot_3 <- shiny::renderPlot({
          createPlot3()
        })
        output$summarise_cohort_timing_plot_3_download <- shiny::downloadHandler(
          filename = "plot_summarise_cohort_timing.png",
          content = function(file) {
            plt <- createPlot3()
            ggplot2::ggsave(filename = file, plot = plt)
          }
        )
        # summarise_large_scale_characteristics ----
        getRawDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_large_scale_characteristics", input) |>
            omopViewer::tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_large_scale_characteristics_show_settings,
              showGroupping = input$summarise_large_scale_characteristics_show_groupping,
              pivotEstimates = input$summarise_large_scale_characteristics_pivot_estimates
            )
        })
        output$summarise_large_scale_characteristics_raw_table <- DT::renderDT({
          DT::datatable(getRawDataSummariseLargeScaleCharacteristics(), options = list(scrollX = TRUE))
        })
        getFormattedDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          x <- data |>
            omopViewer::filterData("summarise_large_scale_characteristics", input)
          header <- input$summarise_large_scale_characteristics_header
          group <- input$summarise_large_scale_characteristics_group
          hide <- input$summarise_large_scale_characteristics_hide
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
        output$summarise_large_scale_characteristics_formatted_table <- gt::render_gt({
          getFormattedDataSummariseLargeScaleCharacteristics()
        })
        output$summarise_large_scale_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "gt_table_summarise_large_scale_characteristics.docx",
          content = function(file) {
            getFormattedDataSummariseLargeScaleCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        # end -----
      }

