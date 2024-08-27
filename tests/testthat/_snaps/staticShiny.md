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
        # end -----
      }

