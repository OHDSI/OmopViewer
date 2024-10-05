# logo

    Code
      cat(uiStatic(logo = "my_pic.png"), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = shiny::tags$span(
          shiny::tags$img(
            src = "my_pic.png",
            width = "auto",
            height = "46px",
            class = "me-3",
            alt = "logo"
          ),
          ""
        ),
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )

# empty shiny

    Code
      cat(uiStatic(), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = "",
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )

---

    Code
      cat(serverStatic(), sep = "\n")
    Output
      server <- function(input, output, session) {
        # download raw data -----
        output$download_raw <- shiny::downloadHandler(
          filename = "results.csv",
          content = function(file) {
            omopViewer::exportSummarisedResult(data, fileName = file)
          }
        )
      }


# background

    Code
      createBackground(full)
    Output
      [1] "bslib::nav_panel(\n  title = \"Background\",\n  icon = shiny::icon(\"disease\"),\n  bslib::card(bslib::card_header(shiny::markdown('Abstract')), bslib::card_title(shiny::markdown('**Introduction**')), shiny::p(shiny::markdown('Example of an [introduction](https://github.com/oxford-pharmacoepi/omopViewer).')), bslib::card_title(shiny::markdown('Methods')), shiny::p(shiny::markdown('Methods example, with a footer* example.')), bslib::card_footer(shiny::markdown('*Here is the footer.')))\n)"
# CohortCharacteristics shiny

    Code
      cat(uiStatic(choices = getChoices(result)), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = "",
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_panel(
          title = "Cohort characteristics",
          icon = shiny::icon("users-gear"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_characteristics_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot characteristics",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_characteristics_plot_4_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_plot_style",
                        label = "plotStyle",
                        choices = c("boxplot", "barplot", "scatterplot"),
                        selected = c("barplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_characteristics_plot_4")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort Attrition",
          icon = shiny::icon("layer-group"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason",
                    label = "Reason",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason_id",
                    label = "Reason id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_attrition_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_attrition_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("reason", "reason_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_attrition_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_attrition_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_attrition_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "cohort_definition_id"),
                          input_id = "summarise_cohort_attrition_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_attrition_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Diagram",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "Width (px)", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "Height (px)", value = 10),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_plot_2_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      position = "right"
                    ),
                    DiagrammeR::grVizOutput("summarise_cohort_attrition_plot_2")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort count",
          icon = shiny::icon("users"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_count_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_count_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_count_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_count_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_count_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_cohort_count_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_count_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort count",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_count_plot_5_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_count_plot_5")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort overlap",
          icon = shiny::icon("circle-half-stroke"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("Cohort overlap shows the number of subjects that contribute to a pair of cohorts.")
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_overlap_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_overlap_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_overlap_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_overlap_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_overlap_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort overlap",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_plot_1_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_plot_1_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_overlap_plot_1")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort timing",
          icon = shiny::icon("chart-simple"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_settings_restrict_to_first_entry",
                    label = "Restrict to first entry",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_timing_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_timing_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_timing_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_timing_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_timing_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("restrict_to_first_entry"),
                          input_id = "summarise_cohort_timing_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_timing_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort timing",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_plot_3_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_plot_type",
                        label = "plotType",
                        choices = c("boxplot", "density"),
                        selected = c("boxplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_time_scale",
                        label = "timeScale",
                        choices = c("days", "years"),
                        selected = c("days"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cohort_name_comparator"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_plot_3_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_timing_plot_3")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Large Scale Characteristics",
          icon = shiny::icon("arrow-up-right-dots"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_sex",
                    label = "Sex",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_age_group",
                    label = "Age group",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_concept_id",
                    label = "Concept id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_large_scale_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_large_scale_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("sex", "age_group", "concept_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "type", "analysis"),
                          input_id = "summarise_large_scale_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_large_scale_characteristics_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )

---

    Code

      createBackground(full, "OHDSI")
    Output
      [1] "bslib::nav_panel(\n  title = \"Background\",\n  icon = shiny::icon(\"disease\"),\n  bslib::card(bslib::card_header(shiny::markdown('Abstract')), bslib::card_title(shiny::markdown('**Introduction**')), shiny::p(shiny::markdown('Example of an [introduction](https://github.com/oxford-pharmacoepi/omopViewer).')), bslib::card_title(shiny::markdown('Methods')), shiny::p(shiny::markdown('Methods example, with a footer* example.')), bslib::card_footer(shiny::markdown('*Here is the footer.')),\nshiny::tags$img(\n  src = \"OHDSI\",\n  width = \"auto\",\n  height = \"100px\",\n  alt = \"logo\",\n  align = \"left\"\n))\n)"

      cat(serverStatic(resultTypes = names(getChoices(result))), sep = "\n")
    Output
      server <- function(input, output, session) {
        # download raw data -----
        output$download_raw <- shiny::downloadHandler(
          filename = "results.csv",
          content = function(file) {
            omopViewer::exportSummarisedResult(data, fileName = file)
          }
        )
        # fill selectise variables ----
        shiny::observe({
          choices <- omopViewer::getChoices(data, flatten = TRUE)
          for (k in seq_along(choices)) {
            shiny::updateSelectizeInput(
              session,
              inputId = names(choices)[k],
              choices = choices[[k]],
              selected = choices[[k]]
            )
          }
        })
        # summarise_characteristics -----
        ## tidy summarise_characteristics -----
        getTidyDataSummariseCharacteristics <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_characteristics", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_characteristics_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_characteristics_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_characteristics_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseCharacteristics(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_characteristics_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_characteristics.csv",
          content = function(file) {
            getTidyDataSummariseCharacteristics() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_characteristics -----
        getFormattedDataSummariseCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_characteristics", input) |>
            omopViewer::visTable(
              header = input$summarise_characteristics_formatted_header,
              group = input$summarise_characteristics_formatted_group,
              hide = input$summarise_characteristics_formatted_hide
            )
        })
        output$summarise_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummariseCharacteristics()
        })
        output$summarise_characteristics_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_characteristics.", input$summarise_characteristics_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_characteristics -----
        createPlot4 <- shiny::reactive({
          result <- data |>
            omopViewer::filterData("summarise_characteristics", input)
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
            ggplot2::ggsave(
              filename = file, plot = plt,
              width = as.numeric(input$summarise_characteristics_plot_4_download_width),
              height = as.numeric(input$summarise_characteristics_plot_4_download_height),
              units = input$summarise_characteristics_plot_4_download_units,
              dpi = as.numeric(input$summarise_characteristics_plot_4_download_dpi)
            )
          }
        )
      
      
        # summarise_cohort_attrition -----
        ## tidy summarise_cohort_attrition -----
        getTidyDataSummariseCohortAttrition <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_cohort_attrition", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_cohort_attrition_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_cohort_attrition_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_cohort_attrition_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseCohortAttrition(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_cohort_attrition_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_cohort_attrition.csv",
          content = function(file) {
            getTidyDataSummariseCohortAttrition() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_cohort_attrition -----
        getFormattedDataSummariseCohortAttrition <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_attrition", input) |>
            omopViewer::visTable(
              header = input$summarise_cohort_attrition_formatted_header,
              group = input$summarise_cohort_attrition_formatted_group,
              hide = input$summarise_cohort_attrition_formatted_hide
            )
        })
        output$summarise_cohort_attrition_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortAttrition()
        })
        output$summarise_cohort_attrition_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_cohort_attrition.", input$summarise_cohort_attrition_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseCohortAttrition() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_attrition -----
        createPlot2 <- shiny::reactive({
          result <- data |>
            omopViewer::filterData("summarise_cohort_attrition", input)
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
            DiagrammeR::export_graph(
              graph = plt, file_name = file, fily_type = "png",
              width = as.numeric(input$summarise_cohort_attrition_plot_2_download_width),
              height = as.numeric(input$summarise_cohort_attrition_plot_2_download_height)
            )
          }
        )
      
      
        # summarise_cohort_count -----
        ## tidy summarise_cohort_count -----
        getTidyDataSummariseCohortCount <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_cohort_count", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_cohort_count_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_cohort_count_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_cohort_count_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseCohortCount(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_cohort_count_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_cohort_count.csv",
          content = function(file) {
            getTidyDataSummariseCohortCount() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_cohort_count -----
        getFormattedDataSummariseCohortCount <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_count", input) |>
            omopViewer::visTable(
              header = input$summarise_cohort_count_formatted_header,
              group = input$summarise_cohort_count_formatted_group,
              hide = input$summarise_cohort_count_formatted_hide
            )
        })
        output$summarise_cohort_count_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortCount()
        })
        output$summarise_cohort_count_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_cohort_count.", input$summarise_cohort_count_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseCohortCount() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_count -----
        createPlot5 <- shiny::reactive({
          result <- data |>
            omopViewer::filterData("summarise_cohort_count", input)
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
            ggplot2::ggsave(
              filename = file, plot = plt,
              width = as.numeric(input$summarise_cohort_count_plot_5_download_width),
              height = as.numeric(input$summarise_cohort_count_plot_5_download_height),
              units = input$summarise_cohort_count_plot_5_download_units,
              dpi = as.numeric(input$summarise_cohort_count_plot_5_download_dpi)
            )
          }
        )
      
      
        # summarise_cohort_overlap -----
        ## tidy summarise_cohort_overlap -----
        getTidyDataSummariseCohortOverlap <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_cohort_overlap", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_cohort_overlap_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_cohort_overlap_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_cohort_overlap_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseCohortOverlap(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_cohort_overlap_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_cohort_overlap.csv",
          content = function(file) {
            getTidyDataSummariseCohortOverlap() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_cohort_overlap -----
        getFormattedDataSummariseCohortOverlap <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_overlap", input) |>
            omopViewer::visTable(
              header = input$summarise_cohort_overlap_formatted_header,
              group = input$summarise_cohort_overlap_formatted_group,
              hide = input$summarise_cohort_overlap_formatted_hide
            )
        })
        output$summarise_cohort_overlap_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortOverlap()
        })
        output$summarise_cohort_overlap_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_cohort_overlap.", input$summarise_cohort_overlap_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseCohortOverlap() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_overlap -----
        createPlot1 <- shiny::reactive({
          result <- data |>
            omopViewer::filterData("summarise_cohort_overlap", input)
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
            ggplot2::ggsave(
              filename = file, plot = plt,
              width = as.numeric(input$summarise_cohort_overlap_plot_1_download_width),
              height = as.numeric(input$summarise_cohort_overlap_plot_1_download_height),
              units = input$summarise_cohort_overlap_plot_1_download_units,
              dpi = as.numeric(input$summarise_cohort_overlap_plot_1_download_dpi)
            )
          }
        )
      
      
        # summarise_cohort_timing -----
        ## tidy summarise_cohort_timing -----
        getTidyDataSummariseCohortTiming <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_cohort_timing", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_cohort_timing_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_cohort_timing_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_cohort_timing_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseCohortTiming(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_cohort_timing_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_cohort_timing.csv",
          content = function(file) {
            getTidyDataSummariseCohortTiming() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_cohort_timing -----
        getFormattedDataSummariseCohortTiming <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_cohort_timing", input) |>
            omopViewer::visTable(
              header = input$summarise_cohort_timing_formatted_header,
              group = input$summarise_cohort_timing_formatted_group,
              hide = input$summarise_cohort_timing_formatted_hide
            )
        })
        output$summarise_cohort_timing_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortTiming()
        })
        output$summarise_cohort_timing_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_cohort_timing.", input$summarise_cohort_timing_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseCohortTiming() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_timing -----
        createPlot3 <- shiny::reactive({
          result <- data |>
            omopViewer::filterData("summarise_cohort_timing", input)
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
            ggplot2::ggsave(
              filename = file, plot = plt,
              width = as.numeric(input$summarise_cohort_timing_plot_3_download_width),
              height = as.numeric(input$summarise_cohort_timing_plot_3_download_height),
              units = input$summarise_cohort_timing_plot_3_download_units,
              dpi = as.numeric(input$summarise_cohort_timing_plot_3_download_dpi)
            )
          }
        )
      
      
        # summarise_large_scale_characteristics -----
        ## tidy summarise_large_scale_characteristics -----
        getTidyDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          res <- data |>
            omopViewer::filterData("summarise_large_scale_characteristics", input) |>
            omopViewer::tidyData()
      
          # columns to eliminate
          colsEliminate <- colnames(res)
          colsEliminate <- colsEliminate[!colsEliminate %in% c(
            input$summarise_large_scale_characteristics_tidy_columns, "variable_name", "variable_level",
            "estimate_name", "estimate_type", "estimate_value"
          )]
      
          # pivot
          pivot <- input$summarise_large_scale_characteristics_tidy_pivot
          if (pivot != "none") {
            vars <- switch(pivot,
              "estimates" = "estimate_name",
              "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
            )
            res <- res |>
              visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
          }
      
          res |>
            dplyr::select(!dplyr::all_of(colsEliminate))
        })
        output$summarise_large_scale_characteristics_tidy <- DT::renderDT({
          DT::datatable(
            getTidyDataSummariseLargeScaleCharacteristics(),
            options = list(scrollX = TRUE),
            rownames = FALSE
          )
        })
        output$summarise_large_scale_characteristics_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarise_large_scale_characteristics.csv",
          content = function(file) {
            getTidyDataSummariseLargeScaleCharacteristics() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarise_large_scale_characteristics -----
        getFormattedDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          data |>
            omopViewer::filterData("summarise_large_scale_characteristics", input) |>
            omopViewer::visTable(
              header = input$summarise_large_scale_characteristics_formatted_header,
              group = input$summarise_large_scale_characteristics_formatted_group,
              hide = input$summarise_large_scale_characteristics_formatted_hide
            )
        })
        output$summarise_large_scale_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummariseLargeScaleCharacteristics()
        })
        output$summarise_large_scale_characteristics_formatted_download <- shiny::downloadHandler(
          filename = function() {
            paste0("formatted_summarise_large_scale_characteristics.", input$summarise_large_scale_characteristics_formatted_download_type)
          },
          content = function(file) {
            getFormattedDataSummariseLargeScaleCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_large_scale_characteristics -----
      }

---

    Code
      cat(x, sep = "\n")
    Output
      # Generated by omopViewer 0.0.0.900
      # Be careful editing this file
      
      library(CohortCharacteristics)
      library(DT)
      library(DiagrammeR)
      library(bslib)
      library(dplyr)
      library(ggplot2)
      library(gt)
      library(here)
      library(omopViewer)
      library(readr)
      library(shiny)
      library(sortable)
      library(visOmopResults)
      
      data <- omopViewer::importSummarisedResult(here::here("data")) |>
        omopViewer::correctSettings()

---

    Code
      cat(uiStatic(choices = getChoices(result), summary = capture.output(summary(
        result), type = "message"), logo = NULL), sep = "\n")
    Message
      A summarised_result object with 33196 rows, 9 different result_id, 1 different cdm names, and 8 settings.
      CDM names: mock database.
      Settings: package_name, package_version, result_type, table_name, cohort_definition_id, restrict_to_first_entry, type, and analysis.
    Output
      ui <- bslib::page_navbar(
        title = "",
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_panel(
          title = "Cohort characteristics",
          icon = shiny::icon("users-gear"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_characteristics_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot characteristics",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_characteristics_plot_4_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_plot_style",
                        label = "plotStyle",
                        choices = c("boxplot", "barplot", "scatterplot"),
                        selected = c("barplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_characteristics_plot_4")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort Attrition",
          icon = shiny::icon("layer-group"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason",
                    label = "Reason",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason_id",
                    label = "Reason id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_attrition_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_attrition_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("reason", "reason_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_attrition_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_attrition_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_attrition_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "cohort_definition_id"),
                          input_id = "summarise_cohort_attrition_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_attrition_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Diagram",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "Width (px)", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "Height (px)", value = 10),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_plot_2_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      position = "right"
                    ),
                    DiagrammeR::grVizOutput("summarise_cohort_attrition_plot_2")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort count",
          icon = shiny::icon("users"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_count_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_count_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_count_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_count_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_count_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_cohort_count_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_count_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort count",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_count_plot_5_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_count_plot_5")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort overlap",
          icon = shiny::icon("circle-half-stroke"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("Cohort overlap shows the number of subjects that contribute to a pair of cohorts.")
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_overlap_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_overlap_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_overlap_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_overlap_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_overlap_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort overlap",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_plot_1_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_plot_1_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_overlap_plot_1")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort timing",
          icon = shiny::icon("chart-simple"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_settings_restrict_to_first_entry",
                    label = "Restrict to first entry",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_timing_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_timing_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_timing_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_timing_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_timing_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("restrict_to_first_entry"),
                          input_id = "summarise_cohort_timing_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_timing_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort timing",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_plot_3_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_plot_type",
                        label = "plotType",
                        choices = c("boxplot", "density"),
                        selected = c("boxplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_time_scale",
                        label = "timeScale",
                        choices = c("days", "years"),
                        selected = c("days"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cohort_name_comparator"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_plot_3_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_timing_plot_3")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Large Scale Characteristics",
          icon = shiny::icon("arrow-up-right-dots"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_sex",
                    label = "Sex",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_age_group",
                    label = "Age group",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_concept_id",
                    label = "Concept id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_large_scale_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_large_scale_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("sex", "age_group", "concept_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "type", "analysis"),
                          input_id = "summarise_large_scale_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_large_scale_characteristics_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )

---

    Code
      cat(uiStatic(choices = getChoices(result), summary = capture.output(summary(
        result), type = "message"), logo = "HDS"), sep = "\n")
    Message
      A summarised_result object with 33196 rows, 9 different result_id, 1 different cdm names, and 8 settings.
      CDM names: mock database.
      Settings: package_name, package_version, result_type, table_name, cohort_definition_id, restrict_to_first_entry, type, and analysis.
    Output
      ui <- bslib::page_navbar(
        title = shiny::tags$span(
          shiny::tags$img(
            src = "HDS",
            width = "auto",
            height = "46px",
            class = "me-3",
            alt = "logo"
          ),
          ""
        ),
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_panel(
          title = "Cohort characteristics",
          icon = shiny::icon("users-gear"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_characteristics_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot characteristics",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_characteristics_plot_4_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_plot_style",
                        label = "plotStyle",
                        choices = c("boxplot", "barplot", "scatterplot"),
                        selected = c("barplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_characteristics_plot_4_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name", "table_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_characteristics_plot_4")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort Attrition",
          icon = shiny::icon("layer-group"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason",
                    label = "Reason",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_grouping_reason_id",
                    label = "Reason id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_attrition_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_attrition_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("reason", "reason_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_attrition_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_attrition_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_attrition_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "cohort_definition_id"),
                          input_id = "summarise_cohort_attrition_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_attrition_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Diagram",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "Width (px)", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "Height (px)", value = 10),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_plot_2_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      position = "right"
                    ),
                    DiagrammeR::grVizOutput("summarise_cohort_attrition_plot_2")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort count",
          icon = shiny::icon("users"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_count_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_count_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_count_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_count_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_cohort_count_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name"),
                          input_id = "summarise_cohort_count_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_count_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort count",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_count_plot_5_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_count_plot_5_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name", "variable_name", "variable_level", "estimate_name"),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_count_plot_5")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort overlap",
          icon = shiny::icon("circle-half-stroke"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("Cohort overlap shows the number of subjects that contribute to a pair of cohorts.")
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_overlap_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_overlap_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_overlap_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_overlap_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = character(),
                          input_id = "summarise_cohort_overlap_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_overlap_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort overlap",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_plot_1_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_overlap_plot_1_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_plot_1_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_overlap_plot_1")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort timing",
          icon = shiny::icon("chart-simple"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_settings_restrict_to_first_entry",
                    label = "Restrict to first entry",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_grouping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_cohort_timing_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_cohort_timing_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_cohort_timing_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_cohort_timing_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "summarise_cohort_timing_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("restrict_to_first_entry"),
                          input_id = "summarise_cohort_timing_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_cohort_timing_formatted")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Plot cohort timing",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "Width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "Height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_dpi", label = "dpi", value = 300),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_plot_3_download", label = "Download png")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_plot_type",
                        label = "plotType",
                        choices = c("boxplot", "density"),
                        selected = c("boxplot"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_time_scale",
                        label = "timeScale",
                        choices = c("days", "years"),
                        selected = c("days"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_facet",
                        label = "facet",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cdm_name", "cohort_name_reference"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_timing_plot_3_colour",
                        label = "colour",
                        choices = c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "variable_name", "variable_level", "estimate_name", "restrict_to_first_entry"),
                        selected = c("cohort_name_comparator"),
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_plot_3_unique_combinations",
                        label = "uniqueCombinations",
                        value = c(TRUE)
                      ),
                      position = "right"
                    ),
                    shiny::plotOutput("summarise_cohort_timing_plot_3")
                  )
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Large Scale Characteristics",
          icon = shiny::icon("arrow-up-right-dots"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Settings",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_table_name",
                    label = "Table name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "grouping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cdm_name",
                    label = "Cdm name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_cohort_name",
                    label = "Cohort name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_sex",
                    label = "Sex",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_age_group",
                    label = "Age group",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_grouping_concept_id",
                    label = "Concept id",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_tidy_columns",
                        label = "Columns",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::radioButtons(
                        inputId = "summarise_large_scale_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarise_large_scale_characteristics_tidy")
                  )
                )
              ),
              bslib::nav_panel(
                title = "Formatted",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::selectizeInput(
                        inputId = "summarise_large_scale_characteristics_formatted_download_type",
                        label = "File",
                        choices = c("docx", "png", "pdf", "html"),
                        selected = c("docx"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      sortable::bucket_list(
                        header = NULL,
                        sortable::add_rank_list(
                          text = "None",
                          labels = c("sex", "age_group", "concept_id", "variable_name", "variable_level", "estimate_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarise_large_scale_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "type", "analysis"),
                          input_id = "summarise_large_scale_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarise_large_scale_characteristics_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )
    

# title

    Code
      cat(x, sep = "\n")
    Output
      # Generated by omopViewer 0.0.0.900
      # Be careful editing this file
      
      ui <- bslib::page_navbar(
        title = shiny::tags$span(
          shiny::tags$img(
            src = "ohdsi_logo.svg",
            width = "auto",
            height = "46px",
            class = "me-3",
            alt = "logo"
          ),
          "example"
        ),
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          omopViewer::cardFromMd("background.md")
        ),
        bslib::nav_panel(
          title = "Summary",
          icon = shiny::icon("file-alt"),
          bslib::card(
            bslib::card_header("Summary of results"),
            shiny::p(shiny::markdown("A summarised_result object with **0** rows, **0** different result_id, different cdm names, and **3** settings.")), shiny::p(shiny::markdown(" - **Settings:** result_type, package_name, and package_version.")),
            shiny::tags$img(
              src = "ohdsi_logo.svg",
              width = "auto",
              height = "100px",
              alt = "logo",
              align = "left"
            )
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("download"),
            shiny::downloadButton(
              outputId = "download_raw",
              label = "Download raw data",
              icon = shiny::icon("download")
            )
          )
        ),
        bslib::nav_item(
          bslib::popover(
            shiny::icon("circle-info"),
            shiny::tags$img(
              src = "hds_logo.svg",
              class = "logo-img",
              alt = "Logo",
              height = "auto",
              width = "30%",
              style = "float:right"
            ),
            "This shiny app was generated with ",
            shiny::a(
              "omopViewer",
              href = "https://github.com/oxford-pharmacoepi/omopViewer",
              target = "_blank"
            ),
            shiny::strong("v0.0.0.900")
          )
        ),
        bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
      )

# order tabs

    Code
      uiStatic(choices = panels$choices)
    Output
        [1] "ui <- bslib::page_navbar("                                                                                                                                    
        [2] "  title = \"\","                                                                                                                                              
        [3] "  bslib::nav_panel("                                                                                                                                          
        [4] "    title = \"Cohort count\","                                                                                                                                
        [5] "    icon = shiny::icon(\"users\"),"                                                                                                                           
        [6] "    bslib::layout_sidebar("                                                                                                                                   
        [7] "      sidebar = bslib::sidebar("                                                                                                                              
        [8] "        bslib::accordion("                                                                                                                                    
        [9] "          bslib::accordion_panel("                                                                                                                            
       [10] "            title = \"Information\","                                                                                                                         
       [11] "            icon = shiny::icon(\"info\"),"                                                                                                                    
       [12] "            shiny::p(\"\")"                                                                                                                                   
       [13] "          ),"                                                                                                                                                 
       [14] "          bslib::accordion_panel("                                                                                                                            
       [15] "            title = \"Settings\","                                                                                                                            
       [16] "            shiny::selectizeInput("                                                                                                                           
       [17] "              inputId = \"summarise_cohort_count_settings_table_name\","                                                                                      
       [18] "              label = \"Table name\","                                                                                                                        
       [19] "              choices = c(\"cohort\"),"                                                                                                                       
       [20] "              selected = c(\"cohort\"),"                                                                                                                      
       [21] "              multiple = TRUE,"                                                                                                                               
       [22] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [23] "            )"                                                                                                                                                
       [24] "          ),"                                                                                                                                                 
       [25] "          bslib::accordion_panel("                                                                                                                            
       [26] "            title = \"grouping\","                                                                                                                            
       [27] "            shiny::selectizeInput("                                                                                                                           
       [28] "              inputId = \"summarise_cohort_count_grouping_cdm_name\","                                                                                        
       [29] "              label = \"Cdm name\","                                                                                                                          
       [30] "              choices = c(\"mock database\"),"                                                                                                                
       [31] "              selected = c(\"mock database\"),"                                                                                                               
       [32] "              multiple = TRUE,"                                                                                                                               
       [33] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [34] "            ),"                                                                                                                                               
       [35] "            shiny::selectizeInput("                                                                                                                           
       [36] "              inputId = \"summarise_cohort_count_grouping_cohort_name\","                                                                                     
       [37] "              label = \"Cohort name\","                                                                                                                       
       [38] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
       [39] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
       [40] "              multiple = TRUE,"                                                                                                                               
       [41] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [42] "            )"                                                                                                                                                
       [43] "          ),"                                                                                                                                                 
       [44] "          bslib::accordion_panel("                                                                                                                            
       [45] "            title = \"Variables\","                                                                                                                           
       [46] "            shiny::selectizeInput("                                                                                                                           
       [47] "              inputId = \"summarise_cohort_count_variable_name\","                                                                                            
       [48] "              label = \"Variable name\","                                                                                                                     
       [49] "              choices = NULL,"                                                                                                                                
       [50] "              selected = NULL,"                                                                                                                               
       [51] "              multiple = TRUE,"                                                                                                                               
       [52] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [53] "            )"                                                                                                                                                
       [54] "          ),"                                                                                                                                                 
       [55] "          bslib::accordion_panel("                                                                                                                            
       [56] "            title = \"Estimates\","                                                                                                                           
       [57] "            shiny::selectizeInput("                                                                                                                           
       [58] "              inputId = \"summarise_cohort_count_estimate_name\","                                                                                            
       [59] "              label = \"Estimate name\","                                                                                                                     
       [60] "              choices = c(\"count\"),"                                                                                                                        
       [61] "              selected = c(\"count\"),"                                                                                                                       
       [62] "              multiple = TRUE,"                                                                                                                               
       [63] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [64] "            )"                                                                                                                                                
       [65] "          )"                                                                                                                                                  
       [66] "        )"                                                                                                                                                    
       [67] "      ),"                                                                                                                                                     
       [68] "      bslib::navset_card_tab("                                                                                                                                
       [69] "        bslib::nav_panel("                                                                                                                                    
       [70] "          title = \"Raw\","                                                                                                                                   
       [71] "          bslib::card("                                                                                                                                       
       [72] "            full_screen = TRUE,"                                                                                                                              
       [73] "            bslib::card_header("                                                                                                                              
       [74] "              bslib::popover("                                                                                                                                
       [75] "                shiny::icon(\"download\"),"                                                                                                                   
       [76] "                shiny::downloadButton(outputId = \"summarise_cohort_count_raw_download\", label = \"Download summarised_result\")"                            
       [77] "              ),"                                                                                                                                             
       [78] "              class = \"text-end\""                                                                                                                           
       [79] "            ),"                                                                                                                                               
       [80] "            DT::dataTableOutput(\"summarise_cohort_count_raw\")"                                                                                              
       [81] "          )"                                                                                                                                                  
       [82] "        ),"                                                                                                                                                   
       [83] "        bslib::nav_panel("                                                                                                                                    
       [84] "          title = \"Tidy\","                                                                                                                                  
       [85] "          bslib::card("                                                                                                                                       
       [86] "            full_screen = TRUE,"                                                                                                                              
       [87] "            bslib::card_header("                                                                                                                              
       [88] "              bslib::popover("                                                                                                                                
       [89] "                shiny::icon(\"download\"),"                                                                                                                   
       [90] "                shiny::downloadButton(outputId = \"summarise_cohort_count_tidy_download\", label = \"Download csv\")"                                         
       [91] "              ),"                                                                                                                                             
       [92] "              class = \"text-end\""                                                                                                                           
       [93] "            ),"                                                                                                                                               
       [94] "            bslib::layout_sidebar("                                                                                                                           
       [95] "              sidebar = bslib::sidebar("                                                                                                                      
       [96] "                shiny::checkboxInput("                                                                                                                        
       [97] "                  inputId = \"summarise_cohort_count_tidy_settings\","                                                                                        
       [98] "                  label = \"Show settings\","                                                                                                                 
       [99] "                  value = FALSE"                                                                                                                              
      [100] "                ),"                                                                                                                                           
      [101] "                shiny::checkboxInput("                                                                                                                        
      [102] "                  inputId = \"summarise_cohort_count_tidy_grouping\","                                                                                        
      [103] "                  label = \"Show grouping\","                                                                                                                 
      [104] "                  value = TRUE"                                                                                                                               
      [105] "                ),"                                                                                                                                           
      [106] "                shiny::radioButtons("                                                                                                                         
      [107] "                  inputId = \"summarise_cohort_count_tidy_pivot\","                                                                                           
      [108] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [109] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [110] "                  selected = \"none\""                                                                                                                        
      [111] "                ),"                                                                                                                                           
      [112] "                position = \"right\""                                                                                                                         
      [113] "              ),"                                                                                                                                             
      [114] "              DT::dataTableOutput(\"summarise_cohort_count_tidy\")"                                                                                           
      [115] "            )"                                                                                                                                                
      [116] "          )"                                                                                                                                                  
      [117] "        ),"                                                                                                                                                   
      [118] "        bslib::nav_panel("                                                                                                                                    
      [119] "          title = \"Formatted\","                                                                                                                             
      [120] "          bslib::card("                                                                                                                                       
      [121] "            full_screen = TRUE,"                                                                                                                              
      [122] "            bslib::card_header("                                                                                                                              
      [123] "              bslib::popover("                                                                                                                                
      [124] "                shiny::icon(\"download\"),"                                                                                                                   
      [125] "                shiny::selectizeInput("                                                                                                                       
      [126] "                  inputId = \"summarise_cohort_count_formatted_download_type\","                                                                              
      [127] "                  label = \"File\","                                                                                                                          
      [128] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [129] "                  selected = c(\"docx\"),"                                                                                                                    
      [130] "                  multiple = FALSE,"                                                                                                                          
      [131] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [132] "                ),"                                                                                                                                           
      [133] "                shiny::downloadButton(outputId = \"summarise_cohort_count_formatted_download\", label = \"Download\")"                                        
      [134] "              ),"                                                                                                                                             
      [135] "              class = \"text-end\""                                                                                                                           
      [136] "            ),"                                                                                                                                               
      [137] "            bslib::layout_sidebar("                                                                                                                           
      [138] "              sidebar = bslib::sidebar("                                                                                                                      
      [139] "                sortable::bucket_list("                                                                                                                       
      [140] "                  header = NULL,"                                                                                                                             
      [141] "                  sortable::add_rank_list("                                                                                                                   
      [142] "                    text = \"None\","                                                                                                                         
      [143] "                    labels = c(\"variable_name\", \"variable_level\", \"estimate_name\"),"                                                                    
      [144] "                    input_id = \"summarise_cohort_count_formatted_none\""                                                                                     
      [145] "                  ),"                                                                                                                                         
      [146] "                  sortable::add_rank_list("                                                                                                                   
      [147] "                    text = \"Header\","                                                                                                                       
      [148] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [149] "                    input_id = \"summarise_cohort_count_formatted_header\""                                                                                   
      [150] "                  ),"                                                                                                                                         
      [151] "                  sortable::add_rank_list("                                                                                                                   
      [152] "                    text = \"Group\","                                                                                                                        
      [153] "                    labels = c(\"cohort_name\"),"                                                                                                             
      [154] "                    input_id = \"summarise_cohort_count_formatted_group\""                                                                                    
      [155] "                  ),"                                                                                                                                         
      [156] "                  sortable::add_rank_list("                                                                                                                   
      [157] "                    text = \"Hide\","                                                                                                                         
      [158] "                    labels = c(\"table_name\"),"                                                                                                              
      [159] "                    input_id = \"summarise_cohort_count_formatted_hide\""                                                                                     
      [160] "                  )"                                                                                                                                          
      [161] "                ),"                                                                                                                                           
      [162] "                position = \"right\""                                                                                                                         
      [163] "              ),"                                                                                                                                             
      [164] "              gt::gt_output(\"summarise_cohort_count_formatted\")"                                                                                            
      [165] "            )"                                                                                                                                                
      [166] "          )"                                                                                                                                                  
      [167] "        ),"                                                                                                                                                   
      [168] "        bslib::nav_panel("                                                                                                                                    
      [169] "          title = \"Plot cohort count\","                                                                                                                     
      [170] "          bslib::card("                                                                                                                                       
      [171] "            full_screen = TRUE,"                                                                                                                              
      [172] "            bslib::card_header("                                                                                                                              
      [173] "              bslib::popover("                                                                                                                                
      [174] "                shiny::icon(\"download\"),"                                                                                                                   
      [175] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_width\", label = \"Width\", value = 15),"                              
      [176] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_height\", label = \"Height\", value = 10),"                            
      [177] "                shiny::selectizeInput("                                                                                                                       
      [178] "                  inputId = \"summarise_cohort_count_plot_5_download_units\","                                                                                
      [179] "                  label = \"Units\","                                                                                                                         
      [180] "                  choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                     
      [181] "                  selected = c(\"cm\"),"                                                                                                                      
      [182] "                  multiple = FALSE,"                                                                                                                          
      [183] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [184] "                ),"                                                                                                                                           
      [185] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_dpi\", label = \"dpi\", value = 300),"                                 
      [186] "                shiny::downloadButton(outputId = \"summarise_cohort_count_plot_5_download\", label = \"Download png\")"                                       
      [187] "              ),"                                                                                                                                             
      [188] "              class = \"text-end\""                                                                                                                           
      [189] "            ),"                                                                                                                                               
      [190] "            bslib::layout_sidebar("                                                                                                                           
      [191] "              sidebar = bslib::sidebar("                                                                                                                      
      [192] "                shiny::selectizeInput("                                                                                                                       
      [193] "                  inputId = \"summarise_cohort_count_plot_5_facet\","                                                                                         
      [194] "                  label = \"facet\","                                                                                                                         
      [195] "                  choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                      
      [196] "                  selected = c(\"cdm_name\"),"                                                                                                                
      [197] "                  multiple = TRUE,"                                                                                                                           
      [198] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [199] "                ),"                                                                                                                                           
      [200] "                shiny::selectizeInput("                                                                                                                       
      [201] "                  inputId = \"summarise_cohort_count_plot_5_colour\","                                                                                        
      [202] "                  label = \"colour\","                                                                                                                        
      [203] "                  choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                      
      [204] "                  selected = NULL,"                                                                                                                           
      [205] "                  multiple = TRUE,"                                                                                                                           
      [206] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [207] "                ),"                                                                                                                                           
      [208] "                position = \"right\""                                                                                                                         
      [209] "              ),"                                                                                                                                             
      [210] "              shiny::plotOutput(\"summarise_cohort_count_plot_5\")"                                                                                           
      [211] "            )"                                                                                                                                                
      [212] "          )"                                                                                                                                                  
      [213] "        )"                                                                                                                                                    
      [214] "      )"                                                                                                                                                      
      [215] "    )"                                                                                                                                                        
      [216] "  ),"                                                                                                                                                         
      [217] "  bslib::nav_panel("                                                                                                                                          
      [218] "    title = \"Cohort Attrition\","                                                                                                                            
      [219] "    icon = shiny::icon(\"layer-group\"),"                                                                                                                     
      [220] "    bslib::layout_sidebar("                                                                                                                                   
      [221] "      sidebar = bslib::sidebar("                                                                                                                              
      [222] "        bslib::accordion("                                                                                                                                    
      [223] "          bslib::accordion_panel("                                                                                                                            
      [224] "            title = \"Information\","                                                                                                                         
      [225] "            icon = shiny::icon(\"info\"),"                                                                                                                    
      [226] "            shiny::p(\"\")"                                                                                                                                   
      [227] "          ),"                                                                                                                                                 
      [228] "          bslib::accordion_panel("                                                                                                                            
      [229] "            title = \"Settings\","                                                                                                                            
      [230] "            shiny::selectizeInput("                                                                                                                           
      [231] "              inputId = \"summarise_cohort_attrition_settings_cohort_definition_id\","                                                                        
      [232] "              label = \"Cohort definition id\","                                                                                                              
      [233] "              choices = c(1, 2, 3),"                                                                                                                          
      [234] "              selected = c(1, 2, 3),"                                                                                                                         
      [235] "              multiple = TRUE,"                                                                                                                               
      [236] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [237] "            ),"                                                                                                                                               
      [238] "            shiny::selectizeInput("                                                                                                                           
      [239] "              inputId = \"summarise_cohort_attrition_settings_table_name\","                                                                                  
      [240] "              label = \"Table name\","                                                                                                                        
      [241] "              choices = c(\"cohort\"),"                                                                                                                       
      [242] "              selected = c(\"cohort\"),"                                                                                                                      
      [243] "              multiple = TRUE,"                                                                                                                               
      [244] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [245] "            )"                                                                                                                                                
      [246] "          ),"                                                                                                                                                 
      [247] "          bslib::accordion_panel("                                                                                                                            
      [248] "            title = \"grouping\","                                                                                                                            
      [249] "            shiny::selectizeInput("                                                                                                                           
      [250] "              inputId = \"summarise_cohort_attrition_grouping_cdm_name\","                                                                                    
      [251] "              label = \"Cdm name\","                                                                                                                          
      [252] "              choices = c(\"mock database\"),"                                                                                                                
      [253] "              selected = c(\"mock database\"),"                                                                                                               
      [254] "              multiple = TRUE,"                                                                                                                               
      [255] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [256] "            ),"                                                                                                                                               
      [257] "            shiny::selectizeInput("                                                                                                                           
      [258] "              inputId = \"summarise_cohort_attrition_grouping_cohort_name\","                                                                                 
      [259] "              label = \"Cohort name\","                                                                                                                       
      [260] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [261] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [262] "              multiple = TRUE,"                                                                                                                               
      [263] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [264] "            ),"                                                                                                                                               
      [265] "            shiny::selectizeInput("                                                                                                                           
      [266] "              inputId = \"summarise_cohort_attrition_grouping_reason\","                                                                                      
      [267] "              label = \"Reason\","                                                                                                                            
      [268] "              choices = c(\"Initial qualifying events\"),"                                                                                                    
      [269] "              selected = c(\"Initial qualifying events\"),"                                                                                                   
      [270] "              multiple = TRUE,"                                                                                                                               
      [271] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [272] "            ),"                                                                                                                                               
      [273] "            shiny::selectizeInput("                                                                                                                           
      [274] "              inputId = \"summarise_cohort_attrition_grouping_reason_id\","                                                                                   
      [275] "              label = \"Reason id\","                                                                                                                         
      [276] "              choices = c(\"1\"),"                                                                                                                            
      [277] "              selected = c(\"1\"),"                                                                                                                           
      [278] "              multiple = TRUE,"                                                                                                                               
      [279] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [280] "            )"                                                                                                                                                
      [281] "          ),"                                                                                                                                                 
      [282] "          bslib::accordion_panel("                                                                                                                            
      [283] "            title = \"Variables\","                                                                                                                           
      [284] "            shiny::selectizeInput("                                                                                                                           
      [285] "              inputId = \"summarise_cohort_attrition_variable_name\","                                                                                        
      [286] "              label = \"Variable name\","                                                                                                                     
      [287] "              choices = NULL,"                                                                                                                                
      [288] "              selected = NULL,"                                                                                                                               
      [289] "              multiple = TRUE,"                                                                                                                               
      [290] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [291] "            )"                                                                                                                                                
      [292] "          ),"                                                                                                                                                 
      [293] "          bslib::accordion_panel("                                                                                                                            
      [294] "            title = \"Estimates\","                                                                                                                           
      [295] "            shiny::selectizeInput("                                                                                                                           
      [296] "              inputId = \"summarise_cohort_attrition_estimate_name\","                                                                                        
      [297] "              label = \"Estimate name\","                                                                                                                     
      [298] "              choices = c(\"count\"),"                                                                                                                        
      [299] "              selected = c(\"count\"),"                                                                                                                       
      [300] "              multiple = TRUE,"                                                                                                                               
      [301] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [302] "            )"                                                                                                                                                
      [303] "          )"                                                                                                                                                  
      [304] "        )"                                                                                                                                                    
      [305] "      ),"                                                                                                                                                     
      [306] "      bslib::navset_card_tab("                                                                                                                                
      [307] "        bslib::nav_panel("                                                                                                                                    
      [308] "          title = \"Raw\","                                                                                                                                   
      [309] "          bslib::card("                                                                                                                                       
      [310] "            full_screen = TRUE,"                                                                                                                              
      [311] "            bslib::card_header("                                                                                                                              
      [312] "              bslib::popover("                                                                                                                                
      [313] "                shiny::icon(\"download\"),"                                                                                                                   
      [314] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_raw_download\", label = \"Download summarised_result\")"                        
      [315] "              ),"                                                                                                                                             
      [316] "              class = \"text-end\""                                                                                                                           
      [317] "            ),"                                                                                                                                               
      [318] "            DT::dataTableOutput(\"summarise_cohort_attrition_raw\")"                                                                                          
      [319] "          )"                                                                                                                                                  
      [320] "        ),"                                                                                                                                                   
      [321] "        bslib::nav_panel("                                                                                                                                    
      [322] "          title = \"Tidy\","                                                                                                                                  
      [323] "          bslib::card("                                                                                                                                       
      [324] "            full_screen = TRUE,"                                                                                                                              
      [325] "            bslib::card_header("                                                                                                                              
      [326] "              bslib::popover("                                                                                                                                
      [327] "                shiny::icon(\"download\"),"                                                                                                                   
      [328] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_tidy_download\", label = \"Download csv\")"                                     
      [329] "              ),"                                                                                                                                             
      [330] "              class = \"text-end\""                                                                                                                           
      [331] "            ),"                                                                                                                                               
      [332] "            bslib::layout_sidebar("                                                                                                                           
      [333] "              sidebar = bslib::sidebar("                                                                                                                      
      [334] "                shiny::checkboxInput("                                                                                                                        
      [335] "                  inputId = \"summarise_cohort_attrition_tidy_settings\","                                                                                    
      [336] "                  label = \"Show settings\","                                                                                                                 
      [337] "                  value = FALSE"                                                                                                                              
      [338] "                ),"                                                                                                                                           
      [339] "                shiny::checkboxInput("                                                                                                                        
      [340] "                  inputId = \"summarise_cohort_attrition_tidy_grouping\","                                                                                    
      [341] "                  label = \"Show grouping\","                                                                                                                 
      [342] "                  value = TRUE"                                                                                                                               
      [343] "                ),"                                                                                                                                           
      [344] "                shiny::radioButtons("                                                                                                                         
      [345] "                  inputId = \"summarise_cohort_attrition_tidy_pivot\","                                                                                       
      [346] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [347] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [348] "                  selected = \"none\""                                                                                                                        
      [349] "                ),"                                                                                                                                           
      [350] "                position = \"right\""                                                                                                                         
      [351] "              ),"                                                                                                                                             
      [352] "              DT::dataTableOutput(\"summarise_cohort_attrition_tidy\")"                                                                                       
      [353] "            )"                                                                                                                                                
      [354] "          )"                                                                                                                                                  
      [355] "        ),"                                                                                                                                                   
      [356] "        bslib::nav_panel("                                                                                                                                    
      [357] "          title = \"Formatted\","                                                                                                                             
      [358] "          bslib::card("                                                                                                                                       
      [359] "            full_screen = TRUE,"                                                                                                                              
      [360] "            bslib::card_header("                                                                                                                              
      [361] "              bslib::popover("                                                                                                                                
      [362] "                shiny::icon(\"download\"),"                                                                                                                   
      [363] "                shiny::selectizeInput("                                                                                                                       
      [364] "                  inputId = \"summarise_cohort_attrition_formatted_download_type\","                                                                          
      [365] "                  label = \"File\","                                                                                                                          
      [366] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [367] "                  selected = c(\"docx\"),"                                                                                                                    
      [368] "                  multiple = FALSE,"                                                                                                                          
      [369] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [370] "                ),"                                                                                                                                           
      [371] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_formatted_download\", label = \"Download\")"                                    
      [372] "              ),"                                                                                                                                             
      [373] "              class = \"text-end\""                                                                                                                           
      [374] "            ),"                                                                                                                                               
      [375] "            bslib::layout_sidebar("                                                                                                                           
      [376] "              sidebar = bslib::sidebar("                                                                                                                      
      [377] "                sortable::bucket_list("                                                                                                                       
      [378] "                  header = NULL,"                                                                                                                             
      [379] "                  sortable::add_rank_list("                                                                                                                   
      [380] "                    text = \"None\","                                                                                                                         
      [381] "                    labels = c(\"reason\", \"reason_id\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                         
      [382] "                    input_id = \"summarise_cohort_attrition_formatted_none\""                                                                                 
      [383] "                  ),"                                                                                                                                         
      [384] "                  sortable::add_rank_list("                                                                                                                   
      [385] "                    text = \"Header\","                                                                                                                       
      [386] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [387] "                    input_id = \"summarise_cohort_attrition_formatted_header\""                                                                               
      [388] "                  ),"                                                                                                                                         
      [389] "                  sortable::add_rank_list("                                                                                                                   
      [390] "                    text = \"Group\","                                                                                                                        
      [391] "                    labels = c(\"cohort_name\"),"                                                                                                             
      [392] "                    input_id = \"summarise_cohort_attrition_formatted_group\""                                                                                
      [393] "                  ),"                                                                                                                                         
      [394] "                  sortable::add_rank_list("                                                                                                                   
      [395] "                    text = \"Hide\","                                                                                                                         
      [396] "                    labels = c(\"cohort_definition_id\", \"table_name\"),"                                                                                    
      [397] "                    input_id = \"summarise_cohort_attrition_formatted_hide\""                                                                                 
      [398] "                  )"                                                                                                                                          
      [399] "                ),"                                                                                                                                           
      [400] "                position = \"right\""                                                                                                                         
      [401] "              ),"                                                                                                                                             
      [402] "              gt::gt_output(\"summarise_cohort_attrition_formatted\")"                                                                                        
      [403] "            )"                                                                                                                                                
      [404] "          )"                                                                                                                                                  
      [405] "        ),"                                                                                                                                                   
      [406] "        bslib::nav_panel("                                                                                                                                    
      [407] "          title = \"Diagram\","                                                                                                                               
      [408] "          bslib::card("                                                                                                                                       
      [409] "            full_screen = TRUE,"                                                                                                                              
      [410] "            bslib::card_header("                                                                                                                              
      [411] "              bslib::popover("                                                                                                                                
      [412] "                shiny::icon(\"download\"),"                                                                                                                   
      [413] "                shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_width\", label = \"Width (px)\", value = 15),"                     
      [414] "                shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_height\", label = \"Height (px)\", value = 10),"                   
      [415] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_plot_2_download\", label = \"Download png\")"                                   
      [416] "              ),"                                                                                                                                             
      [417] "              class = \"text-end\""                                                                                                                           
      [418] "            ),"                                                                                                                                               
      [419] "            bslib::layout_sidebar("                                                                                                                           
      [420] "              sidebar = bslib::sidebar("                                                                                                                      
      [421] "                position = \"right\""                                                                                                                         
      [422] "              ),"                                                                                                                                             
      [423] "              DiagrammeR::grVizOutput(\"summarise_cohort_attrition_plot_2\")"                                                                                 
      [424] "            )"                                                                                                                                                
      [425] "          )"                                                                                                                                                  
      [426] "        )"                                                                                                                                                    
      [427] "      )"                                                                                                                                                      
      [428] "    )"                                                                                                                                                        
      [429] "  ),"                                                                                                                                                         
      [430] "  bslib::nav_panel("                                                                                                                                          
      [431] "    title = \"Cohort overlap\","                                                                                                                              
      [432] "    icon = shiny::icon(\"circle-half-stroke\"),"                                                                                                              
      [433] "    bslib::layout_sidebar("                                                                                                                                   
      [434] "      sidebar = bslib::sidebar("                                                                                                                              
      [435] "        bslib::accordion("                                                                                                                                    
      [436] "          bslib::accordion_panel("                                                                                                                            
      [437] "            title = \"Information\","                                                                                                                         
      [438] "            icon = shiny::icon(\"info\"),"                                                                                                                    
      [439] "            shiny::p(\"Cohort overlap shows the number of subjects that contribute to a pair of cohorts.\")"                                                  
      [440] "          ),"                                                                                                                                                 
      [441] "          bslib::accordion_panel("                                                                                                                            
      [442] "            title = \"grouping\","                                                                                                                            
      [443] "            shiny::selectizeInput("                                                                                                                           
      [444] "              inputId = \"summarise_cohort_overlap_grouping_cdm_name\","                                                                                      
      [445] "              label = \"Cdm name\","                                                                                                                          
      [446] "              choices = c(\"mock database\"),"                                                                                                                
      [447] "              selected = c(\"mock database\"),"                                                                                                               
      [448] "              multiple = TRUE,"                                                                                                                               
      [449] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [450] "            ),"                                                                                                                                               
      [451] "            shiny::selectizeInput("                                                                                                                           
      [452] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_reference\","                                                                         
      [453] "              label = \"Cohort name reference\","                                                                                                             
      [454] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [455] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [456] "              multiple = TRUE,"                                                                                                                               
      [457] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [458] "            ),"                                                                                                                                               
      [459] "            shiny::selectizeInput("                                                                                                                           
      [460] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_comparator\","                                                                        
      [461] "              label = \"Cohort name comparator\","                                                                                                            
      [462] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [463] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [464] "              multiple = TRUE,"                                                                                                                               
      [465] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [466] "            )"                                                                                                                                                
      [467] "          ),"                                                                                                                                                 
      [468] "          bslib::accordion_panel("                                                                                                                            
      [469] "            title = \"Variables\","                                                                                                                           
      [470] "            shiny::selectizeInput("                                                                                                                           
      [471] "              inputId = \"summarise_cohort_overlap_variable_name\","                                                                                          
      [472] "              label = \"Variable name\","                                                                                                                     
      [473] "              choices = NULL,"                                                                                                                                
      [474] "              selected = NULL,"                                                                                                                               
      [475] "              multiple = TRUE,"                                                                                                                               
      [476] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [477] "            )"                                                                                                                                                
      [478] "          ),"                                                                                                                                                 
      [479] "          bslib::accordion_panel("                                                                                                                            
      [480] "            title = \"Estimates\","                                                                                                                           
      [481] "            shiny::selectizeInput("                                                                                                                           
      [482] "              inputId = \"summarise_cohort_overlap_estimate_name\","                                                                                          
      [483] "              label = \"Estimate name\","                                                                                                                     
      [484] "              choices = c(\"count\", \"percentage\"),"                                                                                                        
      [485] "              selected = c(\"count\", \"percentage\"),"                                                                                                       
      [486] "              multiple = TRUE,"                                                                                                                               
      [487] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [488] "            )"                                                                                                                                                
      [489] "          )"                                                                                                                                                  
      [490] "        )"                                                                                                                                                    
      [491] "      ),"                                                                                                                                                     
      [492] "      bslib::navset_card_tab("                                                                                                                                
      [493] "        bslib::nav_panel("                                                                                                                                    
      [494] "          title = \"Raw\","                                                                                                                                   
      [495] "          bslib::card("                                                                                                                                       
      [496] "            full_screen = TRUE,"                                                                                                                              
      [497] "            bslib::card_header("                                                                                                                              
      [498] "              bslib::popover("                                                                                                                                
      [499] "                shiny::icon(\"download\"),"                                                                                                                   
      [500] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_raw_download\", label = \"Download summarised_result\")"                          
      [501] "              ),"                                                                                                                                             
      [502] "              class = \"text-end\""                                                                                                                           
      [503] "            ),"                                                                                                                                               
      [504] "            DT::dataTableOutput(\"summarise_cohort_overlap_raw\")"                                                                                            
      [505] "          )"                                                                                                                                                  
      [506] "        ),"                                                                                                                                                   
      [507] "        bslib::nav_panel("                                                                                                                                    
      [508] "          title = \"Tidy\","                                                                                                                                  
      [509] "          bslib::card("                                                                                                                                       
      [510] "            full_screen = TRUE,"                                                                                                                              
      [511] "            bslib::card_header("                                                                                                                              
      [512] "              bslib::popover("                                                                                                                                
      [513] "                shiny::icon(\"download\"),"                                                                                                                   
      [514] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_tidy_download\", label = \"Download csv\")"                                       
      [515] "              ),"                                                                                                                                             
      [516] "              class = \"text-end\""                                                                                                                           
      [517] "            ),"                                                                                                                                               
      [518] "            bslib::layout_sidebar("                                                                                                                           
      [519] "              sidebar = bslib::sidebar("                                                                                                                      
      [520] "                shiny::checkboxInput("                                                                                                                        
      [521] "                  inputId = \"summarise_cohort_overlap_tidy_settings\","                                                                                      
      [522] "                  label = \"Show settings\","                                                                                                                 
      [523] "                  value = FALSE"                                                                                                                              
      [524] "                ),"                                                                                                                                           
      [525] "                shiny::checkboxInput("                                                                                                                        
      [526] "                  inputId = \"summarise_cohort_overlap_tidy_grouping\","                                                                                      
      [527] "                  label = \"Show grouping\","                                                                                                                 
      [528] "                  value = TRUE"                                                                                                                               
      [529] "                ),"                                                                                                                                           
      [530] "                shiny::radioButtons("                                                                                                                         
      [531] "                  inputId = \"summarise_cohort_overlap_tidy_pivot\","                                                                                         
      [532] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [533] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [534] "                  selected = \"none\""                                                                                                                        
      [535] "                ),"                                                                                                                                           
      [536] "                position = \"right\""                                                                                                                         
      [537] "              ),"                                                                                                                                             
      [538] "              DT::dataTableOutput(\"summarise_cohort_overlap_tidy\")"                                                                                         
      [539] "            )"                                                                                                                                                
      [540] "          )"                                                                                                                                                  
      [541] "        ),"                                                                                                                                                   
      [542] "        bslib::nav_panel("                                                                                                                                    
      [543] "          title = \"Formatted\","                                                                                                                             
      [544] "          bslib::card("                                                                                                                                       
      [545] "            full_screen = TRUE,"                                                                                                                              
      [546] "            bslib::card_header("                                                                                                                              
      [547] "              bslib::popover("                                                                                                                                
      [548] "                shiny::icon(\"download\"),"                                                                                                                   
      [549] "                shiny::selectizeInput("                                                                                                                       
      [550] "                  inputId = \"summarise_cohort_overlap_formatted_download_type\","                                                                            
      [551] "                  label = \"File\","                                                                                                                          
      [552] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [553] "                  selected = c(\"docx\"),"                                                                                                                    
      [554] "                  multiple = FALSE,"                                                                                                                          
      [555] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [556] "                ),"                                                                                                                                           
      [557] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_formatted_download\", label = \"Download\")"                                      
      [558] "              ),"                                                                                                                                             
      [559] "              class = \"text-end\""                                                                                                                           
      [560] "            ),"                                                                                                                                               
      [561] "            bslib::layout_sidebar("                                                                                                                           
      [562] "              sidebar = bslib::sidebar("                                                                                                                      
      [563] "                sortable::bucket_list("                                                                                                                       
      [564] "                  header = NULL,"                                                                                                                             
      [565] "                  sortable::add_rank_list("                                                                                                                   
      [566] "                    text = \"None\","                                                                                                                         
      [567] "                    labels = c(\"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"             
      [568] "                    input_id = \"summarise_cohort_overlap_formatted_none\""                                                                                   
      [569] "                  ),"                                                                                                                                         
      [570] "                  sortable::add_rank_list("                                                                                                                   
      [571] "                    text = \"Header\","                                                                                                                       
      [572] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [573] "                    input_id = \"summarise_cohort_overlap_formatted_header\""                                                                                 
      [574] "                  ),"                                                                                                                                         
      [575] "                  sortable::add_rank_list("                                                                                                                   
      [576] "                    text = \"Group\","                                                                                                                        
      [577] "                    labels = character(),"                                                                                                                    
      [578] "                    input_id = \"summarise_cohort_overlap_formatted_group\""                                                                                  
      [579] "                  ),"                                                                                                                                         
      [580] "                  sortable::add_rank_list("                                                                                                                   
      [581] "                    text = \"Hide\","                                                                                                                         
      [582] "                    labels = character(),"                                                                                                                    
      [583] "                    input_id = \"summarise_cohort_overlap_formatted_hide\""                                                                                   
      [584] "                  )"                                                                                                                                          
      [585] "                ),"                                                                                                                                           
      [586] "                position = \"right\""                                                                                                                         
      [587] "              ),"                                                                                                                                             
      [588] "              gt::gt_output(\"summarise_cohort_overlap_formatted\")"                                                                                          
      [589] "            )"                                                                                                                                                
      [590] "          )"                                                                                                                                                  
      [591] "        ),"                                                                                                                                                   
      [592] "        bslib::nav_panel("                                                                                                                                    
      [593] "          title = \"Plot cohort overlap\","                                                                                                                   
      [594] "          bslib::card("                                                                                                                                       
      [595] "            full_screen = TRUE,"                                                                                                                              
      [596] "            bslib::card_header("                                                                                                                              
      [597] "              bslib::popover("                                                                                                                                
      [598] "                shiny::icon(\"download\"),"                                                                                                                   
      [599] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_width\", label = \"Width\", value = 15),"                            
      [600] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_height\", label = \"Height\", value = 10),"                          
      [601] "                shiny::selectizeInput("                                                                                                                       
      [602] "                  inputId = \"summarise_cohort_overlap_plot_1_download_units\","                                                                              
      [603] "                  label = \"Units\","                                                                                                                         
      [604] "                  choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                     
      [605] "                  selected = c(\"cm\"),"                                                                                                                      
      [606] "                  multiple = FALSE,"                                                                                                                          
      [607] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [608] "                ),"                                                                                                                                           
      [609] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_dpi\", label = \"dpi\", value = 300),"                               
      [610] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_plot_1_download\", label = \"Download png\")"                                     
      [611] "              ),"                                                                                                                                             
      [612] "              class = \"text-end\""                                                                                                                           
      [613] "            ),"                                                                                                                                               
      [614] "            bslib::layout_sidebar("                                                                                                                           
      [615] "              sidebar = bslib::sidebar("                                                                                                                      
      [616] "                shiny::selectizeInput("                                                                                                                       
      [617] "                  inputId = \"summarise_cohort_overlap_plot_1_facet\","                                                                                       
      [618] "                  label = \"facet\","                                                                                                                         
      [619] "                  choices = c(\"cdm_name\", \"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"
      [620] "                  selected = c(\"cdm_name\", \"cohort_name_reference\"),"                                                                                     
      [621] "                  multiple = TRUE,"                                                                                                                           
      [622] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [623] "                ),"                                                                                                                                           
      [624] "                shiny::checkboxInput("                                                                                                                        
      [625] "                  inputId = \"summarise_cohort_overlap_plot_1_unique_combinations\","                                                                         
      [626] "                  label = \"uniqueCombinations\","                                                                                                            
      [627] "                  value = c(TRUE)"                                                                                                                            
      [628] "                ),"                                                                                                                                           
      [629] "                position = \"right\""                                                                                                                         
      [630] "              ),"                                                                                                                                             
      [631] "              shiny::plotOutput(\"summarise_cohort_overlap_plot_1\")"                                                                                         
      [632] "            )"                                                                                                                                                
      [633] "          )"                                                                                                                                                  
      [634] "        )"                                                                                                                                                    
      [635] "      )"                                                                                                                                                      
      [636] "    )"                                                                                                                                                        
      [637] "  ),"                                                                                                                                                         
      [638] "  bslib::nav_spacer(),"                                                                                                                                       
      [639] "  bslib::nav_item("                                                                                                                                           
      [640] "    bslib::popover("                                                                                                                                          
      [641] "      shiny::icon(\"circle-info\"),"                                                                                                                          
      [642] "      shiny::tags$img("                                                                                                                                       
      [643] "        src = \"hds_logo.svg\","                                                                                                                              
      [644] "        class = \"logo-img\","                                                                                                                                
      [645] "        alt = \"Logo\","                                                                                                                                      
      [646] "        height = \"auto\","                                                                                                                                   
      [647] "        width = \"30%\","                                                                                                                                     
      [648] "        style = \"float:right\""                                                                                                                              
      [649] "      ),"                                                                                                                                                     
      [650] "      \"This shiny app was generated with \","                                                                                                                
      [651] "      shiny::a("                                                                                                                                              
      [652] "        \"omopViewer\","                                                                                                                                      
      [653] "        href = \"https://github.com/oxford-pharmacoepi/omopViewer\","                                                                                         
      [654] "        target = \"_blank\""                                                                                                                                  
      [655] "      ),"                                                                                                                                                     
      [656] "      shiny::strong(\"v0.0.0.900\")"                                                                                                                          
      [657] "    )"                                                                                                                                                        
      [658] "  ),"                                                                                                                                                         
      [659] "  bslib::nav_item(bslib::input_dark_mode(id = \"dark_mode\", mode = \"light\"))"                                                                              
      [660] ")"                                                                                                                                                            

---

    Code
      uiStatic(choices = panels$choices)
    Output
        [1] "ui <- bslib::page_navbar("                                                                                                                                    
        [2] "  title = \"\","                                                                                                                                              
        [3] "  bslib::nav_panel("                                                                                                                                          
        [4] "    title = \"Cohort overlap\","                                                                                                                              
        [5] "    icon = shiny::icon(\"circle-half-stroke\"),"                                                                                                              
        [6] "    bslib::layout_sidebar("                                                                                                                                   
        [7] "      sidebar = bslib::sidebar("                                                                                                                              
        [8] "        bslib::accordion("                                                                                                                                    
        [9] "          bslib::accordion_panel("                                                                                                                            
       [10] "            title = \"Information\","                                                                                                                         
       [11] "            icon = shiny::icon(\"info\"),"                                                                                                                    
       [12] "            shiny::p(\"Cohort overlap shows the number of subjects that contribute to a pair of cohorts.\")"                                                  
       [13] "          ),"                                                                                                                                                 
       [14] "          bslib::accordion_panel("                                                                                                                            
       [15] "            title = \"grouping\","                                                                                                                            
       [16] "            shiny::selectizeInput("                                                                                                                           
       [17] "              inputId = \"summarise_cohort_overlap_grouping_cdm_name\","                                                                                      
       [18] "              label = \"Cdm name\","                                                                                                                          
       [19] "              choices = c(\"mock database\"),"                                                                                                                
       [20] "              selected = c(\"mock database\"),"                                                                                                               
       [21] "              multiple = TRUE,"                                                                                                                               
       [22] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [23] "            ),"                                                                                                                                               
       [24] "            shiny::selectizeInput("                                                                                                                           
       [25] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_reference\","                                                                         
       [26] "              label = \"Cohort name reference\","                                                                                                             
       [27] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
       [28] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
       [29] "              multiple = TRUE,"                                                                                                                               
       [30] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [31] "            ),"                                                                                                                                               
       [32] "            shiny::selectizeInput("                                                                                                                           
       [33] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_comparator\","                                                                        
       [34] "              label = \"Cohort name comparator\","                                                                                                            
       [35] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
       [36] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
       [37] "              multiple = TRUE,"                                                                                                                               
       [38] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [39] "            )"                                                                                                                                                
       [40] "          ),"                                                                                                                                                 
       [41] "          bslib::accordion_panel("                                                                                                                            
       [42] "            title = \"Variables\","                                                                                                                           
       [43] "            shiny::selectizeInput("                                                                                                                           
       [44] "              inputId = \"summarise_cohort_overlap_variable_name\","                                                                                          
       [45] "              label = \"Variable name\","                                                                                                                     
       [46] "              choices = NULL,"                                                                                                                                
       [47] "              selected = NULL,"                                                                                                                               
       [48] "              multiple = TRUE,"                                                                                                                               
       [49] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [50] "            )"                                                                                                                                                
       [51] "          ),"                                                                                                                                                 
       [52] "          bslib::accordion_panel("                                                                                                                            
       [53] "            title = \"Estimates\","                                                                                                                           
       [54] "            shiny::selectizeInput("                                                                                                                           
       [55] "              inputId = \"summarise_cohort_overlap_estimate_name\","                                                                                          
       [56] "              label = \"Estimate name\","                                                                                                                     
       [57] "              choices = c(\"count\", \"percentage\"),"                                                                                                        
       [58] "              selected = c(\"count\", \"percentage\"),"                                                                                                       
       [59] "              multiple = TRUE,"                                                                                                                               
       [60] "              options = list(plugins = \"remove_button\")"                                                                                                    
       [61] "            )"                                                                                                                                                
       [62] "          )"                                                                                                                                                  
       [63] "        )"                                                                                                                                                    
       [64] "      ),"                                                                                                                                                     
       [65] "      bslib::navset_card_tab("                                                                                                                                
       [66] "        bslib::nav_panel("                                                                                                                                    
       [67] "          title = \"Raw\","                                                                                                                                   
       [68] "          bslib::card("                                                                                                                                       
       [69] "            full_screen = TRUE,"                                                                                                                              
       [70] "            bslib::card_header("                                                                                                                              
       [71] "              bslib::popover("                                                                                                                                
       [72] "                shiny::icon(\"download\"),"                                                                                                                   
       [73] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_raw_download\", label = \"Download summarised_result\")"                          
       [74] "              ),"                                                                                                                                             
       [75] "              class = \"text-end\""                                                                                                                           
       [76] "            ),"                                                                                                                                               
       [77] "            DT::dataTableOutput(\"summarise_cohort_overlap_raw\")"                                                                                            
       [78] "          )"                                                                                                                                                  
       [79] "        ),"                                                                                                                                                   
       [80] "        bslib::nav_panel("                                                                                                                                    
       [81] "          title = \"Tidy\","                                                                                                                                  
       [82] "          bslib::card("                                                                                                                                       
       [83] "            full_screen = TRUE,"                                                                                                                              
       [84] "            bslib::card_header("                                                                                                                              
       [85] "              bslib::popover("                                                                                                                                
       [86] "                shiny::icon(\"download\"),"                                                                                                                   
       [87] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_tidy_download\", label = \"Download csv\")"                                       
       [88] "              ),"                                                                                                                                             
       [89] "              class = \"text-end\""                                                                                                                           
       [90] "            ),"                                                                                                                                               
       [91] "            bslib::layout_sidebar("                                                                                                                           
       [92] "              sidebar = bslib::sidebar("                                                                                                                      
       [93] "                shiny::checkboxInput("                                                                                                                        
       [94] "                  inputId = \"summarise_cohort_overlap_tidy_settings\","                                                                                      
       [95] "                  label = \"Show settings\","                                                                                                                 
       [96] "                  value = FALSE"                                                                                                                              
       [97] "                ),"                                                                                                                                           
       [98] "                shiny::checkboxInput("                                                                                                                        
       [99] "                  inputId = \"summarise_cohort_overlap_tidy_grouping\","                                                                                      
      [100] "                  label = \"Show grouping\","                                                                                                                 
      [101] "                  value = TRUE"                                                                                                                               
      [102] "                ),"                                                                                                                                           
      [103] "                shiny::radioButtons("                                                                                                                         
      [104] "                  inputId = \"summarise_cohort_overlap_tidy_pivot\","                                                                                         
      [105] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [106] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [107] "                  selected = \"none\""                                                                                                                        
      [108] "                ),"                                                                                                                                           
      [109] "                position = \"right\""                                                                                                                         
      [110] "              ),"                                                                                                                                             
      [111] "              DT::dataTableOutput(\"summarise_cohort_overlap_tidy\")"                                                                                         
      [112] "            )"                                                                                                                                                
      [113] "          )"                                                                                                                                                  
      [114] "        ),"                                                                                                                                                   
      [115] "        bslib::nav_panel("                                                                                                                                    
      [116] "          title = \"Formatted\","                                                                                                                             
      [117] "          bslib::card("                                                                                                                                       
      [118] "            full_screen = TRUE,"                                                                                                                              
      [119] "            bslib::card_header("                                                                                                                              
      [120] "              bslib::popover("                                                                                                                                
      [121] "                shiny::icon(\"download\"),"                                                                                                                   
      [122] "                shiny::selectizeInput("                                                                                                                       
      [123] "                  inputId = \"summarise_cohort_overlap_formatted_download_type\","                                                                            
      [124] "                  label = \"File\","                                                                                                                          
      [125] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [126] "                  selected = c(\"docx\"),"                                                                                                                    
      [127] "                  multiple = FALSE,"                                                                                                                          
      [128] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [129] "                ),"                                                                                                                                           
      [130] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_formatted_download\", label = \"Download\")"                                      
      [131] "              ),"                                                                                                                                             
      [132] "              class = \"text-end\""                                                                                                                           
      [133] "            ),"                                                                                                                                               
      [134] "            bslib::layout_sidebar("                                                                                                                           
      [135] "              sidebar = bslib::sidebar("                                                                                                                      
      [136] "                sortable::bucket_list("                                                                                                                       
      [137] "                  header = NULL,"                                                                                                                             
      [138] "                  sortable::add_rank_list("                                                                                                                   
      [139] "                    text = \"None\","                                                                                                                         
      [140] "                    labels = c(\"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"             
      [141] "                    input_id = \"summarise_cohort_overlap_formatted_none\""                                                                                   
      [142] "                  ),"                                                                                                                                         
      [143] "                  sortable::add_rank_list("                                                                                                                   
      [144] "                    text = \"Header\","                                                                                                                       
      [145] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [146] "                    input_id = \"summarise_cohort_overlap_formatted_header\""                                                                                 
      [147] "                  ),"                                                                                                                                         
      [148] "                  sortable::add_rank_list("                                                                                                                   
      [149] "                    text = \"Group\","                                                                                                                        
      [150] "                    labels = character(),"                                                                                                                    
      [151] "                    input_id = \"summarise_cohort_overlap_formatted_group\""                                                                                  
      [152] "                  ),"                                                                                                                                         
      [153] "                  sortable::add_rank_list("                                                                                                                   
      [154] "                    text = \"Hide\","                                                                                                                         
      [155] "                    labels = character(),"                                                                                                                    
      [156] "                    input_id = \"summarise_cohort_overlap_formatted_hide\""                                                                                   
      [157] "                  )"                                                                                                                                          
      [158] "                ),"                                                                                                                                           
      [159] "                position = \"right\""                                                                                                                         
      [160] "              ),"                                                                                                                                             
      [161] "              gt::gt_output(\"summarise_cohort_overlap_formatted\")"                                                                                          
      [162] "            )"                                                                                                                                                
      [163] "          )"                                                                                                                                                  
      [164] "        ),"                                                                                                                                                   
      [165] "        bslib::nav_panel("                                                                                                                                    
      [166] "          title = \"Plot cohort overlap\","                                                                                                                   
      [167] "          bslib::card("                                                                                                                                       
      [168] "            full_screen = TRUE,"                                                                                                                              
      [169] "            bslib::card_header("                                                                                                                              
      [170] "              bslib::popover("                                                                                                                                
      [171] "                shiny::icon(\"download\"),"                                                                                                                   
      [172] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_width\", label = \"Width\", value = 15),"                            
      [173] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_height\", label = \"Height\", value = 10),"                          
      [174] "                shiny::selectizeInput("                                                                                                                       
      [175] "                  inputId = \"summarise_cohort_overlap_plot_1_download_units\","                                                                              
      [176] "                  label = \"Units\","                                                                                                                         
      [177] "                  choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                     
      [178] "                  selected = c(\"cm\"),"                                                                                                                      
      [179] "                  multiple = FALSE,"                                                                                                                          
      [180] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [181] "                ),"                                                                                                                                           
      [182] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_dpi\", label = \"dpi\", value = 300),"                               
      [183] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_plot_1_download\", label = \"Download png\")"                                     
      [184] "              ),"                                                                                                                                             
      [185] "              class = \"text-end\""                                                                                                                           
      [186] "            ),"                                                                                                                                               
      [187] "            bslib::layout_sidebar("                                                                                                                           
      [188] "              sidebar = bslib::sidebar("                                                                                                                      
      [189] "                shiny::selectizeInput("                                                                                                                       
      [190] "                  inputId = \"summarise_cohort_overlap_plot_1_facet\","                                                                                       
      [191] "                  label = \"facet\","                                                                                                                         
      [192] "                  choices = c(\"cdm_name\", \"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"
      [193] "                  selected = c(\"cdm_name\", \"cohort_name_reference\"),"                                                                                     
      [194] "                  multiple = TRUE,"                                                                                                                           
      [195] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [196] "                ),"                                                                                                                                           
      [197] "                shiny::checkboxInput("                                                                                                                        
      [198] "                  inputId = \"summarise_cohort_overlap_plot_1_unique_combinations\","                                                                         
      [199] "                  label = \"uniqueCombinations\","                                                                                                            
      [200] "                  value = c(TRUE)"                                                                                                                            
      [201] "                ),"                                                                                                                                           
      [202] "                position = \"right\""                                                                                                                         
      [203] "              ),"                                                                                                                                             
      [204] "              shiny::plotOutput(\"summarise_cohort_overlap_plot_1\")"                                                                                         
      [205] "            )"                                                                                                                                                
      [206] "          )"                                                                                                                                                  
      [207] "        )"                                                                                                                                                    
      [208] "      )"                                                                                                                                                      
      [209] "    )"                                                                                                                                                        
      [210] "  ),"                                                                                                                                                         
      [211] "  bslib::nav_panel("                                                                                                                                          
      [212] "    title = \"Cohort Attrition\","                                                                                                                            
      [213] "    icon = shiny::icon(\"layer-group\"),"                                                                                                                     
      [214] "    bslib::layout_sidebar("                                                                                                                                   
      [215] "      sidebar = bslib::sidebar("                                                                                                                              
      [216] "        bslib::accordion("                                                                                                                                    
      [217] "          bslib::accordion_panel("                                                                                                                            
      [218] "            title = \"Information\","                                                                                                                         
      [219] "            icon = shiny::icon(\"info\"),"                                                                                                                    
      [220] "            shiny::p(\"\")"                                                                                                                                   
      [221] "          ),"                                                                                                                                                 
      [222] "          bslib::accordion_panel("                                                                                                                            
      [223] "            title = \"Settings\","                                                                                                                            
      [224] "            shiny::selectizeInput("                                                                                                                           
      [225] "              inputId = \"summarise_cohort_attrition_settings_cohort_definition_id\","                                                                        
      [226] "              label = \"Cohort definition id\","                                                                                                              
      [227] "              choices = c(1, 2, 3),"                                                                                                                          
      [228] "              selected = c(1, 2, 3),"                                                                                                                         
      [229] "              multiple = TRUE,"                                                                                                                               
      [230] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [231] "            ),"                                                                                                                                               
      [232] "            shiny::selectizeInput("                                                                                                                           
      [233] "              inputId = \"summarise_cohort_attrition_settings_table_name\","                                                                                  
      [234] "              label = \"Table name\","                                                                                                                        
      [235] "              choices = c(\"cohort\"),"                                                                                                                       
      [236] "              selected = c(\"cohort\"),"                                                                                                                      
      [237] "              multiple = TRUE,"                                                                                                                               
      [238] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [239] "            )"                                                                                                                                                
      [240] "          ),"                                                                                                                                                 
      [241] "          bslib::accordion_panel("                                                                                                                            
      [242] "            title = \"grouping\","                                                                                                                            
      [243] "            shiny::selectizeInput("                                                                                                                           
      [244] "              inputId = \"summarise_cohort_attrition_grouping_cdm_name\","                                                                                    
      [245] "              label = \"Cdm name\","                                                                                                                          
      [246] "              choices = c(\"mock database\"),"                                                                                                                
      [247] "              selected = c(\"mock database\"),"                                                                                                               
      [248] "              multiple = TRUE,"                                                                                                                               
      [249] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [250] "            ),"                                                                                                                                               
      [251] "            shiny::selectizeInput("                                                                                                                           
      [252] "              inputId = \"summarise_cohort_attrition_grouping_cohort_name\","                                                                                 
      [253] "              label = \"Cohort name\","                                                                                                                       
      [254] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [255] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [256] "              multiple = TRUE,"                                                                                                                               
      [257] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [258] "            ),"                                                                                                                                               
      [259] "            shiny::selectizeInput("                                                                                                                           
      [260] "              inputId = \"summarise_cohort_attrition_grouping_reason\","                                                                                      
      [261] "              label = \"Reason\","                                                                                                                            
      [262] "              choices = c(\"Initial qualifying events\"),"                                                                                                    
      [263] "              selected = c(\"Initial qualifying events\"),"                                                                                                   
      [264] "              multiple = TRUE,"                                                                                                                               
      [265] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [266] "            ),"                                                                                                                                               
      [267] "            shiny::selectizeInput("                                                                                                                           
      [268] "              inputId = \"summarise_cohort_attrition_grouping_reason_id\","                                                                                   
      [269] "              label = \"Reason id\","                                                                                                                         
      [270] "              choices = c(\"1\"),"                                                                                                                            
      [271] "              selected = c(\"1\"),"                                                                                                                           
      [272] "              multiple = TRUE,"                                                                                                                               
      [273] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [274] "            )"                                                                                                                                                
      [275] "          ),"                                                                                                                                                 
      [276] "          bslib::accordion_panel("                                                                                                                            
      [277] "            title = \"Variables\","                                                                                                                           
      [278] "            shiny::selectizeInput("                                                                                                                           
      [279] "              inputId = \"summarise_cohort_attrition_variable_name\","                                                                                        
      [280] "              label = \"Variable name\","                                                                                                                     
      [281] "              choices = NULL,"                                                                                                                                
      [282] "              selected = NULL,"                                                                                                                               
      [283] "              multiple = TRUE,"                                                                                                                               
      [284] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [285] "            )"                                                                                                                                                
      [286] "          ),"                                                                                                                                                 
      [287] "          bslib::accordion_panel("                                                                                                                            
      [288] "            title = \"Estimates\","                                                                                                                           
      [289] "            shiny::selectizeInput("                                                                                                                           
      [290] "              inputId = \"summarise_cohort_attrition_estimate_name\","                                                                                        
      [291] "              label = \"Estimate name\","                                                                                                                     
      [292] "              choices = c(\"count\"),"                                                                                                                        
      [293] "              selected = c(\"count\"),"                                                                                                                       
      [294] "              multiple = TRUE,"                                                                                                                               
      [295] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [296] "            )"                                                                                                                                                
      [297] "          )"                                                                                                                                                  
      [298] "        )"                                                                                                                                                    
      [299] "      ),"                                                                                                                                                     
      [300] "      bslib::navset_card_tab("                                                                                                                                
      [301] "        bslib::nav_panel("                                                                                                                                    
      [302] "          title = \"Raw\","                                                                                                                                   
      [303] "          bslib::card("                                                                                                                                       
      [304] "            full_screen = TRUE,"                                                                                                                              
      [305] "            bslib::card_header("                                                                                                                              
      [306] "              bslib::popover("                                                                                                                                
      [307] "                shiny::icon(\"download\"),"                                                                                                                   
      [308] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_raw_download\", label = \"Download summarised_result\")"                        
      [309] "              ),"                                                                                                                                             
      [310] "              class = \"text-end\""                                                                                                                           
      [311] "            ),"                                                                                                                                               
      [312] "            DT::dataTableOutput(\"summarise_cohort_attrition_raw\")"                                                                                          
      [313] "          )"                                                                                                                                                  
      [314] "        ),"                                                                                                                                                   
      [315] "        bslib::nav_panel("                                                                                                                                    
      [316] "          title = \"Tidy\","                                                                                                                                  
      [317] "          bslib::card("                                                                                                                                       
      [318] "            full_screen = TRUE,"                                                                                                                              
      [319] "            bslib::card_header("                                                                                                                              
      [320] "              bslib::popover("                                                                                                                                
      [321] "                shiny::icon(\"download\"),"                                                                                                                   
      [322] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_tidy_download\", label = \"Download csv\")"                                     
      [323] "              ),"                                                                                                                                             
      [324] "              class = \"text-end\""                                                                                                                           
      [325] "            ),"                                                                                                                                               
      [326] "            bslib::layout_sidebar("                                                                                                                           
      [327] "              sidebar = bslib::sidebar("                                                                                                                      
      [328] "                shiny::checkboxInput("                                                                                                                        
      [329] "                  inputId = \"summarise_cohort_attrition_tidy_settings\","                                                                                    
      [330] "                  label = \"Show settings\","                                                                                                                 
      [331] "                  value = FALSE"                                                                                                                              
      [332] "                ),"                                                                                                                                           
      [333] "                shiny::checkboxInput("                                                                                                                        
      [334] "                  inputId = \"summarise_cohort_attrition_tidy_grouping\","                                                                                    
      [335] "                  label = \"Show grouping\","                                                                                                                 
      [336] "                  value = TRUE"                                                                                                                               
      [337] "                ),"                                                                                                                                           
      [338] "                shiny::radioButtons("                                                                                                                         
      [339] "                  inputId = \"summarise_cohort_attrition_tidy_pivot\","                                                                                       
      [340] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [341] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [342] "                  selected = \"none\""                                                                                                                        
      [343] "                ),"                                                                                                                                           
      [344] "                position = \"right\""                                                                                                                         
      [345] "              ),"                                                                                                                                             
      [346] "              DT::dataTableOutput(\"summarise_cohort_attrition_tidy\")"                                                                                       
      [347] "            )"                                                                                                                                                
      [348] "          )"                                                                                                                                                  
      [349] "        ),"                                                                                                                                                   
      [350] "        bslib::nav_panel("                                                                                                                                    
      [351] "          title = \"Formatted\","                                                                                                                             
      [352] "          bslib::card("                                                                                                                                       
      [353] "            full_screen = TRUE,"                                                                                                                              
      [354] "            bslib::card_header("                                                                                                                              
      [355] "              bslib::popover("                                                                                                                                
      [356] "                shiny::icon(\"download\"),"                                                                                                                   
      [357] "                shiny::selectizeInput("                                                                                                                       
      [358] "                  inputId = \"summarise_cohort_attrition_formatted_download_type\","                                                                          
      [359] "                  label = \"File\","                                                                                                                          
      [360] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [361] "                  selected = c(\"docx\"),"                                                                                                                    
      [362] "                  multiple = FALSE,"                                                                                                                          
      [363] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [364] "                ),"                                                                                                                                           
      [365] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_formatted_download\", label = \"Download\")"                                    
      [366] "              ),"                                                                                                                                             
      [367] "              class = \"text-end\""                                                                                                                           
      [368] "            ),"                                                                                                                                               
      [369] "            bslib::layout_sidebar("                                                                                                                           
      [370] "              sidebar = bslib::sidebar("                                                                                                                      
      [371] "                sortable::bucket_list("                                                                                                                       
      [372] "                  header = NULL,"                                                                                                                             
      [373] "                  sortable::add_rank_list("                                                                                                                   
      [374] "                    text = \"None\","                                                                                                                         
      [375] "                    labels = c(\"reason\", \"reason_id\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                         
      [376] "                    input_id = \"summarise_cohort_attrition_formatted_none\""                                                                                 
      [377] "                  ),"                                                                                                                                         
      [378] "                  sortable::add_rank_list("                                                                                                                   
      [379] "                    text = \"Header\","                                                                                                                       
      [380] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [381] "                    input_id = \"summarise_cohort_attrition_formatted_header\""                                                                               
      [382] "                  ),"                                                                                                                                         
      [383] "                  sortable::add_rank_list("                                                                                                                   
      [384] "                    text = \"Group\","                                                                                                                        
      [385] "                    labels = c(\"cohort_name\"),"                                                                                                             
      [386] "                    input_id = \"summarise_cohort_attrition_formatted_group\""                                                                                
      [387] "                  ),"                                                                                                                                         
      [388] "                  sortable::add_rank_list("                                                                                                                   
      [389] "                    text = \"Hide\","                                                                                                                         
      [390] "                    labels = c(\"cohort_definition_id\", \"table_name\"),"                                                                                    
      [391] "                    input_id = \"summarise_cohort_attrition_formatted_hide\""                                                                                 
      [392] "                  )"                                                                                                                                          
      [393] "                ),"                                                                                                                                           
      [394] "                position = \"right\""                                                                                                                         
      [395] "              ),"                                                                                                                                             
      [396] "              gt::gt_output(\"summarise_cohort_attrition_formatted\")"                                                                                        
      [397] "            )"                                                                                                                                                
      [398] "          )"                                                                                                                                                  
      [399] "        ),"                                                                                                                                                   
      [400] "        bslib::nav_panel("                                                                                                                                    
      [401] "          title = \"Diagram\","                                                                                                                               
      [402] "          bslib::card("                                                                                                                                       
      [403] "            full_screen = TRUE,"                                                                                                                              
      [404] "            bslib::card_header("                                                                                                                              
      [405] "              bslib::popover("                                                                                                                                
      [406] "                shiny::icon(\"download\"),"                                                                                                                   
      [407] "                shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_width\", label = \"Width (px)\", value = 15),"                     
      [408] "                shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_height\", label = \"Height (px)\", value = 10),"                   
      [409] "                shiny::downloadButton(outputId = \"summarise_cohort_attrition_plot_2_download\", label = \"Download png\")"                                   
      [410] "              ),"                                                                                                                                             
      [411] "              class = \"text-end\""                                                                                                                           
      [412] "            ),"                                                                                                                                               
      [413] "            bslib::layout_sidebar("                                                                                                                           
      [414] "              sidebar = bslib::sidebar("                                                                                                                      
      [415] "                position = \"right\""                                                                                                                         
      [416] "              ),"                                                                                                                                             
      [417] "              DiagrammeR::grVizOutput(\"summarise_cohort_attrition_plot_2\")"                                                                                 
      [418] "            )"                                                                                                                                                
      [419] "          )"                                                                                                                                                  
      [420] "        )"                                                                                                                                                    
      [421] "      )"                                                                                                                                                      
      [422] "    )"                                                                                                                                                        
      [423] "  ),"                                                                                                                                                         
      [424] "  bslib::nav_panel("                                                                                                                                          
      [425] "    title = \"Cohort count\","                                                                                                                                
      [426] "    icon = shiny::icon(\"users\"),"                                                                                                                           
      [427] "    bslib::layout_sidebar("                                                                                                                                   
      [428] "      sidebar = bslib::sidebar("                                                                                                                              
      [429] "        bslib::accordion("                                                                                                                                    
      [430] "          bslib::accordion_panel("                                                                                                                            
      [431] "            title = \"Information\","                                                                                                                         
      [432] "            icon = shiny::icon(\"info\"),"                                                                                                                    
      [433] "            shiny::p(\"\")"                                                                                                                                   
      [434] "          ),"                                                                                                                                                 
      [435] "          bslib::accordion_panel("                                                                                                                            
      [436] "            title = \"Settings\","                                                                                                                            
      [437] "            shiny::selectizeInput("                                                                                                                           
      [438] "              inputId = \"summarise_cohort_count_settings_table_name\","                                                                                      
      [439] "              label = \"Table name\","                                                                                                                        
      [440] "              choices = c(\"cohort\"),"                                                                                                                       
      [441] "              selected = c(\"cohort\"),"                                                                                                                      
      [442] "              multiple = TRUE,"                                                                                                                               
      [443] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [444] "            )"                                                                                                                                                
      [445] "          ),"                                                                                                                                                 
      [446] "          bslib::accordion_panel("                                                                                                                            
      [447] "            title = \"grouping\","                                                                                                                            
      [448] "            shiny::selectizeInput("                                                                                                                           
      [449] "              inputId = \"summarise_cohort_count_grouping_cdm_name\","                                                                                        
      [450] "              label = \"Cdm name\","                                                                                                                          
      [451] "              choices = c(\"mock database\"),"                                                                                                                
      [452] "              selected = c(\"mock database\"),"                                                                                                               
      [453] "              multiple = TRUE,"                                                                                                                               
      [454] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [455] "            ),"                                                                                                                                               
      [456] "            shiny::selectizeInput("                                                                                                                           
      [457] "              inputId = \"summarise_cohort_count_grouping_cohort_name\","                                                                                     
      [458] "              label = \"Cohort name\","                                                                                                                       
      [459] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [460] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [461] "              multiple = TRUE,"                                                                                                                               
      [462] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [463] "            )"                                                                                                                                                
      [464] "          ),"                                                                                                                                                 
      [465] "          bslib::accordion_panel("                                                                                                                            
      [466] "            title = \"Variables\","                                                                                                                           
      [467] "            shiny::selectizeInput("                                                                                                                           
      [468] "              inputId = \"summarise_cohort_count_variable_name\","                                                                                            
      [469] "              label = \"Variable name\","                                                                                                                     
      [470] "              choices = NULL,"                                                                                                                                
      [471] "              selected = NULL,"                                                                                                                               
      [472] "              multiple = TRUE,"                                                                                                                               
      [473] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [474] "            )"                                                                                                                                                
      [475] "          ),"                                                                                                                                                 
      [476] "          bslib::accordion_panel("                                                                                                                            
      [477] "            title = \"Estimates\","                                                                                                                           
      [478] "            shiny::selectizeInput("                                                                                                                           
      [479] "              inputId = \"summarise_cohort_count_estimate_name\","                                                                                            
      [480] "              label = \"Estimate name\","                                                                                                                     
      [481] "              choices = c(\"count\"),"                                                                                                                        
      [482] "              selected = c(\"count\"),"                                                                                                                       
      [483] "              multiple = TRUE,"                                                                                                                               
      [484] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [485] "            )"                                                                                                                                                
      [486] "          )"                                                                                                                                                  
      [487] "        )"                                                                                                                                                    
      [488] "      ),"                                                                                                                                                     
      [489] "      bslib::navset_card_tab("                                                                                                                                
      [490] "        bslib::nav_panel("                                                                                                                                    
      [491] "          title = \"Raw\","                                                                                                                                   
      [492] "          bslib::card("                                                                                                                                       
      [493] "            full_screen = TRUE,"                                                                                                                              
      [494] "            bslib::card_header("                                                                                                                              
      [495] "              bslib::popover("                                                                                                                                
      [496] "                shiny::icon(\"download\"),"                                                                                                                   
      [497] "                shiny::downloadButton(outputId = \"summarise_cohort_count_raw_download\", label = \"Download summarised_result\")"                            
      [498] "              ),"                                                                                                                                             
      [499] "              class = \"text-end\""                                                                                                                           
      [500] "            ),"                                                                                                                                               
      [501] "            DT::dataTableOutput(\"summarise_cohort_count_raw\")"                                                                                              
      [502] "          )"                                                                                                                                                  
      [503] "        ),"                                                                                                                                                   
      [504] "        bslib::nav_panel("                                                                                                                                    
      [505] "          title = \"Tidy\","                                                                                                                                  
      [506] "          bslib::card("                                                                                                                                       
      [507] "            full_screen = TRUE,"                                                                                                                              
      [508] "            bslib::card_header("                                                                                                                              
      [509] "              bslib::popover("                                                                                                                                
      [510] "                shiny::icon(\"download\"),"                                                                                                                   
      [511] "                shiny::downloadButton(outputId = \"summarise_cohort_count_tidy_download\", label = \"Download csv\")"                                         
      [512] "              ),"                                                                                                                                             
      [513] "              class = \"text-end\""                                                                                                                           
      [514] "            ),"                                                                                                                                               
      [515] "            bslib::layout_sidebar("                                                                                                                           
      [516] "              sidebar = bslib::sidebar("                                                                                                                      
      [517] "                shiny::checkboxInput("                                                                                                                        
      [518] "                  inputId = \"summarise_cohort_count_tidy_settings\","                                                                                        
      [519] "                  label = \"Show settings\","                                                                                                                 
      [520] "                  value = FALSE"                                                                                                                              
      [521] "                ),"                                                                                                                                           
      [522] "                shiny::checkboxInput("                                                                                                                        
      [523] "                  inputId = \"summarise_cohort_count_tidy_grouping\","                                                                                        
      [524] "                  label = \"Show grouping\","                                                                                                                 
      [525] "                  value = TRUE"                                                                                                                               
      [526] "                ),"                                                                                                                                           
      [527] "                shiny::radioButtons("                                                                                                                         
      [528] "                  inputId = \"summarise_cohort_count_tidy_pivot\","                                                                                           
      [529] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [530] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [531] "                  selected = \"none\""                                                                                                                        
      [532] "                ),"                                                                                                                                           
      [533] "                position = \"right\""                                                                                                                         
      [534] "              ),"                                                                                                                                             
      [535] "              DT::dataTableOutput(\"summarise_cohort_count_tidy\")"                                                                                           
      [536] "            )"                                                                                                                                                
      [537] "          )"                                                                                                                                                  
      [538] "        ),"                                                                                                                                                   
      [539] "        bslib::nav_panel("                                                                                                                                    
      [540] "          title = \"Formatted\","                                                                                                                             
      [541] "          bslib::card("                                                                                                                                       
      [542] "            full_screen = TRUE,"                                                                                                                              
      [543] "            bslib::card_header("                                                                                                                              
      [544] "              bslib::popover("                                                                                                                                
      [545] "                shiny::icon(\"download\"),"                                                                                                                   
      [546] "                shiny::selectizeInput("                                                                                                                       
      [547] "                  inputId = \"summarise_cohort_count_formatted_download_type\","                                                                              
      [548] "                  label = \"File\","                                                                                                                          
      [549] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [550] "                  selected = c(\"docx\"),"                                                                                                                    
      [551] "                  multiple = FALSE,"                                                                                                                          
      [552] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [553] "                ),"                                                                                                                                           
      [554] "                shiny::downloadButton(outputId = \"summarise_cohort_count_formatted_download\", label = \"Download\")"                                        
      [555] "              ),"                                                                                                                                             
      [556] "              class = \"text-end\""                                                                                                                           
      [557] "            ),"                                                                                                                                               
      [558] "            bslib::layout_sidebar("                                                                                                                           
      [559] "              sidebar = bslib::sidebar("                                                                                                                      
      [560] "                sortable::bucket_list("                                                                                                                       
      [561] "                  header = NULL,"                                                                                                                             
      [562] "                  sortable::add_rank_list("                                                                                                                   
      [563] "                    text = \"None\","                                                                                                                         
      [564] "                    labels = c(\"variable_name\", \"variable_level\", \"estimate_name\"),"                                                                    
      [565] "                    input_id = \"summarise_cohort_count_formatted_none\""                                                                                     
      [566] "                  ),"                                                                                                                                         
      [567] "                  sortable::add_rank_list("                                                                                                                   
      [568] "                    text = \"Header\","                                                                                                                       
      [569] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [570] "                    input_id = \"summarise_cohort_count_formatted_header\""                                                                                   
      [571] "                  ),"                                                                                                                                         
      [572] "                  sortable::add_rank_list("                                                                                                                   
      [573] "                    text = \"Group\","                                                                                                                        
      [574] "                    labels = c(\"cohort_name\"),"                                                                                                             
      [575] "                    input_id = \"summarise_cohort_count_formatted_group\""                                                                                    
      [576] "                  ),"                                                                                                                                         
      [577] "                  sortable::add_rank_list("                                                                                                                   
      [578] "                    text = \"Hide\","                                                                                                                         
      [579] "                    labels = c(\"table_name\"),"                                                                                                              
      [580] "                    input_id = \"summarise_cohort_count_formatted_hide\""                                                                                     
      [581] "                  )"                                                                                                                                          
      [582] "                ),"                                                                                                                                           
      [583] "                position = \"right\""                                                                                                                         
      [584] "              ),"                                                                                                                                             
      [585] "              gt::gt_output(\"summarise_cohort_count_formatted\")"                                                                                            
      [586] "            )"                                                                                                                                                
      [587] "          )"                                                                                                                                                  
      [588] "        ),"                                                                                                                                                   
      [589] "        bslib::nav_panel("                                                                                                                                    
      [590] "          title = \"Plot cohort count\","                                                                                                                     
      [591] "          bslib::card("                                                                                                                                       
      [592] "            full_screen = TRUE,"                                                                                                                              
      [593] "            bslib::card_header("                                                                                                                              
      [594] "              bslib::popover("                                                                                                                                
      [595] "                shiny::icon(\"download\"),"                                                                                                                   
      [596] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_width\", label = \"Width\", value = 15),"                              
      [597] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_height\", label = \"Height\", value = 10),"                            
      [598] "                shiny::selectizeInput("                                                                                                                       
      [599] "                  inputId = \"summarise_cohort_count_plot_5_download_units\","                                                                                
      [600] "                  label = \"Units\","                                                                                                                         
      [601] "                  choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                     
      [602] "                  selected = c(\"cm\"),"                                                                                                                      
      [603] "                  multiple = FALSE,"                                                                                                                          
      [604] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [605] "                ),"                                                                                                                                           
      [606] "                shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_dpi\", label = \"dpi\", value = 300),"                                 
      [607] "                shiny::downloadButton(outputId = \"summarise_cohort_count_plot_5_download\", label = \"Download png\")"                                       
      [608] "              ),"                                                                                                                                             
      [609] "              class = \"text-end\""                                                                                                                           
      [610] "            ),"                                                                                                                                               
      [611] "            bslib::layout_sidebar("                                                                                                                           
      [612] "              sidebar = bslib::sidebar("                                                                                                                      
      [613] "                shiny::selectizeInput("                                                                                                                       
      [614] "                  inputId = \"summarise_cohort_count_plot_5_facet\","                                                                                         
      [615] "                  label = \"facet\","                                                                                                                         
      [616] "                  choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                      
      [617] "                  selected = c(\"cdm_name\"),"                                                                                                                
      [618] "                  multiple = TRUE,"                                                                                                                           
      [619] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [620] "                ),"                                                                                                                                           
      [621] "                shiny::selectizeInput("                                                                                                                       
      [622] "                  inputId = \"summarise_cohort_count_plot_5_colour\","                                                                                        
      [623] "                  label = \"colour\","                                                                                                                        
      [624] "                  choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                      
      [625] "                  selected = NULL,"                                                                                                                           
      [626] "                  multiple = TRUE,"                                                                                                                           
      [627] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [628] "                ),"                                                                                                                                           
      [629] "                position = \"right\""                                                                                                                         
      [630] "              ),"                                                                                                                                             
      [631] "              shiny::plotOutput(\"summarise_cohort_count_plot_5\")"                                                                                           
      [632] "            )"                                                                                                                                                
      [633] "          )"                                                                                                                                                  
      [634] "        )"                                                                                                                                                    
      [635] "      )"                                                                                                                                                      
      [636] "    )"                                                                                                                                                        
      [637] "  ),"                                                                                                                                                         
      [638] "  bslib::nav_spacer(),"                                                                                                                                       
      [639] "  bslib::nav_item("                                                                                                                                           
      [640] "    bslib::popover("                                                                                                                                          
      [641] "      shiny::icon(\"circle-info\"),"                                                                                                                          
      [642] "      shiny::tags$img("                                                                                                                                       
      [643] "        src = \"hds_logo.svg\","                                                                                                                              
      [644] "        class = \"logo-img\","                                                                                                                                
      [645] "        alt = \"Logo\","                                                                                                                                      
      [646] "        height = \"auto\","                                                                                                                                   
      [647] "        width = \"30%\","                                                                                                                                     
      [648] "        style = \"float:right\""                                                                                                                              
      [649] "      ),"                                                                                                                                                     
      [650] "      \"This shiny app was generated with \","                                                                                                                
      [651] "      shiny::a("                                                                                                                                              
      [652] "        \"omopViewer\","                                                                                                                                      
      [653] "        href = \"https://github.com/oxford-pharmacoepi/omopViewer\","                                                                                         
      [654] "        target = \"_blank\""                                                                                                                                  
      [655] "      ),"                                                                                                                                                     
      [656] "      shiny::strong(\"v0.0.0.900\")"                                                                                                                          
      [657] "    )"                                                                                                                                                        
      [658] "  ),"                                                                                                                                                         
      [659] "  bslib::nav_item(bslib::input_dark_mode(id = \"dark_mode\", mode = \"light\"))"                                                                              
      [660] ")"                                                                                                                                                            

---

    Code
      uiStatic(choices = panels$choices)
    Output
        [1] "ui <- bslib::page_navbar("                                                                                                                                    
        [2] "  title = \"\","                                                                                                                                              
        [3] "  bslib::nav_menu("                                                                                                                                           
        [4] "    title = \"DETAILS\","                                                                                                                                     
        [5] "    bslib::nav_panel("                                                                                                                                        
        [6] "      title = \"Cohort count\","                                                                                                                              
        [7] "      icon = shiny::icon(\"users\"),"                                                                                                                         
        [8] "      bslib::layout_sidebar("                                                                                                                                 
        [9] "        sidebar = bslib::sidebar("                                                                                                                            
       [10] "          bslib::accordion("                                                                                                                                  
       [11] "            bslib::accordion_panel("                                                                                                                          
       [12] "              title = \"Information\","                                                                                                                       
       [13] "              icon = shiny::icon(\"info\"),"                                                                                                                  
       [14] "              shiny::p(\"\")"                                                                                                                                 
       [15] "            ),"                                                                                                                                               
       [16] "            bslib::accordion_panel("                                                                                                                          
       [17] "              title = \"Settings\","                                                                                                                          
       [18] "              shiny::selectizeInput("                                                                                                                         
       [19] "                inputId = \"summarise_cohort_count_settings_table_name\","                                                                                    
       [20] "                label = \"Table name\","                                                                                                                      
       [21] "                choices = c(\"cohort\"),"                                                                                                                     
       [22] "                selected = c(\"cohort\"),"                                                                                                                    
       [23] "                multiple = TRUE,"                                                                                                                             
       [24] "                options = list(plugins = \"remove_button\")"                                                                                                  
       [25] "              )"                                                                                                                                              
       [26] "            ),"                                                                                                                                               
       [27] "            bslib::accordion_panel("                                                                                                                          
       [28] "              title = \"grouping\","                                                                                                                          
       [29] "              shiny::selectizeInput("                                                                                                                         
       [30] "                inputId = \"summarise_cohort_count_grouping_cdm_name\","                                                                                      
       [31] "                label = \"Cdm name\","                                                                                                                        
       [32] "                choices = c(\"mock database\"),"                                                                                                              
       [33] "                selected = c(\"mock database\"),"                                                                                                             
       [34] "                multiple = TRUE,"                                                                                                                             
       [35] "                options = list(plugins = \"remove_button\")"                                                                                                  
       [36] "              ),"                                                                                                                                             
       [37] "              shiny::selectizeInput("                                                                                                                         
       [38] "                inputId = \"summarise_cohort_count_grouping_cohort_name\","                                                                                   
       [39] "                label = \"Cohort name\","                                                                                                                     
       [40] "                choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                  
       [41] "                selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                 
       [42] "                multiple = TRUE,"                                                                                                                             
       [43] "                options = list(plugins = \"remove_button\")"                                                                                                  
       [44] "              )"                                                                                                                                              
       [45] "            ),"                                                                                                                                               
       [46] "            bslib::accordion_panel("                                                                                                                          
       [47] "              title = \"Variables\","                                                                                                                         
       [48] "              shiny::selectizeInput("                                                                                                                         
       [49] "                inputId = \"summarise_cohort_count_variable_name\","                                                                                          
       [50] "                label = \"Variable name\","                                                                                                                   
       [51] "                choices = NULL,"                                                                                                                              
       [52] "                selected = NULL,"                                                                                                                             
       [53] "                multiple = TRUE,"                                                                                                                             
       [54] "                options = list(plugins = \"remove_button\")"                                                                                                  
       [55] "              )"                                                                                                                                              
       [56] "            ),"                                                                                                                                               
       [57] "            bslib::accordion_panel("                                                                                                                          
       [58] "              title = \"Estimates\","                                                                                                                         
       [59] "              shiny::selectizeInput("                                                                                                                         
       [60] "                inputId = \"summarise_cohort_count_estimate_name\","                                                                                          
       [61] "                label = \"Estimate name\","                                                                                                                   
       [62] "                choices = c(\"count\"),"                                                                                                                      
       [63] "                selected = c(\"count\"),"                                                                                                                     
       [64] "                multiple = TRUE,"                                                                                                                             
       [65] "                options = list(plugins = \"remove_button\")"                                                                                                  
       [66] "              )"                                                                                                                                              
       [67] "            )"                                                                                                                                                
       [68] "          )"                                                                                                                                                  
       [69] "        ),"                                                                                                                                                   
       [70] "        bslib::navset_card_tab("                                                                                                                              
       [71] "          bslib::nav_panel("                                                                                                                                  
       [72] "            title = \"Raw\","                                                                                                                                 
       [73] "            bslib::card("                                                                                                                                     
       [74] "              full_screen = TRUE,"                                                                                                                            
       [75] "              bslib::card_header("                                                                                                                            
       [76] "                bslib::popover("                                                                                                                              
       [77] "                  shiny::icon(\"download\"),"                                                                                                                 
       [78] "                  shiny::downloadButton(outputId = \"summarise_cohort_count_raw_download\", label = \"Download summarised_result\")"                          
       [79] "                ),"                                                                                                                                           
       [80] "                class = \"text-end\""                                                                                                                         
       [81] "              ),"                                                                                                                                             
       [82] "              DT::dataTableOutput(\"summarise_cohort_count_raw\")"                                                                                            
       [83] "            )"                                                                                                                                                
       [84] "          ),"                                                                                                                                                 
       [85] "          bslib::nav_panel("                                                                                                                                  
       [86] "            title = \"Tidy\","                                                                                                                                
       [87] "            bslib::card("                                                                                                                                     
       [88] "              full_screen = TRUE,"                                                                                                                            
       [89] "              bslib::card_header("                                                                                                                            
       [90] "                bslib::popover("                                                                                                                              
       [91] "                  shiny::icon(\"download\"),"                                                                                                                 
       [92] "                  shiny::downloadButton(outputId = \"summarise_cohort_count_tidy_download\", label = \"Download csv\")"                                       
       [93] "                ),"                                                                                                                                           
       [94] "                class = \"text-end\""                                                                                                                         
       [95] "              ),"                                                                                                                                             
       [96] "              bslib::layout_sidebar("                                                                                                                         
       [97] "                sidebar = bslib::sidebar("                                                                                                                    
       [98] "                  shiny::checkboxInput("                                                                                                                      
       [99] "                    inputId = \"summarise_cohort_count_tidy_settings\","                                                                                      
      [100] "                    label = \"Show settings\","                                                                                                               
      [101] "                    value = FALSE"                                                                                                                            
      [102] "                  ),"                                                                                                                                         
      [103] "                  shiny::checkboxInput("                                                                                                                      
      [104] "                    inputId = \"summarise_cohort_count_tidy_grouping\","                                                                                      
      [105] "                    label = \"Show grouping\","                                                                                                               
      [106] "                    value = TRUE"                                                                                                                             
      [107] "                  ),"                                                                                                                                         
      [108] "                  shiny::radioButtons("                                                                                                                       
      [109] "                    inputId = \"summarise_cohort_count_tidy_pivot\","                                                                                         
      [110] "                    label = \"Pivot estimates/variables\","                                                                                                   
      [111] "                    choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                       
      [112] "                    selected = \"none\""                                                                                                                      
      [113] "                  ),"                                                                                                                                         
      [114] "                  position = \"right\""                                                                                                                       
      [115] "                ),"                                                                                                                                           
      [116] "                DT::dataTableOutput(\"summarise_cohort_count_tidy\")"                                                                                         
      [117] "              )"                                                                                                                                              
      [118] "            )"                                                                                                                                                
      [119] "          ),"                                                                                                                                                 
      [120] "          bslib::nav_panel("                                                                                                                                  
      [121] "            title = \"Formatted\","                                                                                                                           
      [122] "            bslib::card("                                                                                                                                     
      [123] "              full_screen = TRUE,"                                                                                                                            
      [124] "              bslib::card_header("                                                                                                                            
      [125] "                bslib::popover("                                                                                                                              
      [126] "                  shiny::icon(\"download\"),"                                                                                                                 
      [127] "                  shiny::selectizeInput("                                                                                                                     
      [128] "                    inputId = \"summarise_cohort_count_formatted_download_type\","                                                                            
      [129] "                    label = \"File\","                                                                                                                        
      [130] "                    choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                       
      [131] "                    selected = c(\"docx\"),"                                                                                                                  
      [132] "                    multiple = FALSE,"                                                                                                                        
      [133] "                    options = list(plugins = \"remove_button\")"                                                                                              
      [134] "                  ),"                                                                                                                                         
      [135] "                  shiny::downloadButton(outputId = \"summarise_cohort_count_formatted_download\", label = \"Download\")"                                      
      [136] "                ),"                                                                                                                                           
      [137] "                class = \"text-end\""                                                                                                                         
      [138] "              ),"                                                                                                                                             
      [139] "              bslib::layout_sidebar("                                                                                                                         
      [140] "                sidebar = bslib::sidebar("                                                                                                                    
      [141] "                  sortable::bucket_list("                                                                                                                     
      [142] "                    header = NULL,"                                                                                                                           
      [143] "                    sortable::add_rank_list("                                                                                                                 
      [144] "                      text = \"None\","                                                                                                                       
      [145] "                      labels = c(\"variable_name\", \"variable_level\", \"estimate_name\"),"                                                                  
      [146] "                      input_id = \"summarise_cohort_count_formatted_none\""                                                                                   
      [147] "                    ),"                                                                                                                                       
      [148] "                    sortable::add_rank_list("                                                                                                                 
      [149] "                      text = \"Header\","                                                                                                                     
      [150] "                      labels = c(\"cdm_name\"),"                                                                                                              
      [151] "                      input_id = \"summarise_cohort_count_formatted_header\""                                                                                 
      [152] "                    ),"                                                                                                                                       
      [153] "                    sortable::add_rank_list("                                                                                                                 
      [154] "                      text = \"Group\","                                                                                                                      
      [155] "                      labels = c(\"cohort_name\"),"                                                                                                           
      [156] "                      input_id = \"summarise_cohort_count_formatted_group\""                                                                                  
      [157] "                    ),"                                                                                                                                       
      [158] "                    sortable::add_rank_list("                                                                                                                 
      [159] "                      text = \"Hide\","                                                                                                                       
      [160] "                      labels = c(\"table_name\"),"                                                                                                            
      [161] "                      input_id = \"summarise_cohort_count_formatted_hide\""                                                                                   
      [162] "                    )"                                                                                                                                        
      [163] "                  ),"                                                                                                                                         
      [164] "                  position = \"right\""                                                                                                                       
      [165] "                ),"                                                                                                                                           
      [166] "                gt::gt_output(\"summarise_cohort_count_formatted\")"                                                                                          
      [167] "              )"                                                                                                                                              
      [168] "            )"                                                                                                                                                
      [169] "          ),"                                                                                                                                                 
      [170] "          bslib::nav_panel("                                                                                                                                  
      [171] "            title = \"Plot cohort count\","                                                                                                                   
      [172] "            bslib::card("                                                                                                                                     
      [173] "              full_screen = TRUE,"                                                                                                                            
      [174] "              bslib::card_header("                                                                                                                            
      [175] "                bslib::popover("                                                                                                                              
      [176] "                  shiny::icon(\"download\"),"                                                                                                                 
      [177] "                  shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_width\", label = \"Width\", value = 15),"                            
      [178] "                  shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_height\", label = \"Height\", value = 10),"                          
      [179] "                  shiny::selectizeInput("                                                                                                                     
      [180] "                    inputId = \"summarise_cohort_count_plot_5_download_units\","                                                                              
      [181] "                    label = \"Units\","                                                                                                                       
      [182] "                    choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                   
      [183] "                    selected = c(\"cm\"),"                                                                                                                    
      [184] "                    multiple = FALSE,"                                                                                                                        
      [185] "                    options = list(plugins = \"remove_button\")"                                                                                              
      [186] "                  ),"                                                                                                                                         
      [187] "                  shiny::numericInput(inputId = \"summarise_cohort_count_plot_5_download_dpi\", label = \"dpi\", value = 300),"                               
      [188] "                  shiny::downloadButton(outputId = \"summarise_cohort_count_plot_5_download\", label = \"Download png\")"                                     
      [189] "                ),"                                                                                                                                           
      [190] "                class = \"text-end\""                                                                                                                         
      [191] "              ),"                                                                                                                                             
      [192] "              bslib::layout_sidebar("                                                                                                                         
      [193] "                sidebar = bslib::sidebar("                                                                                                                    
      [194] "                  shiny::selectizeInput("                                                                                                                     
      [195] "                    inputId = \"summarise_cohort_count_plot_5_facet\","                                                                                       
      [196] "                    label = \"facet\","                                                                                                                       
      [197] "                    choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                    
      [198] "                    selected = c(\"cdm_name\"),"                                                                                                              
      [199] "                    multiple = TRUE,"                                                                                                                         
      [200] "                    options = list(plugins = \"remove_button\")"                                                                                              
      [201] "                  ),"                                                                                                                                         
      [202] "                  shiny::selectizeInput("                                                                                                                     
      [203] "                    inputId = \"summarise_cohort_count_plot_5_colour\","                                                                                      
      [204] "                    label = \"colour\","                                                                                                                      
      [205] "                    choices = c(\"cdm_name\", \"cohort_name\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                    
      [206] "                    selected = NULL,"                                                                                                                         
      [207] "                    multiple = TRUE,"                                                                                                                         
      [208] "                    options = list(plugins = \"remove_button\")"                                                                                              
      [209] "                  ),"                                                                                                                                         
      [210] "                  position = \"right\""                                                                                                                       
      [211] "                ),"                                                                                                                                           
      [212] "                shiny::plotOutput(\"summarise_cohort_count_plot_5\")"                                                                                         
      [213] "              )"                                                                                                                                              
      [214] "            )"                                                                                                                                                
      [215] "          )"                                                                                                                                                  
      [216] "        )"                                                                                                                                                    
      [217] "      )"                                                                                                                                                      
      [218] "    ),"                                                                                                                                                       
      [219] "    bslib::nav_panel("                                                                                                                                        
      [220] "      title = \"Cohort Attrition\","                                                                                                                          
      [221] "      icon = shiny::icon(\"layer-group\"),"                                                                                                                   
      [222] "      bslib::layout_sidebar("                                                                                                                                 
      [223] "        sidebar = bslib::sidebar("                                                                                                                            
      [224] "          bslib::accordion("                                                                                                                                  
      [225] "            bslib::accordion_panel("                                                                                                                          
      [226] "              title = \"Information\","                                                                                                                       
      [227] "              icon = shiny::icon(\"info\"),"                                                                                                                  
      [228] "              shiny::p(\"\")"                                                                                                                                 
      [229] "            ),"                                                                                                                                               
      [230] "            bslib::accordion_panel("                                                                                                                          
      [231] "              title = \"Settings\","                                                                                                                          
      [232] "              shiny::selectizeInput("                                                                                                                         
      [233] "                inputId = \"summarise_cohort_attrition_settings_cohort_definition_id\","                                                                      
      [234] "                label = \"Cohort definition id\","                                                                                                            
      [235] "                choices = c(1, 2, 3),"                                                                                                                        
      [236] "                selected = c(1, 2, 3),"                                                                                                                       
      [237] "                multiple = TRUE,"                                                                                                                             
      [238] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [239] "              ),"                                                                                                                                             
      [240] "              shiny::selectizeInput("                                                                                                                         
      [241] "                inputId = \"summarise_cohort_attrition_settings_table_name\","                                                                                
      [242] "                label = \"Table name\","                                                                                                                      
      [243] "                choices = c(\"cohort\"),"                                                                                                                     
      [244] "                selected = c(\"cohort\"),"                                                                                                                    
      [245] "                multiple = TRUE,"                                                                                                                             
      [246] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [247] "              )"                                                                                                                                              
      [248] "            ),"                                                                                                                                               
      [249] "            bslib::accordion_panel("                                                                                                                          
      [250] "              title = \"grouping\","                                                                                                                          
      [251] "              shiny::selectizeInput("                                                                                                                         
      [252] "                inputId = \"summarise_cohort_attrition_grouping_cdm_name\","                                                                                  
      [253] "                label = \"Cdm name\","                                                                                                                        
      [254] "                choices = c(\"mock database\"),"                                                                                                              
      [255] "                selected = c(\"mock database\"),"                                                                                                             
      [256] "                multiple = TRUE,"                                                                                                                             
      [257] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [258] "              ),"                                                                                                                                             
      [259] "              shiny::selectizeInput("                                                                                                                         
      [260] "                inputId = \"summarise_cohort_attrition_grouping_cohort_name\","                                                                               
      [261] "                label = \"Cohort name\","                                                                                                                     
      [262] "                choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                  
      [263] "                selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                 
      [264] "                multiple = TRUE,"                                                                                                                             
      [265] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [266] "              ),"                                                                                                                                             
      [267] "              shiny::selectizeInput("                                                                                                                         
      [268] "                inputId = \"summarise_cohort_attrition_grouping_reason\","                                                                                    
      [269] "                label = \"Reason\","                                                                                                                          
      [270] "                choices = c(\"Initial qualifying events\"),"                                                                                                  
      [271] "                selected = c(\"Initial qualifying events\"),"                                                                                                 
      [272] "                multiple = TRUE,"                                                                                                                             
      [273] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [274] "              ),"                                                                                                                                             
      [275] "              shiny::selectizeInput("                                                                                                                         
      [276] "                inputId = \"summarise_cohort_attrition_grouping_reason_id\","                                                                                 
      [277] "                label = \"Reason id\","                                                                                                                       
      [278] "                choices = c(\"1\"),"                                                                                                                          
      [279] "                selected = c(\"1\"),"                                                                                                                         
      [280] "                multiple = TRUE,"                                                                                                                             
      [281] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [282] "              )"                                                                                                                                              
      [283] "            ),"                                                                                                                                               
      [284] "            bslib::accordion_panel("                                                                                                                          
      [285] "              title = \"Variables\","                                                                                                                         
      [286] "              shiny::selectizeInput("                                                                                                                         
      [287] "                inputId = \"summarise_cohort_attrition_variable_name\","                                                                                      
      [288] "                label = \"Variable name\","                                                                                                                   
      [289] "                choices = NULL,"                                                                                                                              
      [290] "                selected = NULL,"                                                                                                                             
      [291] "                multiple = TRUE,"                                                                                                                             
      [292] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [293] "              )"                                                                                                                                              
      [294] "            ),"                                                                                                                                               
      [295] "            bslib::accordion_panel("                                                                                                                          
      [296] "              title = \"Estimates\","                                                                                                                         
      [297] "              shiny::selectizeInput("                                                                                                                         
      [298] "                inputId = \"summarise_cohort_attrition_estimate_name\","                                                                                      
      [299] "                label = \"Estimate name\","                                                                                                                   
      [300] "                choices = c(\"count\"),"                                                                                                                      
      [301] "                selected = c(\"count\"),"                                                                                                                     
      [302] "                multiple = TRUE,"                                                                                                                             
      [303] "                options = list(plugins = \"remove_button\")"                                                                                                  
      [304] "              )"                                                                                                                                              
      [305] "            )"                                                                                                                                                
      [306] "          )"                                                                                                                                                  
      [307] "        ),"                                                                                                                                                   
      [308] "        bslib::navset_card_tab("                                                                                                                              
      [309] "          bslib::nav_panel("                                                                                                                                  
      [310] "            title = \"Raw\","                                                                                                                                 
      [311] "            bslib::card("                                                                                                                                     
      [312] "              full_screen = TRUE,"                                                                                                                            
      [313] "              bslib::card_header("                                                                                                                            
      [314] "                bslib::popover("                                                                                                                              
      [315] "                  shiny::icon(\"download\"),"                                                                                                                 
      [316] "                  shiny::downloadButton(outputId = \"summarise_cohort_attrition_raw_download\", label = \"Download summarised_result\")"                      
      [317] "                ),"                                                                                                                                           
      [318] "                class = \"text-end\""                                                                                                                         
      [319] "              ),"                                                                                                                                             
      [320] "              DT::dataTableOutput(\"summarise_cohort_attrition_raw\")"                                                                                        
      [321] "            )"                                                                                                                                                
      [322] "          ),"                                                                                                                                                 
      [323] "          bslib::nav_panel("                                                                                                                                  
      [324] "            title = \"Tidy\","                                                                                                                                
      [325] "            bslib::card("                                                                                                                                     
      [326] "              full_screen = TRUE,"                                                                                                                            
      [327] "              bslib::card_header("                                                                                                                            
      [328] "                bslib::popover("                                                                                                                              
      [329] "                  shiny::icon(\"download\"),"                                                                                                                 
      [330] "                  shiny::downloadButton(outputId = \"summarise_cohort_attrition_tidy_download\", label = \"Download csv\")"                                   
      [331] "                ),"                                                                                                                                           
      [332] "                class = \"text-end\""                                                                                                                         
      [333] "              ),"                                                                                                                                             
      [334] "              bslib::layout_sidebar("                                                                                                                         
      [335] "                sidebar = bslib::sidebar("                                                                                                                    
      [336] "                  shiny::checkboxInput("                                                                                                                      
      [337] "                    inputId = \"summarise_cohort_attrition_tidy_settings\","                                                                                  
      [338] "                    label = \"Show settings\","                                                                                                               
      [339] "                    value = FALSE"                                                                                                                            
      [340] "                  ),"                                                                                                                                         
      [341] "                  shiny::checkboxInput("                                                                                                                      
      [342] "                    inputId = \"summarise_cohort_attrition_tidy_grouping\","                                                                                  
      [343] "                    label = \"Show grouping\","                                                                                                               
      [344] "                    value = TRUE"                                                                                                                             
      [345] "                  ),"                                                                                                                                         
      [346] "                  shiny::radioButtons("                                                                                                                       
      [347] "                    inputId = \"summarise_cohort_attrition_tidy_pivot\","                                                                                     
      [348] "                    label = \"Pivot estimates/variables\","                                                                                                   
      [349] "                    choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                       
      [350] "                    selected = \"none\""                                                                                                                      
      [351] "                  ),"                                                                                                                                         
      [352] "                  position = \"right\""                                                                                                                       
      [353] "                ),"                                                                                                                                           
      [354] "                DT::dataTableOutput(\"summarise_cohort_attrition_tidy\")"                                                                                     
      [355] "              )"                                                                                                                                              
      [356] "            )"                                                                                                                                                
      [357] "          ),"                                                                                                                                                 
      [358] "          bslib::nav_panel("                                                                                                                                  
      [359] "            title = \"Formatted\","                                                                                                                           
      [360] "            bslib::card("                                                                                                                                     
      [361] "              full_screen = TRUE,"                                                                                                                            
      [362] "              bslib::card_header("                                                                                                                            
      [363] "                bslib::popover("                                                                                                                              
      [364] "                  shiny::icon(\"download\"),"                                                                                                                 
      [365] "                  shiny::selectizeInput("                                                                                                                     
      [366] "                    inputId = \"summarise_cohort_attrition_formatted_download_type\","                                                                        
      [367] "                    label = \"File\","                                                                                                                        
      [368] "                    choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                       
      [369] "                    selected = c(\"docx\"),"                                                                                                                  
      [370] "                    multiple = FALSE,"                                                                                                                        
      [371] "                    options = list(plugins = \"remove_button\")"                                                                                              
      [372] "                  ),"                                                                                                                                         
      [373] "                  shiny::downloadButton(outputId = \"summarise_cohort_attrition_formatted_download\", label = \"Download\")"                                  
      [374] "                ),"                                                                                                                                           
      [375] "                class = \"text-end\""                                                                                                                         
      [376] "              ),"                                                                                                                                             
      [377] "              bslib::layout_sidebar("                                                                                                                         
      [378] "                sidebar = bslib::sidebar("                                                                                                                    
      [379] "                  sortable::bucket_list("                                                                                                                     
      [380] "                    header = NULL,"                                                                                                                           
      [381] "                    sortable::add_rank_list("                                                                                                                 
      [382] "                      text = \"None\","                                                                                                                       
      [383] "                      labels = c(\"reason\", \"reason_id\", \"variable_name\", \"variable_level\", \"estimate_name\"),"                                       
      [384] "                      input_id = \"summarise_cohort_attrition_formatted_none\""                                                                               
      [385] "                    ),"                                                                                                                                       
      [386] "                    sortable::add_rank_list("                                                                                                                 
      [387] "                      text = \"Header\","                                                                                                                     
      [388] "                      labels = c(\"cdm_name\"),"                                                                                                              
      [389] "                      input_id = \"summarise_cohort_attrition_formatted_header\""                                                                             
      [390] "                    ),"                                                                                                                                       
      [391] "                    sortable::add_rank_list("                                                                                                                 
      [392] "                      text = \"Group\","                                                                                                                      
      [393] "                      labels = c(\"cohort_name\"),"                                                                                                           
      [394] "                      input_id = \"summarise_cohort_attrition_formatted_group\""                                                                              
      [395] "                    ),"                                                                                                                                       
      [396] "                    sortable::add_rank_list("                                                                                                                 
      [397] "                      text = \"Hide\","                                                                                                                       
      [398] "                      labels = c(\"cohort_definition_id\", \"table_name\"),"                                                                                  
      [399] "                      input_id = \"summarise_cohort_attrition_formatted_hide\""                                                                               
      [400] "                    )"                                                                                                                                        
      [401] "                  ),"                                                                                                                                         
      [402] "                  position = \"right\""                                                                                                                       
      [403] "                ),"                                                                                                                                           
      [404] "                gt::gt_output(\"summarise_cohort_attrition_formatted\")"                                                                                      
      [405] "              )"                                                                                                                                              
      [406] "            )"                                                                                                                                                
      [407] "          ),"                                                                                                                                                 
      [408] "          bslib::nav_panel("                                                                                                                                  
      [409] "            title = \"Diagram\","                                                                                                                             
      [410] "            bslib::card("                                                                                                                                     
      [411] "              full_screen = TRUE,"                                                                                                                            
      [412] "              bslib::card_header("                                                                                                                            
      [413] "                bslib::popover("                                                                                                                              
      [414] "                  shiny::icon(\"download\"),"                                                                                                                 
      [415] "                  shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_width\", label = \"Width (px)\", value = 15),"                   
      [416] "                  shiny::numericInput(inputId = \"summarise_cohort_attrition_plot_2_download_height\", label = \"Height (px)\", value = 10),"                 
      [417] "                  shiny::downloadButton(outputId = \"summarise_cohort_attrition_plot_2_download\", label = \"Download png\")"                                 
      [418] "                ),"                                                                                                                                           
      [419] "                class = \"text-end\""                                                                                                                         
      [420] "              ),"                                                                                                                                             
      [421] "              bslib::layout_sidebar("                                                                                                                         
      [422] "                sidebar = bslib::sidebar("                                                                                                                    
      [423] "                  position = \"right\""                                                                                                                       
      [424] "                ),"                                                                                                                                           
      [425] "                DiagrammeR::grVizOutput(\"summarise_cohort_attrition_plot_2\")"                                                                               
      [426] "              )"                                                                                                                                              
      [427] "            )"                                                                                                                                                
      [428] "          )"                                                                                                                                                  
      [429] "        )"                                                                                                                                                    
      [430] "      )"                                                                                                                                                      
      [431] "    ),"                                                                                                                                                       
      [432] "    icon = shiny::icon(\"rectangle-list\")"                                                                                                                   
      [433] "  ),"                                                                                                                                                         
      [434] "  bslib::nav_panel("                                                                                                                                          
      [435] "    title = \"Cohort overlap\","                                                                                                                              
      [436] "    icon = shiny::icon(\"circle-half-stroke\"),"                                                                                                              
      [437] "    bslib::layout_sidebar("                                                                                                                                   
      [438] "      sidebar = bslib::sidebar("                                                                                                                              
      [439] "        bslib::accordion("                                                                                                                                    
      [440] "          bslib::accordion_panel("                                                                                                                            
      [441] "            title = \"Information\","                                                                                                                         
      [442] "            icon = shiny::icon(\"info\"),"                                                                                                                    
      [443] "            shiny::p(\"Cohort overlap shows the number of subjects that contribute to a pair of cohorts.\")"                                                  
      [444] "          ),"                                                                                                                                                 
      [445] "          bslib::accordion_panel("                                                                                                                            
      [446] "            title = \"grouping\","                                                                                                                            
      [447] "            shiny::selectizeInput("                                                                                                                           
      [448] "              inputId = \"summarise_cohort_overlap_grouping_cdm_name\","                                                                                      
      [449] "              label = \"Cdm name\","                                                                                                                          
      [450] "              choices = c(\"mock database\"),"                                                                                                                
      [451] "              selected = c(\"mock database\"),"                                                                                                               
      [452] "              multiple = TRUE,"                                                                                                                               
      [453] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [454] "            ),"                                                                                                                                               
      [455] "            shiny::selectizeInput("                                                                                                                           
      [456] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_reference\","                                                                         
      [457] "              label = \"Cohort name reference\","                                                                                                             
      [458] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [459] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [460] "              multiple = TRUE,"                                                                                                                               
      [461] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [462] "            ),"                                                                                                                                               
      [463] "            shiny::selectizeInput("                                                                                                                           
      [464] "              inputId = \"summarise_cohort_overlap_grouping_cohort_name_comparator\","                                                                        
      [465] "              label = \"Cohort name comparator\","                                                                                                            
      [466] "              choices = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                    
      [467] "              selected = c(\"asthma\", \"covid\", \"tb\"),"                                                                                                   
      [468] "              multiple = TRUE,"                                                                                                                               
      [469] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [470] "            )"                                                                                                                                                
      [471] "          ),"                                                                                                                                                 
      [472] "          bslib::accordion_panel("                                                                                                                            
      [473] "            title = \"Variables\","                                                                                                                           
      [474] "            shiny::selectizeInput("                                                                                                                           
      [475] "              inputId = \"summarise_cohort_overlap_variable_name\","                                                                                          
      [476] "              label = \"Variable name\","                                                                                                                     
      [477] "              choices = NULL,"                                                                                                                                
      [478] "              selected = NULL,"                                                                                                                               
      [479] "              multiple = TRUE,"                                                                                                                               
      [480] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [481] "            )"                                                                                                                                                
      [482] "          ),"                                                                                                                                                 
      [483] "          bslib::accordion_panel("                                                                                                                            
      [484] "            title = \"Estimates\","                                                                                                                           
      [485] "            shiny::selectizeInput("                                                                                                                           
      [486] "              inputId = \"summarise_cohort_overlap_estimate_name\","                                                                                          
      [487] "              label = \"Estimate name\","                                                                                                                     
      [488] "              choices = c(\"count\", \"percentage\"),"                                                                                                        
      [489] "              selected = c(\"count\", \"percentage\"),"                                                                                                       
      [490] "              multiple = TRUE,"                                                                                                                               
      [491] "              options = list(plugins = \"remove_button\")"                                                                                                    
      [492] "            )"                                                                                                                                                
      [493] "          )"                                                                                                                                                  
      [494] "        )"                                                                                                                                                    
      [495] "      ),"                                                                                                                                                     
      [496] "      bslib::navset_card_tab("                                                                                                                                
      [497] "        bslib::nav_panel("                                                                                                                                    
      [498] "          title = \"Raw\","                                                                                                                                   
      [499] "          bslib::card("                                                                                                                                       
      [500] "            full_screen = TRUE,"                                                                                                                              
      [501] "            bslib::card_header("                                                                                                                              
      [502] "              bslib::popover("                                                                                                                                
      [503] "                shiny::icon(\"download\"),"                                                                                                                   
      [504] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_raw_download\", label = \"Download summarised_result\")"                          
      [505] "              ),"                                                                                                                                             
      [506] "              class = \"text-end\""                                                                                                                           
      [507] "            ),"                                                                                                                                               
      [508] "            DT::dataTableOutput(\"summarise_cohort_overlap_raw\")"                                                                                            
      [509] "          )"                                                                                                                                                  
      [510] "        ),"                                                                                                                                                   
      [511] "        bslib::nav_panel("                                                                                                                                    
      [512] "          title = \"Tidy\","                                                                                                                                  
      [513] "          bslib::card("                                                                                                                                       
      [514] "            full_screen = TRUE,"                                                                                                                              
      [515] "            bslib::card_header("                                                                                                                              
      [516] "              bslib::popover("                                                                                                                                
      [517] "                shiny::icon(\"download\"),"                                                                                                                   
      [518] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_tidy_download\", label = \"Download csv\")"                                       
      [519] "              ),"                                                                                                                                             
      [520] "              class = \"text-end\""                                                                                                                           
      [521] "            ),"                                                                                                                                               
      [522] "            bslib::layout_sidebar("                                                                                                                           
      [523] "              sidebar = bslib::sidebar("                                                                                                                      
      [524] "                shiny::checkboxInput("                                                                                                                        
      [525] "                  inputId = \"summarise_cohort_overlap_tidy_settings\","                                                                                      
      [526] "                  label = \"Show settings\","                                                                                                                 
      [527] "                  value = FALSE"                                                                                                                              
      [528] "                ),"                                                                                                                                           
      [529] "                shiny::checkboxInput("                                                                                                                        
      [530] "                  inputId = \"summarise_cohort_overlap_tidy_grouping\","                                                                                      
      [531] "                  label = \"Show grouping\","                                                                                                                 
      [532] "                  value = TRUE"                                                                                                                               
      [533] "                ),"                                                                                                                                           
      [534] "                shiny::radioButtons("                                                                                                                         
      [535] "                  inputId = \"summarise_cohort_overlap_tidy_pivot\","                                                                                         
      [536] "                  label = \"Pivot estimates/variables\","                                                                                                     
      [537] "                  choices = c(\"none\", \"estimates\", \"estimates and variables\"),"                                                                         
      [538] "                  selected = \"none\""                                                                                                                        
      [539] "                ),"                                                                                                                                           
      [540] "                position = \"right\""                                                                                                                         
      [541] "              ),"                                                                                                                                             
      [542] "              DT::dataTableOutput(\"summarise_cohort_overlap_tidy\")"                                                                                         
      [543] "            )"                                                                                                                                                
      [544] "          )"                                                                                                                                                  
      [545] "        ),"                                                                                                                                                   
      [546] "        bslib::nav_panel("                                                                                                                                    
      [547] "          title = \"Formatted\","                                                                                                                             
      [548] "          bslib::card("                                                                                                                                       
      [549] "            full_screen = TRUE,"                                                                                                                              
      [550] "            bslib::card_header("                                                                                                                              
      [551] "              bslib::popover("                                                                                                                                
      [552] "                shiny::icon(\"download\"),"                                                                                                                   
      [553] "                shiny::selectizeInput("                                                                                                                       
      [554] "                  inputId = \"summarise_cohort_overlap_formatted_download_type\","                                                                            
      [555] "                  label = \"File\","                                                                                                                          
      [556] "                  choices = c(\"docx\", \"png\", \"pdf\", \"html\"),"                                                                                         
      [557] "                  selected = c(\"docx\"),"                                                                                                                    
      [558] "                  multiple = FALSE,"                                                                                                                          
      [559] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [560] "                ),"                                                                                                                                           
      [561] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_formatted_download\", label = \"Download\")"                                      
      [562] "              ),"                                                                                                                                             
      [563] "              class = \"text-end\""                                                                                                                           
      [564] "            ),"                                                                                                                                               
      [565] "            bslib::layout_sidebar("                                                                                                                           
      [566] "              sidebar = bslib::sidebar("                                                                                                                      
      [567] "                sortable::bucket_list("                                                                                                                       
      [568] "                  header = NULL,"                                                                                                                             
      [569] "                  sortable::add_rank_list("                                                                                                                   
      [570] "                    text = \"None\","                                                                                                                         
      [571] "                    labels = c(\"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"             
      [572] "                    input_id = \"summarise_cohort_overlap_formatted_none\""                                                                                   
      [573] "                  ),"                                                                                                                                         
      [574] "                  sortable::add_rank_list("                                                                                                                   
      [575] "                    text = \"Header\","                                                                                                                       
      [576] "                    labels = c(\"cdm_name\"),"                                                                                                                
      [577] "                    input_id = \"summarise_cohort_overlap_formatted_header\""                                                                                 
      [578] "                  ),"                                                                                                                                         
      [579] "                  sortable::add_rank_list("                                                                                                                   
      [580] "                    text = \"Group\","                                                                                                                        
      [581] "                    labels = character(),"                                                                                                                    
      [582] "                    input_id = \"summarise_cohort_overlap_formatted_group\""                                                                                  
      [583] "                  ),"                                                                                                                                         
      [584] "                  sortable::add_rank_list("                                                                                                                   
      [585] "                    text = \"Hide\","                                                                                                                         
      [586] "                    labels = character(),"                                                                                                                    
      [587] "                    input_id = \"summarise_cohort_overlap_formatted_hide\""                                                                                   
      [588] "                  )"                                                                                                                                          
      [589] "                ),"                                                                                                                                           
      [590] "                position = \"right\""                                                                                                                         
      [591] "              ),"                                                                                                                                             
      [592] "              gt::gt_output(\"summarise_cohort_overlap_formatted\")"                                                                                          
      [593] "            )"                                                                                                                                                
      [594] "          )"                                                                                                                                                  
      [595] "        ),"                                                                                                                                                   
      [596] "        bslib::nav_panel("                                                                                                                                    
      [597] "          title = \"Plot cohort overlap\","                                                                                                                   
      [598] "          bslib::card("                                                                                                                                       
      [599] "            full_screen = TRUE,"                                                                                                                              
      [600] "            bslib::card_header("                                                                                                                              
      [601] "              bslib::popover("                                                                                                                                
      [602] "                shiny::icon(\"download\"),"                                                                                                                   
      [603] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_width\", label = \"Width\", value = 15),"                            
      [604] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_height\", label = \"Height\", value = 10),"                          
      [605] "                shiny::selectizeInput("                                                                                                                       
      [606] "                  inputId = \"summarise_cohort_overlap_plot_1_download_units\","                                                                              
      [607] "                  label = \"Units\","                                                                                                                         
      [608] "                  choices = c(\"px\", \"cm\", \"inch\"),"                                                                                                     
      [609] "                  selected = c(\"cm\"),"                                                                                                                      
      [610] "                  multiple = FALSE,"                                                                                                                          
      [611] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [612] "                ),"                                                                                                                                           
      [613] "                shiny::numericInput(inputId = \"summarise_cohort_overlap_plot_1_download_dpi\", label = \"dpi\", value = 300),"                               
      [614] "                shiny::downloadButton(outputId = \"summarise_cohort_overlap_plot_1_download\", label = \"Download png\")"                                     
      [615] "              ),"                                                                                                                                             
      [616] "              class = \"text-end\""                                                                                                                           
      [617] "            ),"                                                                                                                                               
      [618] "            bslib::layout_sidebar("                                                                                                                           
      [619] "              sidebar = bslib::sidebar("                                                                                                                      
      [620] "                shiny::selectizeInput("                                                                                                                       
      [621] "                  inputId = \"summarise_cohort_overlap_plot_1_facet\","                                                                                       
      [622] "                  label = \"facet\","                                                                                                                         
      [623] "                  choices = c(\"cdm_name\", \"cohort_name_reference\", \"cohort_name_comparator\", \"variable_name\", \"variable_level\", \"estimate_name\"),"
      [624] "                  selected = c(\"cdm_name\", \"cohort_name_reference\"),"                                                                                     
      [625] "                  multiple = TRUE,"                                                                                                                           
      [626] "                  options = list(plugins = \"remove_button\")"                                                                                                
      [627] "                ),"                                                                                                                                           
      [628] "                shiny::checkboxInput("                                                                                                                        
      [629] "                  inputId = \"summarise_cohort_overlap_plot_1_unique_combinations\","                                                                         
      [630] "                  label = \"uniqueCombinations\","                                                                                                            
      [631] "                  value = c(TRUE)"                                                                                                                            
      [632] "                ),"                                                                                                                                           
      [633] "                position = \"right\""                                                                                                                         
      [634] "              ),"                                                                                                                                             
      [635] "              shiny::plotOutput(\"summarise_cohort_overlap_plot_1\")"                                                                                         
      [636] "            )"                                                                                                                                                
      [637] "          )"                                                                                                                                                  
      [638] "        )"                                                                                                                                                    
      [639] "      )"                                                                                                                                                      
      [640] "    )"                                                                                                                                                        
      [641] "  ),"                                                                                                                                                         
      [642] "  bslib::nav_spacer(),"                                                                                                                                       
      [643] "  bslib::nav_item("                                                                                                                                           
      [644] "    bslib::popover("                                                                                                                                          
      [645] "      shiny::icon(\"circle-info\"),"                                                                                                                          
      [646] "      shiny::tags$img("                                                                                                                                       
      [647] "        src = \"hds_logo.svg\","                                                                                                                              
      [648] "        class = \"logo-img\","                                                                                                                                
      [649] "        alt = \"Logo\","                                                                                                                                      
      [650] "        height = \"auto\","                                                                                                                                   
      [651] "        width = \"30%\","                                                                                                                                     
      [652] "        style = \"float:right\""                                                                                                                              
      [653] "      ),"                                                                                                                                                     
      [654] "      \"This shiny app was generated with \","                                                                                                                
      [655] "      shiny::a("                                                                                                                                              
      [656] "        \"omopViewer\","                                                                                                                                      
      [657] "        href = \"https://github.com/oxford-pharmacoepi/omopViewer\","                                                                                         
      [658] "        target = \"_blank\""                                                                                                                                  
      [659] "      ),"                                                                                                                                                     
      [660] "      shiny::strong(\"v0.0.0.900\")"                                                                                                                          
      [661] "    )"                                                                                                                                                        
      [662] "  ),"                                                                                                                                                         
      [663] "  bslib::nav_item(bslib::input_dark_mode(id = \"dark_mode\", mode = \"light\"))"                                                                              
      [664] ")"                                                                                                                                                            

