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
          "My study"
        ), ,
        bslib::nav_spacer(),
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
        title = "My study", ,
        bslib::nav_spacer(),
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
      }

# CohortCharacteristics shiny

    Code
      cat(uiStatic(choices = getChoices(result)), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = "My study", ,
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    selected = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    selected = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "height", value = 10),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = c(1, 2, 3),
                    selected = c(1, 2, 3),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason",
                    label = "Reason",
                    choices = c("Initial qualifying events"),
                    selected = c("Initial qualifying events"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason_id",
                    label = "Reason id",
                    choices = c("1"),
                    selected = c("1"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    selected = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_attrition_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_plot_2_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_dpi", label = "dpi", value = 300),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = c("Number records", "Number subjects"),
                    selected = c("Number records", "Number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_count_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "height", value = 10),
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
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = c("comparator", "overlap", "reference"),
                    selected = c("comparator", "overlap", "reference"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_overlap_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "height", value = 10),
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
                    choices = c(TRUE),
                    selected = c(TRUE),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = c("days_between_cohort_entries", "number records", "number subjects"),
                    selected = c("days_between_cohort_entries", "number records", "number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    selected = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_timing_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "height", value = 10),
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
                    choices = c("condition_occurrence", "drug_exposure"),
                    selected = c("condition_occurrence", "drug_exposure"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = c("episode", "event"),
                    selected = c("episode", "event"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = c("standard"),
                    selected = c("standard"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_sex",
                    label = "Sex",
                    choices = c("Female", "Male", "overall"),
                    selected = c("Female", "Male", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_age_group",
                    label = "Age group",
                    choices = c("0 to 44", "45 or above", "overall"),
                    selected = c("0 to 44", "45 or above", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_concept_id",
                    label = "Concept id",
                    choices = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    selected = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    selected = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_large_scale_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download word")
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
      cat(serverStatic(resultTypes = names(getChoices(result))), sep = "\n")
    Output
      server <- function(input, output, session) {
        # summarise_characteristics -----
        ## raw summarise_characteristics -----
        getRawDataSummariseCharacteristics <- shiny::reactive({
          filterData(data, "summarise_characteristics", input)
        })
        output$summarise_characteristics_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarise_characteristics_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_characteristics.csv",
          content = function(file) {
            getRawDataSummariseCharacteristics() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_characteristics -----
        getTidyDataSummariseCharacteristics <- shiny::reactive({
          data |>
            filterData("summarise_characteristics", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_characteristics_tidy_settings,
              showGroupping = input$summarise_characteristics_tidy_groupping,
              pivot = input$summarise_characteristics_tidy_pivot
            )
        })
        output$summarise_characteristics_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseCharacteristics(), options = list(scrollX = TRUE))
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
            filterData("summarise_characteristics", input) |>
            visTable(
              header = input$summarise_characteristics_formatted_header,
              group = input$summarise_characteristics_formatted_group,
              hide = input$summarise_characteristics_formatted_hide
            )
        })
        output$summarise_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummariseCharacteristics()
        })
        output$summarise_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_characteristics.docx",
          content = function(file) {
            getFormattedDataSummariseCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_characteristics -----
        createPlot4 <- shiny::reactive({
          result <- data |>
            filterData("summarise_characteristics", input)
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
      
      
        # summarise_cohort_attrition -----
        ## raw summarise_cohort_attrition -----
        getRawDataSummariseCohortAttrition <- shiny::reactive({
          filterData(data, "summarise_cohort_attrition", input)
        })
        output$summarise_cohort_attrition_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortAttrition(), options = list(scrollX = TRUE))
        })
        output$summarise_cohort_attrition_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_cohort_attrition.csv",
          content = function(file) {
            getRawDataSummariseCohortAttrition() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_cohort_attrition -----
        getTidyDataSummariseCohortAttrition <- shiny::reactive({
          data |>
            filterData("summarise_cohort_attrition", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_attrition_tidy_settings,
              showGroupping = input$summarise_cohort_attrition_tidy_groupping,
              pivot = input$summarise_cohort_attrition_tidy_pivot
            )
        })
        output$summarise_cohort_attrition_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseCohortAttrition(), options = list(scrollX = TRUE))
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
            filterData("summarise_cohort_attrition", input) |>
            visTable(
              header = input$summarise_cohort_attrition_formatted_header,
              group = input$summarise_cohort_attrition_formatted_group,
              hide = input$summarise_cohort_attrition_formatted_hide
            )
        })
        output$summarise_cohort_attrition_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortAttrition()
        })
        output$summarise_cohort_attrition_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_cohort_attrition.docx",
          content = function(file) {
            getFormattedDataSummariseCohortAttrition() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_attrition -----
        createPlot2 <- shiny::reactive({
          result <- data |>
            filterData("summarise_cohort_attrition", input)
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
      
      
        # summarise_cohort_count -----
        ## raw summarise_cohort_count -----
        getRawDataSummariseCohortCount <- shiny::reactive({
          filterData(data, "summarise_cohort_count", input)
        })
        output$summarise_cohort_count_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortCount(), options = list(scrollX = TRUE))
        })
        output$summarise_cohort_count_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_cohort_count.csv",
          content = function(file) {
            getRawDataSummariseCohortCount() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_cohort_count -----
        getTidyDataSummariseCohortCount <- shiny::reactive({
          data |>
            filterData("summarise_cohort_count", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_count_tidy_settings,
              showGroupping = input$summarise_cohort_count_tidy_groupping,
              pivot = input$summarise_cohort_count_tidy_pivot
            )
        })
        output$summarise_cohort_count_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseCohortCount(), options = list(scrollX = TRUE))
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
            filterData("summarise_cohort_count", input) |>
            visTable(
              header = input$summarise_cohort_count_formatted_header,
              group = input$summarise_cohort_count_formatted_group,
              hide = input$summarise_cohort_count_formatted_hide
            )
        })
        output$summarise_cohort_count_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortCount()
        })
        output$summarise_cohort_count_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_cohort_count.docx",
          content = function(file) {
            getFormattedDataSummariseCohortCount() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_count -----
        createPlot5 <- shiny::reactive({
          result <- data |>
            filterData("summarise_cohort_count", input)
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
      
      
        # summarise_cohort_overlap -----
        ## raw summarise_cohort_overlap -----
        getRawDataSummariseCohortOverlap <- shiny::reactive({
          filterData(data, "summarise_cohort_overlap", input)
        })
        output$summarise_cohort_overlap_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortOverlap(), options = list(scrollX = TRUE))
        })
        output$summarise_cohort_overlap_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_cohort_overlap.csv",
          content = function(file) {
            getRawDataSummariseCohortOverlap() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_cohort_overlap -----
        getTidyDataSummariseCohortOverlap <- shiny::reactive({
          data |>
            filterData("summarise_cohort_overlap", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_overlap_tidy_settings,
              showGroupping = input$summarise_cohort_overlap_tidy_groupping,
              pivot = input$summarise_cohort_overlap_tidy_pivot
            )
        })
        output$summarise_cohort_overlap_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseCohortOverlap(), options = list(scrollX = TRUE))
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
            filterData("summarise_cohort_overlap", input) |>
            visTable(
              header = input$summarise_cohort_overlap_formatted_header,
              group = input$summarise_cohort_overlap_formatted_group,
              hide = input$summarise_cohort_overlap_formatted_hide
            )
        })
        output$summarise_cohort_overlap_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortOverlap()
        })
        output$summarise_cohort_overlap_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_cohort_overlap.docx",
          content = function(file) {
            getFormattedDataSummariseCohortOverlap() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_overlap -----
        createPlot1 <- shiny::reactive({
          result <- data |>
            filterData("summarise_cohort_overlap", input)
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
      
      
        # summarise_cohort_timing -----
        ## raw summarise_cohort_timing -----
        getRawDataSummariseCohortTiming <- shiny::reactive({
          filterData(data, "summarise_cohort_timing", input)
        })
        output$summarise_cohort_timing_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseCohortTiming(), options = list(scrollX = TRUE))
        })
        output$summarise_cohort_timing_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_cohort_timing.csv",
          content = function(file) {
            getRawDataSummariseCohortTiming() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_cohort_timing -----
        getTidyDataSummariseCohortTiming <- shiny::reactive({
          data |>
            filterData("summarise_cohort_timing", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_cohort_timing_tidy_settings,
              showGroupping = input$summarise_cohort_timing_tidy_groupping,
              pivot = input$summarise_cohort_timing_tidy_pivot
            )
        })
        output$summarise_cohort_timing_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseCohortTiming(), options = list(scrollX = TRUE))
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
            filterData("summarise_cohort_timing", input) |>
            visTable(
              header = input$summarise_cohort_timing_formatted_header,
              group = input$summarise_cohort_timing_formatted_group,
              hide = input$summarise_cohort_timing_formatted_hide
            )
        })
        output$summarise_cohort_timing_formatted <- gt::render_gt({
          getFormattedDataSummariseCohortTiming()
        })
        output$summarise_cohort_timing_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_cohort_timing.docx",
          content = function(file) {
            getFormattedDataSummariseCohortTiming() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_cohort_timing -----
        createPlot3 <- shiny::reactive({
          result <- data |>
            filterData("summarise_cohort_timing", input)
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
      
      
        # summarise_large_scale_characteristics -----
        ## raw summarise_large_scale_characteristics -----
        getRawDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          filterData(data, "summarise_large_scale_characteristics", input)
        })
        output$summarise_large_scale_characteristics_raw <- DT::renderDT({
          DT::datatable(getRawDataSummariseLargeScaleCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarise_large_scale_characteristics_raw_download <- shiny::downloadHandler(
          filename = "raw_summarise_large_scale_characteristics.csv",
          content = function(file) {
            getRawDataSummariseLargeScaleCharacteristics() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarise_large_scale_characteristics -----
        getTidyDataSummariseLargeScaleCharacteristics <- shiny::reactive({
          data |>
            filterData("summarise_large_scale_characteristics", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarise_large_scale_characteristics_tidy_settings,
              showGroupping = input$summarise_large_scale_characteristics_tidy_groupping,
              pivot = input$summarise_large_scale_characteristics_tidy_pivot
            )
        })
        output$summarise_large_scale_characteristics_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummariseLargeScaleCharacteristics(), options = list(scrollX = TRUE))
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
            filterData("summarise_large_scale_characteristics", input) |>
            visTable(
              header = input$summarise_large_scale_characteristics_formatted_header,
              group = input$summarise_large_scale_characteristics_formatted_group,
              hide = input$summarise_large_scale_characteristics_formatted_hide
            )
        })
        output$summarise_large_scale_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummariseLargeScaleCharacteristics()
        })
        output$summarise_large_scale_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarise_large_scale_characteristics.docx",
          content = function(file) {
            getFormattedDataSummariseLargeScaleCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarise_large_scale_characteristics -----
      }

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
        title = "My study", ,
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    selected = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    selected = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "height", value = 10),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = c(1, 2, 3),
                    selected = c(1, 2, 3),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason",
                    label = "Reason",
                    choices = c("Initial qualifying events"),
                    selected = c("Initial qualifying events"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason_id",
                    label = "Reason id",
                    choices = c("1"),
                    selected = c("1"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    selected = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_attrition_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_plot_2_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_dpi", label = "dpi", value = 300),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = c("Number records", "Number subjects"),
                    selected = c("Number records", "Number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_count_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "height", value = 10),
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
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = c("comparator", "overlap", "reference"),
                    selected = c("comparator", "overlap", "reference"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_overlap_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "height", value = 10),
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
                    choices = c(TRUE),
                    selected = c(TRUE),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = c("days_between_cohort_entries", "number records", "number subjects"),
                    selected = c("days_between_cohort_entries", "number records", "number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    selected = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_timing_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "height", value = 10),
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
                    choices = c("condition_occurrence", "drug_exposure"),
                    selected = c("condition_occurrence", "drug_exposure"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = c("episode", "event"),
                    selected = c("episode", "event"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = c("standard"),
                    selected = c("standard"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_sex",
                    label = "Sex",
                    choices = c("Female", "Male", "overall"),
                    selected = c("Female", "Male", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_age_group",
                    label = "Age group",
                    choices = c("0 to 44", "45 or above", "overall"),
                    selected = c("0 to 44", "45 or above", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_concept_id",
                    label = "Concept id",
                    choices = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    selected = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    selected = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_large_scale_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download word")
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
          "My study"
        ), ,
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    selected = c("Age", "Cohort end date", "Cohort start date", "Days in cohort", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    selected = c("count", "max", "mean", "median", "min", "percentage", "q25", "q75", "sd"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_characteristics_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_characteristics_plot_4_download_height", label = "height", value = 10),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = c(1, 2, 3),
                    selected = c(1, 2, 3),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason",
                    label = "Reason",
                    choices = c("Initial qualifying events"),
                    selected = c("Initial qualifying events"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_groupping_reason_id",
                    label = "Reason id",
                    choices = c("1"),
                    selected = c("1"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_variable_name",
                    label = "Variable name",
                    choices = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    selected = c("excluded_records", "excluded_subjects", "number_records", "number_subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_attrition_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_attrition_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_attrition_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_attrition_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_height", label = "height", value = 10),
                      shiny::selectizeInput(
                        inputId = "summarise_cohort_attrition_plot_2_download_units",
                        label = "Units",
                        choices = c("px", "cm", "inch"),
                        selected = c("cm"),
                        multiple = FALSE,
                        options = list(plugins = "remove_button")
                      ),
                      shiny::numericInput(inputId = "summarise_cohort_attrition_plot_2_download_dpi", label = "dpi", value = 300),
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
                    choices = c("cohort"),
                    selected = c("cohort"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_variable_name",
                    label = "Variable name",
                    choices = c("Number records", "Number subjects"),
                    selected = c("Number records", "Number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_count_estimate_name",
                    label = "Estimate name",
                    choices = c("count"),
                    selected = c("count"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_count_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_count_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_count_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_count_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_count_plot_5_download_height", label = "height", value = 10),
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
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_variable_name",
                    label = "Variable name",
                    choices = c("comparator", "overlap", "reference"),
                    selected = c("comparator", "overlap", "reference"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_overlap_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_overlap_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_overlap_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_overlap_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_overlap_plot_1_download_height", label = "height", value = 10),
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
                    choices = c(TRUE),
                    selected = c(TRUE),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_groupping_cohort_name_comparator",
                    label = "Cohort name comparator",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_variable_name",
                    label = "Variable name",
                    choices = c("days_between_cohort_entries", "number records", "number subjects"),
                    selected = c("days_between_cohort_entries", "number records", "number subjects"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    selected = c("count", "density_x", "density_y", "max", "median", "min", "q25", "q75"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_cohort_timing_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_cohort_timing_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_cohort_timing_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_cohort_timing_formatted_download", label = "Download word")
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
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_width", label = "width", value = 15),
                      shiny::numericInput(inputId = "summarise_cohort_timing_plot_3_download_height", label = "height", value = 10),
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
                    choices = c("condition_occurrence", "drug_exposure"),
                    selected = c("condition_occurrence", "drug_exposure"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = c("episode", "event"),
                    selected = c("episode", "event"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_settings_analysis",
                    label = "Analysis",
                    choices = c("standard"),
                    selected = c("standard"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_sex",
                    label = "Sex",
                    choices = c("Female", "Male", "overall"),
                    selected = c("Female", "Male", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_age_group",
                    label = "Age group",
                    choices = c("0 to 44", "45 or above", "overall"),
                    selected = c("0 to 44", "45 or above", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_groupping_concept_id",
                    label = "Concept id",
                    choices = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    selected = c("1361364", "1361365", "1361368", "1361370", "1830184", "1830278", "1830279", "1830280", "1830281", "1830282", "1830283", "194152", "1971546", "21018250", "21216049", "21601213", "21601386", "21601387", "21603215", "21603248", "21603365", "21603444", "21603530", "21603812", "21603848", "21603890", "21605007", "21605008", "35604394", "35604434", "35604435", "35604439", "35604877", "35604879", "35604880", "35604883", "35604884", "35741956", "35774678", "36034746", "36034747", "36034751", "36036059", "36217206", "36217213", "37110496", "37498042", "37593197", "37787172", "40371897", "40475132", "40475135", "40721254", "40741270", "4151660", "4220473", "4220524", "4226696", "42899580", "4304866", "44022939", "44081436", "44091285", "444074", "45430573", "45511667", "45533778", "45538734", "45548358", "45548372", "45755492", "45755493", "45756021", "45756023", "46274351"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Variables",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    selected = c("ANTIHISTAMINES FOR SYSTEMIC USE", "ANTINEOPLASTIC AGENTS", "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS", "Acquired deformities of fingers and toes", "Alkaline phosphatase bone isoenzyme raised", "Alkaline phosphatase isoenzyme, bone fraction", "Arthropathies", "Bos taurus catalase preparation", "Bus occupant injured in collision with heavy transport vehicle or bus", "Bus occupant injured in collision with heavy transport vehicle or bus, person on outside of vehicle injured in nontraffic accident", "COUGH AND COLD PREPARATIONS", "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES", "Disorders of patella", "ENDOCRINE THERAPY", "Elevated mood", "IMMUNOSTIMULANTS", "IMMUNOSUPPRESSANTS", "Internal derangement of knee", "Jaaps Health Salts", "Manic mood", "Manic symptoms co-occurrent and due to primary psychotic disorder", "Mentha arvensis top extract / sodium chloride / xylitol Nasal Powder", "NASAL PREPARATIONS", "Nasal Powder", "Nasal Product", "OTHER RESPIRATORY SYSTEM PRODUCTS", "Other acquired deformities of limbs", "Other joint disorders", "Other joint disorders, not elsewhere classified", "Other specific joint derangements", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / sodium carbonate / Sodium Chloride / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Injectable Solution", "POTASSIUM SODIUM TARTRATE / Sodium Bicarbonate / tartaric acid Oral Powder", "POTASSIUM SODIUM TARTRATE 0.0094 MG/MG", "POTASSIUM SODIUM TARTRATE 9.4 MG/MG", "RESPIRATORY SYSTEM", "Renal agenesis and dysgenesis", "Renal agenesis or dysgenesis NOS", "Septiline", "Sisymbrium officianale whole extract 10 MG Nasal Powder", "Sisymbrium officianale whole extract 10 MG Nasal Powder Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon]", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50", "Sisymbrium officianale whole extract 10 MG Nasal Powder [Euphon] Box of 50 by Mayoly Spindler", "Sisymbrium officianale whole extract Nasal Powder", "Sisymbrium officianale whole extract Nasal Powder [Euphon]", "Sodium / Sodium Chloride Nasal Powder", "Sodium potassium tartrate", "Sodium potassium tartrate crystal", "THROAT PREPARATIONS", "Topical Liquefied Gas", "Topical Product", "Uroporphyrin III", "Victim of vehicular AND/OR traffic accident", "combinations of electrolytes; parenteral", "glucagon 3 MG Nasal Powder", "glucagon 3 MG Nasal Powder [Baqsimi]", "glucagon 3 MG Nasal Powder [Baqsimi] by Abacus Medicine", "glucagon Nasal Powder", "glucagon Nasal Powder [Baqsimi]", "glucose / potassium / sodium Nasal Powder", "nitrogen 99 % Topical Liquefied Gas", "nitrogen 99.2 % Topical Liquefied Gas", "nitrogen Topical Liquefied Gas", "potassium sodium tartrate", "sodium chloride Nasal Powder", "sodium chloride Nasal Powder [Neilmed Sinus Rins]", "sodium potassium tartrate tetrahydrate", "sumatriptan 11 MG Nasal Powder", "sumatriptan 11 MG Nasal Powder [Onzetra]", "sumatriptan Nasal Powder", "sumatriptan Nasal Powder [Onzetra]"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarise_large_scale_characteristics_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "percentage"),
                    selected = c("count", "percentage"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                )
              )
            ),
            bslib::navset_card_tab(
              bslib::nav_panel(
                title = "Raw",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarise_large_scale_characteristics_raw")
                )
              ),
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
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarise_large_scale_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
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
                      shiny::downloadButton(outputId = "summarise_large_scale_characteristics_formatted_download", label = "Download word")
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

# background

    Code
      createBackground(full)
    Output
      [1] "bslib::nav_panel(\n  title = \"Background\",\n  icon = shiny::icon(\"disease\"),\n  bslib::card(bslib::card_header(shiny::markdown('Abstract')), bslib::card_title(shiny::markdown('**Introduction**')), shiny::p(shiny::markdown('Example of an [introduction](https://github.com/oxford-pharmacoepi/omopViewer).')), bslib::card_title(shiny::markdown('Methods')), shiny::p(shiny::markdown('Methods example, with a footer* example.')), bslib::card_footer(shiny::markdown('*Here is the footer.')))\n)"

---

    Code
      createBackground(full, "HDS")
    Output
      [1] "bslib::nav_panel(\n  title = \"Background\",\n  icon = shiny::icon(\"disease\"),\n  bslib::card(bslib::card_header(shiny::markdown('Abstract')), bslib::card_title(shiny::markdown('**Introduction**')), shiny::p(shiny::markdown('Example of an [introduction](https://github.com/oxford-pharmacoepi/omopViewer).')), bslib::card_title(shiny::markdown('Methods')), shiny::p(shiny::markdown('Methods example, with a footer* example.')), bslib::card_footer(shiny::markdown('*Here is the footer.')),\nshiny::tags$img(\n  src = \"HDS\",\n  width = \"auto\",\n  height = \"100px\",\n  alt = \"logo\",\n  align = \"left\"\n))\n)"

# title

    Code
      cat(x, sep = "\n")
    Output
      # Generated by omopViewer 0.0.0.900
      # Be careful editing this file
      
      ui <- bslib::page_navbar(
        title = shiny::tags$span(
          shiny::tags$img(
            src = "hds_logo.svg",
            width = "auto",
            height = "46px",
            class = "me-3",
            alt = "logo"
          ),
          "example"
        ), ,
        bslib::nav_panel(
          title = "Summary",
          icon = shiny::icon("file-alt"),
          bslib::card(
            bslib::card_header("Summary of results"),
            shiny::p(shiny::markdown("A summarised_result object with **0** rows, **0** different result_id, different cdm names, and **3** settings.")), shiny::p(shiny::markdown(" - **Settings:** result_type, package_name, and package_version.")),
            shiny::tags$img(
              src = "hds_logo.svg",
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

