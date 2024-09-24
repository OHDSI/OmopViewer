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
        ),
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          bslib::card(
            bslib::card_header("My study background"),
            shiny::p("You can use this section to add some background of your study"),
            shiny::tags$img(
              src = "my_pic.png",
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

# empty shiny

    Code
      cat(uiStatic(), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = "My study",
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          bslib::card(
            bslib::card_header("My study background"),
            shiny::p("You can use this section to add some background of your study")
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
      cat(serverStatic(), sep = "\n")
    Output
      server <- function(input, output, session) {
      }

# CohortCharacteristics shiny

    Code
      cat(uiStatic(choices = getChoices(result)), sep = "\n")
    Output
      ui <- bslib::page_navbar(
        title = "My study",
        bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          bslib::card(
            bslib::card_header("My study background"),
            shiny::p("You can use this section to add some background of your study")
          )
        ),
        bslib::nav_panel(
          title = "Cohort attrition",
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
                    inputId = "cohort_attrition_settings_cohort_definition_id",
                    label = "Cohort definition id",
                    choices = c(1, 2, 3),
                    selected = c(1, 2, 3),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_attrition_settings_table_name",
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
                    inputId = "cohort_attrition_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_attrition_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_attrition_groupping_reason",
                    label = "Reason",
                    choices = c("Initial qualifying events"),
                    selected = c("Initial qualifying events"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_attrition_groupping_reason_id",
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
                    inputId = "cohort_attrition_variable_name",
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
                    inputId = "cohort_attrition_estimate_name",
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
                      shiny::downloadButton(outputId = "cohort_attrition_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("cohort_attrition_raw")
                )
              ),
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "cohort_attrition_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::checkboxInput(
                        inputId = "cohort_attrition_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "cohort_attrition_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
                      ),
                      shiny::radioButtons(
                        inputId = "cohort_attrition_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("cohort_attrition_tidy")
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
                      shiny::downloadButton(outputId = "cohort_attrition_formatted_download", label = "Download word")
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
                          input_id = "cohort_attrition_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "cohort_attrition_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "cohort_attrition_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("cohort_definition_id", "table_name"),
                          input_id = "cohort_attrition_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("cohort_attrition_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort overlap",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "cohort_overlap_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_overlap_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_overlap_groupping_cohort_name_comparator",
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
                    inputId = "cohort_overlap_variable_name",
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
                    inputId = "cohort_overlap_estimate_name",
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
                      shiny::downloadButton(outputId = "cohort_overlap_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("cohort_overlap_raw")
                )
              ),
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "cohort_overlap_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::checkboxInput(
                        inputId = "cohort_overlap_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "cohort_overlap_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
                      ),
                      shiny::radioButtons(
                        inputId = "cohort_overlap_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("cohort_overlap_tidy")
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
                      shiny::downloadButton(outputId = "cohort_overlap_formatted_download", label = "Download word")
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
                          input_id = "cohort_overlap_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "cohort_overlap_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "cohort_overlap_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = character(),
                          input_id = "cohort_overlap_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("cohort_overlap_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_panel(
          title = "Cohort timing",
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
                    inputId = "cohort_timing_settings_restrict_to_first_entry",
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
                    inputId = "cohort_timing_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_timing_groupping_cohort_name_reference",
                    label = "Cohort name reference",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "cohort_timing_groupping_cohort_name_comparator",
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
                    inputId = "cohort_timing_variable_name",
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
                    inputId = "cohort_timing_estimate_name",
                    label = "Estimate name",
                    choices = c("count", "max", "median", "min", "q25", "q75"),
                    selected = c("count", "max", "median", "min", "q25", "q75"),
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
                      shiny::downloadButton(outputId = "cohort_timing_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("cohort_timing_raw")
                )
              ),
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "cohort_timing_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::checkboxInput(
                        inputId = "cohort_timing_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "cohort_timing_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
                      ),
                      shiny::radioButtons(
                        inputId = "cohort_timing_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("cohort_timing_tidy")
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
                      shiny::downloadButton(outputId = "cohort_timing_formatted_download", label = "Download word")
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
                          input_id = "cohort_timing_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "cohort_timing_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = character(),
                          input_id = "cohort_timing_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("restrict_to_first_entry"),
                          input_id = "cohort_timing_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("cohort_timing_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_panel(
          title = "Summarised characteristics",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Information",
                  icon = shiny::icon("info"),
                  shiny::p("")
                ),
                bslib::accordion_panel(
                  title = "Groupping",
                  shiny::selectizeInput(
                    inputId = "summarised_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_characteristics_groupping_cohort_name",
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
                    inputId = "summarised_characteristics_variable_name",
                    label = "Variable name",
                    choices = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    selected = c("Age", "Cohort end date", "Cohort start date", "Future observation", "Number records", "Number subjects", "Prior observation", "Sex"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  )
                ),
                bslib::accordion_panel(
                  title = "Estimates",
                  shiny::selectizeInput(
                    inputId = "summarised_characteristics_estimate_name",
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
                      shiny::downloadButton(outputId = "summarised_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarised_characteristics_raw")
                )
              ),
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarised_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::checkboxInput(
                        inputId = "summarised_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarised_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
                      ),
                      shiny::radioButtons(
                        inputId = "summarised_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarised_characteristics_tidy")
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
                      shiny::downloadButton(outputId = "summarised_characteristics_formatted_download", label = "Download word")
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
                          input_id = "summarised_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarised_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarised_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = character(),
                          input_id = "summarised_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarised_characteristics_formatted")
                  )
                )
              ),
            )
          )
        ),
        bslib::nav_panel(
          title = "Summarised large scale characteristics",
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
                    inputId = "summarised_large_scale_characteristics_settings_table_name",
                    label = "Table name",
                    choices = c("condition_occurrence", "drug_exposure"),
                    selected = c("condition_occurrence", "drug_exposure"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_settings_type",
                    label = "Type",
                    choices = c("episode", "event"),
                    selected = c("episode", "event"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_settings_analysis",
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
                    inputId = "summarised_large_scale_characteristics_groupping_cdm_name",
                    label = "Cdm name",
                    choices = c("mock database"),
                    selected = c("mock database"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_groupping_cohort_name",
                    label = "Cohort name",
                    choices = c("asthma", "covid", "tb"),
                    selected = c("asthma", "covid", "tb"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_groupping_sex",
                    label = "Sex",
                    choices = c("Female", "Male", "overall"),
                    selected = c("Female", "Male", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_groupping_age_group",
                    label = "Age group",
                    choices = c("0 to 44", "45 or above", "overall"),
                    selected = c("0 to 44", "45 or above", "overall"),
                    multiple = TRUE,
                    options = list(plugins = "remove_button")
                  ),
                  shiny::selectizeInput(
                    inputId = "summarised_large_scale_characteristics_groupping_concept_id",
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
                    inputId = "summarised_large_scale_characteristics_variable_name",
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
                    inputId = "summarised_large_scale_characteristics_estimate_name",
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
                      shiny::downloadButton(outputId = "summarised_large_scale_characteristics_raw_download", label = "Download summarised_result")
                    ),
                    class = "text-end"
                  ),
                  DT::dataTableOutput("summarised_large_scale_characteristics_raw")
                )
              ),
              bslib::nav_panel(
                title = "Tidy",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    bslib::popover(
                      shiny::icon("download"),
                      shiny::downloadButton(outputId = "summarised_large_scale_characteristics_tidy_download", label = "Download csv")
                    ),
                    class = "text-end"
                  ),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      shiny::checkboxInput(
                        inputId = "summarised_large_scale_characteristics_tidy_settings",
                        label = "Show settings",
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "summarised_large_scale_characteristics_tidy_groupping",
                        label = "Show groupping",
                        value = TRUE
                      ),
                      shiny::radioButtons(
                        inputId = "summarised_large_scale_characteristics_tidy_pivot",
                        label = "Pivot estimates/variables",
                        choices = c("none", "estimates", "estimates and variables"),
                        selected = "none"
                      ),
                      position = "right"
                    ),
                    DT::dataTableOutput("summarised_large_scale_characteristics_tidy")
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
                      shiny::downloadButton(outputId = "summarised_large_scale_characteristics_formatted_download", label = "Download word")
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
                          input_id = "summarised_large_scale_characteristics_formatted_none"
                        ),
                        sortable::add_rank_list(
                          text = "Header",
                          labels = c("cdm_name"),
                          input_id = "summarised_large_scale_characteristics_formatted_header"
                        ),
                        sortable::add_rank_list(
                          text = "Group",
                          labels = c("cohort_name"),
                          input_id = "summarised_large_scale_characteristics_formatted_group"
                        ),
                        sortable::add_rank_list(
                          text = "Hide",
                          labels = c("table_name", "type", "analysis"),
                          input_id = "summarised_large_scale_characteristics_formatted_hide"
                        )
                      ),
                      position = "right"
                    ),
                    gt::gt_output("summarised_large_scale_characteristics_formatted")
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
        # cohort_attrition -----
        ## raw cohort_attrition -----
        getRawDataCohortAttrition <- shiny::reactive({
          filterData(data, "cohort_attrition", input)
        })
        output$cohort_attrition_raw <- DT::renderDT({
          DT::datatable(getRawDataCohortAttrition(), options = list(scrollX = TRUE))
        })
        output$cohort_attrition_raw_download <- shiny::downloadHandler(
          filename = "raw_cohort_attrition.csv",
          content = function(file) {
            getRawDataCohortAttrition() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy cohort_attrition -----
        getTidyDataCohortAttrition <- shiny::reactive({
          data |>
            filterData("cohort_attrition", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_attrition_tidy_settings,
              showGroupping = input$cohort_attrition_tidy_groupping,
              pivot = input$cohort_attrition_tidy_pivot
            )
        })
        output$cohort_attrition_tidy <- DT::renderDT({
          DT::datatable(getTidyDataCohortAttrition(), options = list(scrollX = TRUE))
        })
        output$cohort_attrition_tidy_download <- shiny::downloadHandler(
          filename = "tidy_cohort_attrition.csv",
          content = function(file) {
            getTidyDataCohortAttrition() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted cohort_attrition -----
        getFormattedDataCohortAttrition <- shiny::reactive({
          data |>
            filterData("cohort_attrition", input) |>
            visTable(
              header = input$cohort_attrition_formatted_header,
              group = input$cohort_attrition_formatted_group,
              hide = input$cohort_attrition_formatted_hide
            )
        })
        output$cohort_attrition_formatted <- gt::render_gt({
          getFormattedDataCohortAttrition()
        })
        output$cohort_attrition_formatted_download <- shiny::downloadHandler(
          filename = "formatted_cohort_attrition.docx",
          content = function(file) {
            getFormattedDataCohortAttrition() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot cohort_attrition -----
      
      
        # cohort_overlap -----
        ## raw cohort_overlap -----
        getRawDataCohortOverlap <- shiny::reactive({
          filterData(data, "cohort_overlap", input)
        })
        output$cohort_overlap_raw <- DT::renderDT({
          DT::datatable(getRawDataCohortOverlap(), options = list(scrollX = TRUE))
        })
        output$cohort_overlap_raw_download <- shiny::downloadHandler(
          filename = "raw_cohort_overlap.csv",
          content = function(file) {
            getRawDataCohortOverlap() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy cohort_overlap -----
        getTidyDataCohortOverlap <- shiny::reactive({
          data |>
            filterData("cohort_overlap", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_overlap_tidy_settings,
              showGroupping = input$cohort_overlap_tidy_groupping,
              pivot = input$cohort_overlap_tidy_pivot
            )
        })
        output$cohort_overlap_tidy <- DT::renderDT({
          DT::datatable(getTidyDataCohortOverlap(), options = list(scrollX = TRUE))
        })
        output$cohort_overlap_tidy_download <- shiny::downloadHandler(
          filename = "tidy_cohort_overlap.csv",
          content = function(file) {
            getTidyDataCohortOverlap() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted cohort_overlap -----
        getFormattedDataCohortOverlap <- shiny::reactive({
          data |>
            filterData("cohort_overlap", input) |>
            visTable(
              header = input$cohort_overlap_formatted_header,
              group = input$cohort_overlap_formatted_group,
              hide = input$cohort_overlap_formatted_hide
            )
        })
        output$cohort_overlap_formatted <- gt::render_gt({
          getFormattedDataCohortOverlap()
        })
        output$cohort_overlap_formatted_download <- shiny::downloadHandler(
          filename = "formatted_cohort_overlap.docx",
          content = function(file) {
            getFormattedDataCohortOverlap() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot cohort_overlap -----
      
      
        # cohort_timing -----
        ## raw cohort_timing -----
        getRawDataCohortTiming <- shiny::reactive({
          filterData(data, "cohort_timing", input)
        })
        output$cohort_timing_raw <- DT::renderDT({
          DT::datatable(getRawDataCohortTiming(), options = list(scrollX = TRUE))
        })
        output$cohort_timing_raw_download <- shiny::downloadHandler(
          filename = "raw_cohort_timing.csv",
          content = function(file) {
            getRawDataCohortTiming() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy cohort_timing -----
        getTidyDataCohortTiming <- shiny::reactive({
          data |>
            filterData("cohort_timing", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$cohort_timing_tidy_settings,
              showGroupping = input$cohort_timing_tidy_groupping,
              pivot = input$cohort_timing_tidy_pivot
            )
        })
        output$cohort_timing_tidy <- DT::renderDT({
          DT::datatable(getTidyDataCohortTiming(), options = list(scrollX = TRUE))
        })
        output$cohort_timing_tidy_download <- shiny::downloadHandler(
          filename = "tidy_cohort_timing.csv",
          content = function(file) {
            getTidyDataCohortTiming() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted cohort_timing -----
        getFormattedDataCohortTiming <- shiny::reactive({
          data |>
            filterData("cohort_timing", input) |>
            visTable(
              header = input$cohort_timing_formatted_header,
              group = input$cohort_timing_formatted_group,
              hide = input$cohort_timing_formatted_hide
            )
        })
        output$cohort_timing_formatted <- gt::render_gt({
          getFormattedDataCohortTiming()
        })
        output$cohort_timing_formatted_download <- shiny::downloadHandler(
          filename = "formatted_cohort_timing.docx",
          content = function(file) {
            getFormattedDataCohortTiming() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot cohort_timing -----
      
      
        # summarised_characteristics -----
        ## raw summarised_characteristics -----
        getRawDataSummarisedCharacteristics <- shiny::reactive({
          filterData(data, "summarised_characteristics", input)
        })
        output$summarised_characteristics_raw <- DT::renderDT({
          DT::datatable(getRawDataSummarisedCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarised_characteristics_raw_download <- shiny::downloadHandler(
          filename = "raw_summarised_characteristics.csv",
          content = function(file) {
            getRawDataSummarisedCharacteristics() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarised_characteristics -----
        getTidyDataSummarisedCharacteristics <- shiny::reactive({
          data |>
            filterData("summarised_characteristics", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarised_characteristics_tidy_settings,
              showGroupping = input$summarised_characteristics_tidy_groupping,
              pivot = input$summarised_characteristics_tidy_pivot
            )
        })
        output$summarised_characteristics_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummarisedCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarised_characteristics_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarised_characteristics.csv",
          content = function(file) {
            getTidyDataSummarisedCharacteristics() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarised_characteristics -----
        getFormattedDataSummarisedCharacteristics <- shiny::reactive({
          data |>
            filterData("summarised_characteristics", input) |>
            visTable(
              header = input$summarised_characteristics_formatted_header,
              group = input$summarised_characteristics_formatted_group,
              hide = input$summarised_characteristics_formatted_hide
            )
        })
        output$summarised_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummarisedCharacteristics()
        })
        output$summarised_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarised_characteristics.docx",
          content = function(file) {
            getFormattedDataSummarisedCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarised_characteristics -----
      
      
        # summarised_large_scale_characteristics -----
        ## raw summarised_large_scale_characteristics -----
        getRawDataSummarisedLargeScaleCharacteristics <- shiny::reactive({
          filterData(data, "summarised_large_scale_characteristics", input)
        })
        output$summarised_large_scale_characteristics_raw <- DT::renderDT({
          DT::datatable(getRawDataSummarisedLargeScaleCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarised_large_scale_characteristics_raw_download <- shiny::downloadHandler(
          filename = "raw_summarised_large_scale_characteristics.csv",
          content = function(file) {
            getRawDataSummarisedLargeScaleCharacteristics() |>
              readr::write_csv(file = file)
            # TBR by exportSummarisedResult
          }
        )
        ## tidy summarised_large_scale_characteristics -----
        getTidyDataSummarisedLargeScaleCharacteristics <- shiny::reactive({
          data |>
            filterData("summarised_large_scale_characteristics", input) |>
            tidyData(
              prefixSet = "set:",
              prefixGroup = "group: ",
              showSettings = input$summarised_large_scale_characteristics_tidy_settings,
              showGroupping = input$summarised_large_scale_characteristics_tidy_groupping,
              pivot = input$summarised_large_scale_characteristics_tidy_pivot
            )
        })
        output$summarised_large_scale_characteristics_tidy <- DT::renderDT({
          DT::datatable(getTidyDataSummarisedLargeScaleCharacteristics(), options = list(scrollX = TRUE))
        })
        output$summarised_large_scale_characteristics_tidy_download <- shiny::downloadHandler(
          filename = "tidy_summarised_large_scale_characteristics.csv",
          content = function(file) {
            getTidyDataSummarisedLargeScaleCharacteristics() |>
              readr::write_csv(file = file)
          }
        )
        ## formatted summarised_large_scale_characteristics -----
        getFormattedDataSummarisedLargeScaleCharacteristics <- shiny::reactive({
          data |>
            filterData("summarised_large_scale_characteristics", input) |>
            visTable(
              header = input$summarised_large_scale_characteristics_formatted_header,
              group = input$summarised_large_scale_characteristics_formatted_group,
              hide = input$summarised_large_scale_characteristics_formatted_hide
            )
        })
        output$summarised_large_scale_characteristics_formatted <- gt::render_gt({
          getFormattedDataSummarisedLargeScaleCharacteristics()
        })
        output$summarised_large_scale_characteristics_formatted_download <- shiny::downloadHandler(
          filename = "formatted_summarised_large_scale_characteristics.docx",
          content = function(file) {
            getFormattedDataSummarisedLargeScaleCharacteristics() |>
              gt::gtsave(filename = file)
          }
        )
        ## plot summarised_large_scale_characteristics -----
      }

