

# Advanced dashboard tab, possibly serving as a summary or parent tab
advancedDashboardTab <- div(
  class = "container-fluid",
  div(
    class = "row",
    div(
      class = "col-md-12",
      h2("Advanced Dashboard"),
      p("This is the Advanced Dashboard area. Select a specific analysis from the sidebar."),
      # Any additional summary or overview components can go here
    )
  )
)


cohortAnalysisTab <- fluidPage(
  addSharedResources(),
  div(
    class = "container-fluid",
    div(
      class = "row",
      div(
        class = "col-md-12",
        createCard(
          id = "cohortContent_advanced",
          title = "Cohort Counts",
          filter_ui = filter_function$filter_module_ui("cohort_filter_advanced"),
          table_ui = table_function$ui("cohort_count_view_advanced")
        )
        # Add more cards as needed
      )
    )
  )
)


cohortDefinitionTab <- 
  fluidPage(
    addSharedResources(),
    div(
      class = "container-fluid",
      div(
        class = "row",
        div(
          class = "col-md-12",
          createCard(
            id = "cohortDefinitionContent",
            title = "Cohort Definition",
            filter_ui = filter_function$filter_module_ui("cohort_definition_filter"),
            table_ui = table_function$ui("cohort_definition_view")
            # extra_ui = graph_cohort_overlap$ui("cohort_overlap_plot_view")
          )
          # Add more cards as needed
        )
      )
    )
  )

cohortOverlapTab <- 
  fluidPage(
    addSharedResources(),
    div(
      class = "container-fluid",
      div(
        class = "row",
        div(
          class = "col-md-12",
          createCard(
            id = "cohortOverlapContent",
            title = "Incidence",
            filter_ui = filter_function$filter_module_ui("cohort_overlap_filter"),
            table_ui = table_function$ui("cohort_overlap_view")
            # extra_ui = graph_cohort_overlap$ui("cohort_overlap_plot_view")
          )
          # Add more cards as needed
        )
      )
    )
  )


incidenceAnalysisTab <-   fluidPage(
  addSharedResources(),
  div(
    class = "container-fluid",
    div(
      class = "row",
      div(
        class = "col-md-12",
        createCard(
          id = "incidenceContent_advanced",
          title = "Incidence",
          filter_ui = filter_function$filter_module_ui("incidence_filter_advanced"),
          table_ui = table_function$ui("incidence_view_advanced"),
          extra_ui = graph_incidence$ui("incidence_plot_view_advanced")
        )
        # Add more cards as needed
      )
    )
  )
)
  
prevalenceAnalysisTab <- fluidPage(
  addSharedResources(),
  div(
    class = "container-fluid",
    div(
      class = "row",
      div(
        class = "col-md-12",
        createCard(
          id = "prevalenceContent",
          title = "Prevalence",
          filter_ui = filter_function$filter_module_ui("prevalence_filter"),
          table_ui = table_function$ui("prevalence_view"),
          extra_ui = graph_prevalence$ui("prevalence_plot_view")
        )
        # Add more cards as needed
      )
    )
  )
)
