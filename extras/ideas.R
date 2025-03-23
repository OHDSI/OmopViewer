
shiny <- list(
  incidence = list(
    title = "Tab title",
    icon = "eye",
    filters = list(
      "cdm_name" = list(
        title = "CDM name",
        column_name = "cdm_name",
        button_type = "pickerInput",
        selected = "<cdm_name>",
        choices = "<cdm_name>",
        multiple = TRUE,
        fixed_keyword = "cohort_name"
      )
    ),
    content = list(
      tidy = list(
        title = "Tidy",
        output_type = "DT",
        render_function = "
      IncidencePrevalence::plotIncidence(
        result = x,
        facet = input$facet,
        colour = input$colour
      )",
        buttons = list(
          facet = list(
            title = "Facet",
            button_type = "pickerInput",
            selected = "<strata>",
            choices = "<strata>",
            multiple = TRUE
          )
        )
      )
    )
  )
)

fixKeyWords <- list(
  cohort_name = c("")
)
filters <- dplyr::tribble(
  ~data_id, ~input_id, ~column_name,
  "incidence", "incidence_cdm_name", "cdm_name"
)

createPanel <- function(template, data) {

}

# panel_details class
# export
panelDetailsFromResult()
# internal
newPanelDetails()
# examples
panel_details <- list(
  name = list(
    result_id = c(1, 2, 3),
    title = "Title",
    icon = "xxxx",
    sidebar_filters = list(
      name = list(
        column_name = "target_cohort_name",
        button_type = "pickerInput",
        selected = "<cohort_name>",
        choices = "<cohort_name>",
        multiple = TRUE,
        fixed_keyword = "cohort_name"
      )
    ),
    content = list(
      tidy  = list(
        title = "Tidy",
        output_type = "DT",
        render_content = '...',
        sidebar_inputs = list(
          columns = list(
            button_type = "pickerInput",
            selected = c("<settings>", "<grouping>"),
            default = c("<settings>", "<grouping>"),
            multiple = TRUE
          ),
          pivot_estimates = list(
            button_type = "checkbox",
            value = TRUE,
          )
        )
      ),
      name = list(
        title = "Title",
        output_type = "ggplot2",
        render_content = "",
        sidebar_inputs = list(
          header = list(
            button_type = "pickerInput",
            selected = "png"
          )
        ),
        download = list(
          file_name = "",
          buttons = ""
        )
      )
    )
  )
)

selected <- list(
  "name1_name2" = list(result_id = c(1, 2, 3), column_name = c("cohort_name")),
  "name1_namex" = list(result_id = 1, )
)

defaultPanel <- function(result, resultId) {

}
panel <- function(x, name, resultId) {

}

panelDetails$incidence_1 <- defaultPanel(result, resultId = 1:3)
panelDetails$incidence_2 <- defaultPanel(result, resultId = 4:5)
