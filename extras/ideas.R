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
