
# ui ----
formattedUi <- function(tab, choic) {
  hide <- names(choic$settings)
  none <- c(names(choic$groupping), "variable_name", "variable_level",
            "estimate_name")
  header <- "cdm_name"
  header <- header[header %in% none]
  none <- none[!none %in% header]
  group <- "cohort_name"
  group <- group[group %in% none]
  none <- none[!none %in% group]
  id <- paste0(tab, "_formatted_download")
  'bslib::nav_panel(
    title = "Formatted",
    bslib::card(
      full_screen = TRUE,
      {downloadTable(id, "Download", c("docx", "png", "pdf", "html"))},
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          sortable::bucket_list(
            header = NULL,
            sortable::add_rank_list(
              text = "None",
              labels = {cast(none)},
              input_id = "{tab}_formatted_none"
            ),
            sortable::add_rank_list(
              text = "Header",
              labels = {cast(header)},
              input_id = "{tab}_formatted_header"
            ),
            sortable::add_rank_list(
              text = "Group",
              labels = {cast(group)},
              input_id = "{tab}_formatted_group"
            ),
            sortable::add_rank_list(
              text = "Hide",
              labels = {cast(hide)},
              input_id = "{tab}_formatted_hide"
            )
          ),
          position = "right"
        ),
        gt::gt_output("{tab}_formatted")
      )
    )
  )' |>
    glue::glue() |>
    as.character()
}

# server ----
formattedServer <- function(rt, data) {
  c('getFormattedData[formatCamel(rt)] <- shiny::reactive({
      [data] |>
        filterData("[rt]", input) |>
        visTable(
          header = input$[rt]_formatted_header,
          group = input$[rt]_formatted_group,
          hide = input$[rt]_formatted_hide
        )
    })',
    'output$[rt]_formatted <- gt::render_gt({
      getFormattedData[formatCamel(rt)]()
    })',
    'output$[rt]_formatted_download <- shiny::downloadHandler(
      filename = paste0("formatted_[rt].", input$[rt]_type),
      content = function(file) {
        getFormattedData[formatCamel(rt)]() |>
          gt::gtsave(filename = file)
      }
    )'
  ) |>
    purrr::map_chr(\(x) glue::glue(x, .open = "[", .close = "]"))
}
