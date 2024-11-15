
createSidebar <- function(prefix, filters, information) {
  content <- c(
    sidebarInformation(information),
    sidebarSettings(prefix, filters),
    sidebarGrouping(prefix, filters),
    sidebarVariables(prefix, filters),
    sidebarEstimates(prefix, filters)
  ) |>
    paste0(collapse = ",\n")
  'sidebar = bslib::sidebar(
    bslib::accordion(
      {content}
    )
  ),' |>
    glue::glue() |>
    as.character()
}
sidebarInformation <- function(information) {
  if (length(information) > 0) return(character())
  information <- cast(information)
  'bslib::accordion_panel(
    title = "Information",
    icon = shiny::icon("info"),
    shiny::p({information})
  )' |>
    glue::glue() |>
    as.character()
}
sidebarSettings <- function(prefix, filters) {
  filters <- filters[startsWith(filters, "settings_")]
  if (length(filters) == 0) return(character())
  filters <- substr(filters, 10, nchar(filters))
  writeSidebar(
    title = "Settings",
    prefix = paste0(prefix, "_settings"),
    filters = filters
  )
}
sidebarGrouping <- function(prefix, filters) {
  filters <- filters[startsWith(filters, "grouping_")]
  if (length(filters) == 0) return(character())
  filters <- substr(filters, 10, nchar(filters))
  writeSidebar(
    title = "Grouping",
    prefix = paste0(prefix, "_grouping"),
    filters = filters
  )
}
sidebarVariables <- function(prefix, filters) {
  if (!"variable_name" %in% filters) return(character())
  writeSidebar(title = "Variables", prefix = prefix, filters = "variable_name")
}
sidebarEstimates <- function(prefix, filters) {
  if (!"estimate_name" %in% filters) return(character())
  writeSidebar(title = "Estimates", prefix = prefix, filters = "estimate_name")
}
writeSidebar <- function(title, prefix, filters) {
  selectors <- purrr::map_chr(filters, \(x) {
    id <- paste0(prefix, "_", x)
    def <- paste0("filterValues$", id)
    selector(id, formatTit(x), def, def, TRUE)
  }) |>
    paste0(collapse = ",\n")
  'bslib::accordion_panel(
    title = "{title}",
    {selectors}
  )' |>
    glue::glue() |>
    as.character()
}
