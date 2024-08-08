contactTab <- function() {
  shiny::div(
  class = "contact",
  shiny::img(
    src = system.file("www/images/hds_logo.svg", package = "omopViewer"),
    class = "logo-img",
    height = "50%",
    width = "50%"
  ),
  shiny::tags$h4(
    shiny::tags$span("Visit us on "),
    shiny::tags$a(
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank",
      rel = "nofollow noreferrer",
      "HDS website"
    )
  )
)
}
