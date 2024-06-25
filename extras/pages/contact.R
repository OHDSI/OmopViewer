contactTab <- div(
  class = "contact",
  img(
    src = "images/hds_logo.svg",
    class = "logo-img",
    alt = "Appsilon Logo",
    height = "50%",
    width = "50%"
  ),
  tags$h4(
    tags$span("Visit us on "),
    tags$a(
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank",
      rel = "nofollow noreferrer",
      "HDS website"
    )
  )
)
