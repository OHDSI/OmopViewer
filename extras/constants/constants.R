import("shiny")

# STUDIES_DATA <- readRDS("./data/DATA_FAKE.rds")
# CURRENT_STUDY_DATA <- readRDS("./data/DATA_FAKE.rds")
# APP_DATA <- readRDS("./data/DATA_FAKE.rds")
APP_TITLE <- "Test of PhenotypeR app"
APP_TIME_RANGE <- "Apr 2024"
APP_VERSION <- "0.0.1"

hds_website <- "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology"
marketplace_website <- "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology"
marketplace_name <- "HDS website"

COLORS <- list(
  white = "#FFF",
  black = "#0a1e2b",
  primary = "#0099F9",
  secondary = "#15354A",
  ash = "#B3B8BA",
  ash_light = "#e3e7e9"
)

hds_logo <- tags$a(
  href = hds_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  shiny::tags$img(src = system.file("www/images/hds_logo_noline.svg", package = "omopViewer"),
      class = "logo-img", alt = "HDS Logo",
      width="20", height="20")
)

