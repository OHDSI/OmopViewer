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

##### From old template
# metrics_list <- list(
#   shipments = list(
#     label = "Total shipments",
#     value = paste(sum(APP_DATA$state_data$total.shipments), "trips")
#   ),
#   weight = list(
#     label = "Total weight",
#     value = paste(format(sum(APP_DATA$state_data$total.weight), big.mark = ","), "tons")
#   ),
#   locations = list(
#     label = "Number of locations",
#     value = paste(sum(APP_DATA$state_data$number.of.locations), "locations")
#   ),
#   shipments_day = list(
#     label = "Shipments per day",
#     value = paste(sum(APP_DATA$state_data$shipments.day), "trips")
#   )
# )

hds_logo <- tags$a(
  href = hds_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  img(src = here::here("extras/www/images/hds_logo_noline.svg"),
      class = "logo-img", alt = "HDS Logo",
      width="20", height="20")
)


##### From old template - footer
# hds_footer <- tags$h4(
#   class = "footer-heading",
#   tags$span("Footer"),
#   tags$a(
#     class = "footer-link",
#     href = marketplace_website,
#     target = "_blank",
#     rel = "nofollow noreferrer",
#     marketplace_name
#   )
# )
