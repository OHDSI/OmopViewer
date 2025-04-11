# DO NOT EDIT
# THIS FILE IS FOR THE GITHUB action

devtools::load_all()
exportStaticApp(result = omopViewerResults, directory = here::here("extras"))
rsconnect::setAccountInfo(name = "dpa-pde-oxford",
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(appDir = "extras/shiny", appName = "OmopViewerExample", forceUpdate = TRUE)
