if (!rlang::is_installed("rsconnect")) {
  install.packages("rsconnect")
}
rsconnect::setAccountInfo(name = "dpa-pde-oxford",
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(appDir = "extras/shiny", appName = "OmopViewerExample")
