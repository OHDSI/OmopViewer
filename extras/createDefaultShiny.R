
if (!rlang::is_installed("devtools")) {
  install.packages("devtools")
}
devtools::load_all()

exportStaticApp(result = omopViewerResults, directory = here::here("extras"))
