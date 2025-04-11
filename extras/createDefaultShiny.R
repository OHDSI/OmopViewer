
rlang::check_installed("devtools")
devtools::load_all()

exportStaticApp(result = omopViewerResults, directory = here::here("extras"))
