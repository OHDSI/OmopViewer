
availableThemes <- function() {
  list(
    # visOmopResults themes
    visOmopResults = system.file("brand", package = "visOmopResults") |>
      list.files() |>
      purrr::keep(\(x) stringr::str_ends(string = x, pattern = "\\.yml")) |>
      stringr::str_replace_all(pattern = "\\.yml$", replacement = ""),
    # OmopViewer themes
    OmopViewer = system.file("brand", package = "OmopViewer") |>
      list.files() |>
      purrr::keep(\(x) stringr::str_ends(string = x, pattern = "\\.yml")) |>
      stringr::str_replace_all(pattern = "\\.yml$", replacement = "")
  )
}

validateTheme <- function(theme, call = parent.frame()) {
  if (is.null(theme)) theme <-  "default"

  omopgenerics::assertCharacter(x = theme, length = 1, call = call)

  if (endsWith(x = theme, suffix = ".yml")) {
    at <- availableThemes()
    choices <- unlist(at, use.names = FALSE)
    omopgenerics::assertChoice(x = theme, choices = choices, length = 1, call = call)
    pkg <- ifelse(theme %in% at$visOmopResults, "visOmopResults", "OmopViewer")
    file <- system.file("brand", paste0(theme, ".yml"), package = pkg)
  } else {
    if (!file.exists(theme)) {
      cli::cli_abort(c(x = "File {.path {theme}} does not exist."))
    }
    file <- theme
    pkg <- "none"
  }

  # read theme
  content <- yaml::read_yaml(file = file)

  # correct visOmopResults themes
  if (pkg == "visOmopResults") {
    content <- correctTheme(content = content, name = theme)
  }

  return(content)
}
correctTheme <- function(content, name) {

}
