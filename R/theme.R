
#' List the available configured `OmopViewer` themes
#'
#' @return A character vector with the available `OmopViewer` themes.
#' @export
#'
#' @examples
#' library(OmopViewer)
#'
#' omopViewerThemes()
#'
omopViewerThemes <- function() {
  unlist(availableThemes(), use.names = FALSE)
}

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

  if (!endsWith(x = theme, suffix = ".yml")) {
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
  content <- readBrand(file = file)

  # correct visOmopResults themes
  if (pkg == "visOmopResults") {
    content <- correctTheme(content = content, theme = theme)
  }

  return(content)
}
correctTheme <- function(content, theme) {
  file <- system.file("brand", "complement", paste0(theme, ".yml"), package = "OmopViewer")
  if (file.exists(file)) {
    content <- utils::modifyList(content, readBrand(file = file))
  }
  content
}
readBrand <- function(file = "_brand.yml") {
  content <- yaml::read_yaml(file = file)
  if ("brand" %in% names(content)) {
    content <- content$brand
  }
  return(content)
}
getThemes <- function() {
  omopViewerThemes() |>
    rlang::set_names() |>
    purrr::map(\(x) bslib::bs_theme(brand = validateTheme(x)))
}
