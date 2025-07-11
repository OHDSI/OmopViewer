
validateFileBackground <- function(fileName) {
  if (!is.character(fileName)) return(FALSE)
  if (length(fileName) != 1) return(FALSE)
  if (tools::file_ext(fileName) != "md") return(FALSE)
  if (!file.exists(fileName)) return(FALSE)
  return(TRUE)
}
validateBackground <- function(background, logo, call = parent.frame()) {
  msg <- "'background' must be either TRUE/FALSE or a path to an existing `.md` file."
  if (is.logical(background)) {
    omopgenerics::assertLogical(background, length = 1, call = call, msg = msg)
    if (background) {
      background <- defaultBackground(logo = logo)
    } else {
      background <- NULL
    }
  } else if (is.character(background)) {
    omopgenerics::assertCharacter(background, length = 1, call = call, msg = msg)
    if (file.exists(background)) {
      background <- readLines(background)
    } else {
      cli::cli_abort(message = "background file ({.path {background}}) does not exist.", call = call)
    }
  } else {
    cli::cli_abort(message = msg, call = call)
  }
  return(background)
}
createBackground <- function(background) {
  if (is.null(background)) return(character())
  'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("book-atlas"),
    backgroundCard("background.md")
  )'
}
defaultBackground <- function(logo) {
  c(
    '-----',
    '# this block contain the metadata of the .md document, you can add here:',
    paste0('#   - background keys: ', paste0(backgroundKeywords$keyword, collapse = ', '), '.'),
    '#   - bslib::card arguments.',
    '# you can have more information how to use this section on the background',
    '# vingette',
    'header: "This is the header of the background"',
    '-----',
    '',
    '# Title',
    '## subtitle',
    'content',
    '',
    glue::glue('![](<logo>){width=100px}', .open = "<", .close = ">"),
    ''
  )
}
