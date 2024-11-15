
validateFileBackground <- function(fileName) {
  if (!is.character(fileName)) return(FALSE)
  if (length(fileName) != 1) return(FALSE)
  if (tools::file_ext(fileName) != "md") return(FALSE)
  if (!file.exists(fileName)) return(FALSE)
  return(TRUE)
}
createBackground <- function(background) {
  if (!background) return(character())
  'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
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
