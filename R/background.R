
#' Create a `bslib::card()` object from a `.md` file.
#'
#' @param fileName Name of the .md file.
#'
#' @return Ui `bslib::card` with the background details.
#' @export
#'
cardFromMd <- function(fileName) {
  # validate file
  if (!validateFileBackground(fileName)) {
    cli::cli_warn(c(
      "!" = "background not created, fileName must be the name of an `.md` file"
    ))
    return(bslib::card())
  }

  # read file
  content <- readLines(fileName)

  # extract yaml metadata
  content <- extractYamlMetadata(content)
  metadata <- content$metadata
  body <- content$body

  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # metadata referring to keys
    getCardKeys(metadata),
    # body
    list(shiny::markdown(body))
  )

  do.call(bslib::card, arguments)
}

validateFileBackground <- function(fileName) {
  if (!is.character(fileName)) return(FALSE)
  if (length(fileName) != 1) return(FALSE)
  if (tools::file_ext(fileName) != "md") return(FALSE)
  if (!file.exists(fileName)) return(FALSE)
  return(TRUE)
}
extractYamlMetadata <- function(content) {
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]

  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }

  return(list(body = content, metadata = metadata))
}
getCardKeys <- function(metadata) {
  x <- list()
  for (key in backgroundKeywords$keyword) {
    if (key %in% names(metadata)) {
      y <- paste0(
        backgroundKeywords$fun[backgroundKeywords$keyword == key],
        "(metadata[[key]])"
      ) |>
        rlang::parse_expr() |>
        rlang::eval_tidy()
      x <- c(x, list(y))
    }
  }

  return(x)
}

createBackground <- function(background) {
  if (!background) return(character())
  'bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
    omopViewer::cardFromMd("background.md")
  )'
}

defaultBackground <- function(logo = NULL) {
  if (is.null(logo)) {
    logo <- character()
  } else {
    logo <- c('', paste0('<img src="', logo, '" width="100px">'))
  }
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
    logo,
    ''
  )
}
