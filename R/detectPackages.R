
detectPackages <- function(code) {
  double_colon_regex <- "\\b([a-zA-Z0-9\\.]+)::"

  double_colon_matches <- regmatches(code, gregexpr(double_colon_regex, code))

  libraries <- unlist(double_colon_matches)

  libraries <- gsub("::", "", libraries)
  libraries <- unique(libraries)

  return(libraries)
}
checkInstalledPackages <- function(x) {

  notInstalled <- x[!purrr::map_lgl(x, rlang::is_installed)]
  if (length(notInstalled) > 0) {
    cli::cli_warn(c(
      "!" = "{length(notInstalled)} packages {?is/are} not installed: {.pkg {notInstalled}}."
    ))
    install <- paste0(notInstalled, collapse = '", "')
    cli::cli_inform(c("i" = '{.run install.packages(c("{install}"))}'))
  }

  return(invisible(NULL))
}
