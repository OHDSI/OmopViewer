
detectPackages <- function(code) {
  double_colon_regex <- "\\b([a-zA-Z0-9\\.]+)::"

  double_colon_matches <- regmatches(code, gregexpr(double_colon_regex, code))

  libraries <- unlist(double_colon_matches)

  libraries <- gsub("::", "", libraries)
  libraries <- unique(libraries)
  if (length(libraries) == 0) {
    return()
  }

  return(libraries)
}
checkInstalledPackages <- function(libraries_vector) {

  installed_packages <- names(utils::installed.packages()[,3])
  flag_vector <-  !(libraries_vector %in% installed_packages)

  needed_packages <- libraries_vector[flag_vector]
  if (length(needed_packages) > 0) {
    cli::cli_warn(c(
      "!" = "{length(needed_packages)} packages {?is/are} not installed: {.pkg {needed_packages}}."
    ))
    install <- paste0(needed_packages, collapse = '", "')
    cli::cli_inform(c("i" = '{.run install.packages(c("{install}"))}'))
  }

  return(invisible(NULL))
}
