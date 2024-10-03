
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
    qty <- length(needed_packages)

    # Combine package names without quotes for display
    packages_list <- paste0("{.pkg ", needed_packages, "}", collapse = ", ")

    cli::cli_warn(
      c("i" = sprintf(
        "The following package%s missing: %s.",
        if (qty > 1) "s are" else " is",
        packages_list
      ))
    )

    cli::cli_warn(
      c("i" = sprintf(
        "\nRun {.run install.packages(%s)} to install %s.",
        if (qty > 1) {
          sprintf("c(%s)", paste(sprintf("\"%s\"", needed_packages), collapse = ", "))
        } else {
          sprintf("\"%s\"", needed_packages)
        },
        if (qty > 1) "them" else "it"
      ))
    )
  }



}
