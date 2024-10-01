
detectPackages <- function(code) {
  double_colon_regex <- "\\b([a-zA-Z0-9\\.]+)::"

  double_colon_matches <- regmatches(code, gregexpr(double_colon_regex, code))

  libraries <- unlist(double_colon_matches)

  libraries <- gsub("::", "", libraries)
  libraries <- unique(libraries)
  if (length(libraries) == 0) {
    return("")
  }

  library_statements_list <- paste0("library(", libraries, ")")
  library_statements_list <- c(library_statements_list, "")
  return(styleCode(library_statements_list))
}

