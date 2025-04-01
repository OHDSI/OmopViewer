
styleCode <- function(x) {
  if (rlang::is_installed("styler")) {
    x <- styler::style_text(x)
  } else {
    cli::cli_inform(c("i" = "{.pkg styler} not installed, code not styled."))
  }
  x <- as.character(x)
  return(x)
}
