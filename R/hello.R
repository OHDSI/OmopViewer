#' It says hello to whoever you want, by default world.
#'
#' @param nam name to say hello
#'
#' @return a message
#'
#' @export
#'
#' @examples
#' library(omopViewer)
#' hello()
#'
hello <- function(nam = NULL) {
  if (is.null(nam)) {
    nam <- "world"
  }
  print(paste0("Hello, ", nam))
}
