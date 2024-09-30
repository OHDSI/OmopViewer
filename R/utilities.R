validateSummary <- function(summary, result){
  if (summary) {
    sum <- utils::capture.output(summary(result), type = "message")
  } else {
    sum <- NULL
  }
}
