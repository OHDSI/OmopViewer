validateSummary <- function(summary, result){
  if (summary) {
    sum <- capture.output(summary(result), type = "message")
  } else {
    sum <- NULL
  }
}
