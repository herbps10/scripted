
#' Return a closure with a log file set
#'
#' @param log_file where to write log to
#' @return closure, callable in various ways with strings.
#' @export
logger <- function(log_file = tempfile()) { 
  log_file <- log_file
  log_f <- function(...) cat(paste0(..., collapse=", "), file = log_file, sep = "\n", append = TRUE)
#  of = file(log_file, open = "wt")
#  sink(type = 'message', file = of)
  cat(paste0("Starting at: ", Sys.time(), "\n"), file = log_file)
  return(log_f)
}




