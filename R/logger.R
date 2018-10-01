
start = "START_LOGGER"
debug = "DEBUG"
info = "INFO"
warn = "WARN"
error = "ERROR"
fatal = "FATAL"


#' Return a closure with a log file set
#'
#' @param log_file where to write log to
#' @return closure, callable in various ways with strings.
#' @export
logger <- function(log_file = tempfile()) { 
  log_file <- log_file
  write_message = function(level, ...) {
    msg = paste0(..., collapse=", ")
    stamp = paste0("[", level, "] [", Sys.time(), "]")
    text = paste(stamp, msg)
    cat(text, file = log_file, sep = "\n", append = TRUE)
    if (level == "WARN")
      warning(text)
    if (level == "ERROR" || level == "FATAL")
      stop(text)
    return(log_file)
  }
  write_message(start, "starting script")
  return(write_message)
}




