library(methods)

#' A wrapper class so that objects returned can save themselves.
#' 
#' @field format format to save in
#' @field data data to save access function
#' @field .data actual data to save
#' @field .logger function to log with..
#' @export store
#' @exportClass output
store = methods::setRefClass(Class = "output", 
  fields = list(
    .name = "character",
    .format = "character",
    data = function(x = NULL) {
      if (!is.null(x))
        stop("This object is not mutable.")
      return(.self$.data[['object']])
    },
    .data = "list",
    .logger = "function"

  ),
  methods = list(
    initialize = function(data, format, logger = function(...) return(NULL)) {
      .self$.name = gsub('_', '-', as.character(substitute(data)))
      .self$.format = format
      .self$.data = list(object = data)
      .self$.logger = logger
    },
    save = function(target_dir) {
      if (is.null(.self$data))
	return()
      if (.self$.format == 'rds') {
        file = file.path(target_dir, paste0(.self$.name, '.rds'))
        saveRDS(.self$data, file)
	.self$.logger(info, paste0(.self$.name, " saved as .rds"))
      } else if (.self$.format == 'rdump') {
	e = as.environment(.self$data)
	name_list = ls(e)
        file = file.path(target_dir, paste0(.self$.name, '.rdump'))
        rstan::stan_rdump(list = name_list, file = file, envir = e)
	.self$.logger(info, paste0(.self$.name, " saved as .rdump"))
      }
    }
  )
)

#' Save output
#'
#' @param job job description
#' @param output list of output objects
#' @param logger logger to write log to...
#' @return NULL
#' @export
save_output <- function(target_dir, output) {
  if (is.list(output)) {
    for (o in output) {
      o$save(target_dir)
    }
  } else {
    output$save(target_dir)
  }
  return(NULL)
}

