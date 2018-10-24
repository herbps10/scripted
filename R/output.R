

store = methods::setRefClass(Class = "output", 
  fields = list(
    type = "character",
    data = function(x = NULL) {
      if (!is.null(x))
        stop("This object is not mutable.")
      return(.self$.data[['object']])
    },
    .data = "list",
    .logger = "function"

  ),
  methods = list(
    initialize = function(data, type, logger) {
      .self$.name = gsub('_', '-', as.character(substitute(data)))
      .self$.type = type
      .self$.data = data
      .self$.logger = logger
    },
    save = function(target_dir) {
      if (.self$.format == 'rds') {
        file = file.path(target_dir, paste0(.self$.name, '.rds'))
        saveRDS(.self$.data, file)
	.self$.logger(info, paste0(.self$.name, " saved as .rds"))
      } else if (.self$.format == 'rdump') {
	e = as.environment(.self$.data)
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
  for (o in output) {
    o$save(target_dir)
  }
  return(NULL)
}

