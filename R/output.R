

setRefClass(Class = "output", 
  fields = list(
    type = "character",
    data = function(x = NULL) {
      if (!is.null(x))
        stop("This object is not mutable.")
      return(.data[['object']])
    },
    .data = "list"
  ),
  methods = list(
    save = function() {
  )
)

#' Save output
#'
#' @param job job description
#' @param output list of output objects
#' @param logger logger to write log to...
#' @return NULL
#' @export
save_output <- function(job, target_dir, output, logger) {
  output_names = names(output)
  for (i in 1:length(output_names)) {
    logger("Output extension: ", get_ext(output_path))
    if (get_ext(output_path) == 'rds') {
      saveRDS(output[[output_names[i]]], output_path)
    } else if (get_ext(output_path) == 'rdump') {
      rstan::stan_rdump(list = ls(output[[output_names[i]]]),
        file = output_path, envir = output[[output_names[i]]])
    } else logger("Output type not known for object: ", output_names[i])
  }
  return(NULL)
}

