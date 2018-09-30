
#' Save output
#'
#' @param job job description
#' @param output list of output objects
#' @param logger logger to write log to...
#' @return NULL
#' @export
save_output <- function(job, output, logger) {
  target_dir <- job[['target_dir']]
  dir.create(path = target_dir, showWarnings = FALSE, recursive = TRUE)
  output_files <- sapply(job[['outputs']], function(x) x[['file']])
  output_names <- get_expectations(job, logger)
  for (i in 1:length(output_names)) {
    output_path <- file.path(target_dir, output_files[i])
    if (file.exists(output_path))
      next
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

