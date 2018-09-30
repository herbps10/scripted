#' Retrieve a job with defaults integrated.
#'
#' @param instructions, loaded from .yaml file (list)
#' @param i pull out i'th job
#' @param logger used to log output
#' @return job
#' @export
get_job <- function(instructions, i, logger) {
  job <- instructions[['defaults']] %>% purrr::list_merge(instructions[['runs']][[i]])
  logger("Job name: ", job[[1]][['name']])
  return(job[[1]])
}

#' Retrieve the script to run on.
#'
#' @param job job to get script from
#' @param logger used to log output
#' @return path to job script
#' @export
get_script <- function(job, logger) {
  script_file = paste0(job[['name']], ".R")
  script_path = find_file(job[['source_dir']], script_file)
  if (length(script_path) == 0) 
    logger("Script not found: ", script_file)
  else
    logger("Script path: ", script_path)
  return(script_path)
}


#' Returns the expected output files from a job:
#'
#' @param job a job
#' @param logger, a logger
#' @return character vector of expected object names
#' @export
get_expected_files <- function(job, logger) { 
  expect_file <- sapply(job[['outputs']], function(x) x[['file']])
  return(expect_file)
}

#' Return the expected outputs from a job:
#'
#' @param job a job
#' @param logger, a logger
#' @return character vector of expected object names
#' @export
get_expectations <- function(job, logger) { 
  expect_file <- get_expected_files(job, logger) 
  expect_name <- gsub('\\.[a-zA-Z0-9]+$', '', expect_file)
  expect <- gsub('-', '_', expect_name)
  return(expect)
}

#' Get file extension from path
#'
#' @param s path
#' @return extension
#' @export
get_ext <- function(s) {
  split <- strsplit(x=s, split='\\.')[[1]]
  ls <- length(split)
  return(split[ls])
}

