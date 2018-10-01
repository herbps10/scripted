#' Retrieve a job with defaults integrated.
#'
#' @param instructions, loaded from .yaml file (list)
#' @param i pull out i'th job
#' @param logger used to log output
#' @return job
#' @export
get_job = function(instructions, i, logger) {
  job = instructions[['defaults']] %>% purrr::list_merge(instructions[['runs']][[i]])
  logger(info, "Job name: ", job[[1]][['name']])
  return(job[[1]])
}

#' Retrieve the script to run on.
#'
#' @param job job to get script from
#' @param logger used to log output
#' @return path to job script
#' @export
get_script = function(job, logger) {
  script_file = paste0(job[['name']], ".R")
  script_path = find_file(job[['source_dir']], script_file)
  if (length(script_path) == 0) 
    logger(error, "Script not found: ", script_file)
  else
    logger(info, "Script path: ", script_path)
  return(script_path)
}


#' Get file extension from path
#'
#' @param s path
#' @return extension
#' @export
get_ext = function(s) {
  split = strsplit(x=s, split='\\.')[[1]]
  ls = length(split)
  return(split[ls])
}

#' Create target dir, etc...
#'
#' @param target directory where output will go.
#' @param logger how to log 
#' @return NULL
#' @export
create_target = function(job, target, logger) {
  target_dir = target
  created_target = dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  if (!created_target) {
    target_exists = dir.exists(target_dir)
    if (target_exists) {
      target_is_dir = file.info(target_dir)[['isdir']]
      if (target_is_dir)
        logger(warn, "Target directory already exists, reusing '", target_dir, "'.")
      else {
        logger(error, "Target directory path ('", target_dir, "') exists", 
               "but it is not a directory. Stopping")
        stop("Target directory could not be created, check log.")
      }
    } else {
      logger(error, "Target directory path ('", target_dir, "') could not",
             " be created. Stopping.")
      stop("Target directory coudl not be created, check log.")
    }
  }
  return(target_dir)
}


#' Load a script
#'
#' @param script path to script
#' @param debug whether to echo during source
#' @return NULL
#' @export
load_script = function(script, debug, logger) {
  script_exists = file.exists(script)
  if (script_exists) {
    logger(info, "loading script: ", script)
    source(script, echo = debug)
  } else {
    script_is_dir = dir.exists(script)
    if (script_is_dir) 
      logger(error, "script path ('", script, "') exists but is a directory.")
    else
      logger(error, "script path ('", script, "') does not exist.")
  }
  return(NULL)
}


