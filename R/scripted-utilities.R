

#' Create a .check file name for the script
#'
#' This is not a fail-safe procedure.
#'
#' @param target_dir directory to look in.
#' @return single-string character vector of the file name.
check_file_path = function(target_dir, stub) {
  found_path = dir(path = target_dir, pattern = paste0(stub, ".*\\.check"),
    full.names = TRUE, recursive = FALSE)
  if (length(found_path) == 0) {
    path = tempfile(pattern = paste0(stub, "-"), tmpdir = target_dir,
      fileext = ".check")
  } else {
    path = found_path
  }
  return(path)
}

#' Create a .check file for the script
#' 
#' This is a .yaml file that records file content checksums
#' and their modification times, etc... to verify whether a file
#' is available and has been updated.
#' @param target_dir directory to look in
#' @return check file path
create_check_file = function(target_dir, job, logger) {
  stub = paste0('scripted-target-', job[['name']])
  path = check_file_path(target_dir, stub)
  if (file.exists(path))
    return(path)
  cf_created = file.create(path, showWarnings = FALSE)
  if (!cf_created) {
    if (!file.exists(path)) {
      logger(error, "check file ('", path, "') could not be created ",
        "and does not exist. Stopping.")
    } else {
      cf_is_dir = file.info(path)$isdir
      if (cf_is_dir) 
        logger(error, "check file ('", path, "') exists but is a directory. ",
          "Stopping.")
    }
  }
  return(path)
}

#' Load a check file and check dependencies.
#'
#' @param file path to check file
#' @param logger logger for output
#' @return list with hash for each dependency
#' @importFrom indexer find_file
check_dependencies = function(job, file, logger) {

  # Check for previous check file.
  will_error = FALSE
  if (!file.exists(file))
    logger(error, "Check file ('", file, "') ",
      "does not exist. Stopping")
  if (file.info(file)$isdir)
    logger(error, "There is a directory at the check ",
      "file path ('", file, "'). Stopping.")
  cs = yaml::yaml.load_file(file)
  if (is.null(cs)) {
    logger(warn, "Check file ('", file, "') does not ",
      "contain checksums.  Creating fresh checksums. ",
      "Script will run.") 
    cs = list()
  }

  dependency_files = job[['dependencies']]
  dependency_paths = find_file(job[['source_dir']], dependency_files)
  dependency_files_found = basename(dependency_paths)

  # Check file existence.
  for (dep in dependency_files) {
    if (dep %in% dependency_files_found)
      logger(debug, "Found dependency '", dep, "'.")
    else {
      will_error = TRUE
      for (s in job[['source_dir']])
        logger(warn, "Searched in '", s, "'.")
      logger(warn, "Dependency not found: '", dep, "'.")
    }
  }
  if (will_error)
    logger(error, "Fatal errors encountered.  Stopping.  Check log file.")

  # Check sums.
  new_cs = list()
  will_run = FALSE
  for (dep in dependency_paths) {
    new_cs[[dep]] = openssl::md5(dep)
    if (!(dep %in% names(cs))) {
      will_run = TRUE
      logger(warn, "The dependency '", dep, "' is a new dependency. ",
        "Script will be run.")
    } else if (new_cs[[dep]] != cs[[dep]]) {
      logger(warn, "The dependency '", dep, "' has been changed. ",
        "Script will be run.")
      will_run = TRUE
    }
  }
  yaml::write_yaml(x = new_cs, file = file)
  return(will_run)
}
  
