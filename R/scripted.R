
#' Run scripts based on .yaml instructions
#' 
#' @param file .yaml file with instructions, see example
#' @param log_file where to write text log to.
#' @return log_file where logs were written.
#' @export
scripted <- function(file, log_file = tempfile(), debug=FALSE) {
  instructions <- yaml::yaml.load_file(file)
  n_instructions <- length(instructions[['runs']])
  log <- logger(log_file)
  log("\n\nThere are ", n_instructions, " jobs.")

  for (i in 1:n_instructions) {
    log("Instruction ", i)
    job <- get_job(instructions, i, log)

    log(paste("Search for dependencies in: ", job[['source_dir']]))
    script_path = get_script(job, log)

    log("Loading script.", script_path)
    source(script_path, echo = debug)

    log("Does the output target exist?")
    target_dir <- job[['target_dir']]
    if (!dir.exists(target_dir)) {
      log("Target directory does not exist, creating: ", target_dir)
      if (file.exists(target_dir) && file.info(target_dir)$isdir) {
        log("Target directory path has a file.  Aborting.")
        stop("Target directory path has a file. Aborting.")
      } else {
        dir.create(target_dir, recursive=TRUE)
      }
    }
      

    log("Get expected files.")
    expected_files <- get_expected_files(job, log)
    expectations_met <- file.exists(file.path(job[['target_dir']], expected_files))
    if (all(expectations_met)) {
      log("All outputs are present, skipping job.")
      next
    } else {
      missing_expectations <- expected_files[!expectations_met]
      for (f in missing_expectations) {
        log("Need to produce ", f)
      }
    }

    o <- NULL
    log("Calling script-level main function.")
    o <- main(job)

    if (!is.null(o)) {
      log("Objects in return are: ")
      log(names(o))
    } else {
      log("No objects found in return.")
    }

    expected_objects <- get_expectations(job, log)
    log("Expected objects are: ")
    log(expected_objects)
    if (all(expected_objects %in% names(o))) {
      log("All output found.")
    } else {
      log("Some expected output is missing.")
      missing_output <- expected_objects[!(expected_objects %in% names(o))]
      for (missing in missing_output) {
        log("Object named ", missing, " was not found.")
      }
    }

    log("Saving output.")
    log("Target directory is: ", job[['target_dir']])
    save_output(job, o, log) 
    log("Finished saving output.")
  }
  return(log_file)
}



