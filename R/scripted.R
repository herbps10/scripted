
#' Run scripts based on .yaml instructions
#'
#' An instructions file is a .yaml file that is loaded as an
#' R 'list'.  It contains two top-level elements: "defaults", and
#' "runs".  The "defaults" element is a list describing a single 
#' script to run.  The "runs" element is a list of lists.  Each item
#' in this list describes a single script to run.  So:
#'
#' defaults:
#'   [RUN DEFAULTS]
#'
#' runs:
#'   - [RUN A]
#'   - [RUN B]
#'
#' Before each run is ... run... the run-specific data is merged
#' on top of the run defaults using purrr::list_merge.
#'
#' The elements of a run are:
#'
#' - name : the name of the script to run (without its extension).
#' - parameters : (optional) a list where any script-specific control data
#'                can be placed.
#' - source_dir : a vector of strings, one per directory to search for
#'                resources (data files _and_ scripts).
#' - dependencies : a vector of strings, one per file required for the
#'                  script to run.  If a dependency is update the script
#'                  is re-run.
#' - target_dir : the root of the directory where script outptu will
#'                be saved, a single string.
#' - outputs : 
#' 
#' @param file .yaml file with instructions, see example
#' @param log_file where to write text log to.
#' @return log_file where logs were written.
#' @export
scripted = function(file, target_dir = ".", log_file = tempfile(), debug=FALSE) {
  instructions = yaml::yaml.load_file(file)
  n_jobs = length(instructions[['runs']])
  log = logger(log_file)
  log(info, "There are ", n_jobs, " jobs.")
  log(info, "Target directory is: ", target_dir)

  for (i in 1:n_jobs) {
    job = get_job(instructions, i, log)
    target_dir = create_target(job, target_dir, log)
    check_file = create_check_file(target_dir, job, log)
    script_path = get_script(job, log)
    load_script(script_path, debug, log)

    will_run = check_dependencies(job, check_file, log)
    if (will_run) {
      output = NULL
      log(info, "Calling main function from '", script_path, "'.")
      output = main(job, target_dir, log)
      save_output(target_dir, output) 
    } else {
      log(info, "Skipping main function from '", script_path, "'.")
    }

  }
  log_file = log(info, "Finished all jobs.")
  return(log_file)
}



