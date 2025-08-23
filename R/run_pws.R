#' @import logger
.init_logger <- function(dir_root, project_name, trial_number,
                         log_level = "INFO") {
  directories <- pwsMultiObsR:::fm_trial_set(
    dir_root, project_name, trial_number
  )
  log_dir <- file.path(directories["dir_project"], "logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)


  # Construct log file path
  trial_num <- sprintf("%03d", as.integer(trial_number))
  log_path <- file.path(log_dir, sprintf(
    "%s_trial_%s_%s.log",
    project_name,
    trial_num,
    format(Sys.time(), "%Y%m%d_%H%M%S")
  ))
  log_appender(appender_tee(log_path)) # prints to console too
  log_threshold(log_level)

  log_info("Logging initialized at %s", log_path)
  return(invisible(NULL))
}

#' Function to determine worker count based on 'performance' level
#' @import logger
.set_workers <- function(performance) {
  cores <- parallel::detectCores()

  if (performance == "low") {
    worker_num <- cores %/% 4
  } else if (performance == "medium") {
    worker_num <- cores %/% 2
  } else if (performance == "high") {
    worker_num <- cores - 2
  } else {
    msg <- "Unrecognized performance level"
    log_error(msg)
    stop(msg)
  }
  msg <- paste0("Allocated number of cores: ", worker_num)
  log_info(msg)

  return(worker_num)
}


#' Function to truncate runs to prevent exceeding nruns
#'
#' @import logger
.truncate_runs <- function(runs, nruns) {
  valid_runs <- runs[runs <= nruns]
  if (length(valid_runs) < length(runs)) {
    msg <- ("Some runs exceed 'nruns'.
            Truncating the sequence to avoid missing parameter files.")
    log_info(msg)
  }
  return(valid_runs)
}

#' Function to check for existing folders and adjust runs to avoid overwriting
.check_existing_runs <- function(runs, dir_dynamic_output) {
  adjusted_runs <- vector() # To hold runs that don't already have folders
  for (run in runs) {
    run_dir <- file.path(dir_dynamic_output, paste0("run_", run))
    if (!dir.exists(run_dir)) {
      adjusted_runs <- c(adjusted_runs, run) # Add runs without existing folders
    } else {
      msg <- paste(
        "Folder for run", run,
        "already exists. Skipping this run."
      )
      log_info(msg)
    }
  }
  return(adjusted_runs)
}


#' Function to copy model output in temporary directory to model directory
#'
#' @param temp_working_dir character: created in .run_simulation
#' @param model_dir character: full file path of the .nc output file created
#' in .run_simulation
#' @import logger
.temp_to_output <- function(out_var_names, temp_working_dir, model_dir) {
  # Validate input
  if (!is.character(out_var_names)) {
    msg <- "out_var_names must be a character vector."
    log_error(msg)
    stop(msg)
  }

  # Copy files dynamically
  for (name in out_var_names) {
    src <- file.path(
      temp_working_dir,
      "SA_temp_out", "nhm", paste0(name, ".nc")
    )
    dest <- file.path(model_dir, paste0(name, ".nc"))

    if (!file.exists(src)) {
      msg <- sprintf("Source file missing: %s", src)
      log_warn(msg)
      warning(msg)
      next
    }

    file.copy(src, dest, overwrite = TRUE)
  }
  return()
}


#' Function to run the model for each run
#'
#' @import logger
.run_simulation <- function(
    run,
    dir_dynamic_input,
    dir_default_output,
    dir_dynamic_output,
    dir_python,
    pwsenvname,
    out_var_names) {
  tryCatch(
    {
      run_start_time <- Sys.time()

      # Ensure the Python environment is set up correctly in each worker
      reticulate::use_condaenv(pwsenvname, required = TRUE)

      param_file <- paste0("myparam_", run, ".param") # get param file

      # Create a unique temporary working directory for this run
      temp_working_dir <- tempfile(pattern = paste0("temp_run_", run, "_"))
      dir.create(temp_working_dir, recursive = TRUE)

      # Copy climate .nc files from the default output
      cbh_src <- list.files(file.path(dir_default_output, "cbh_files"))
      dir.create(paste0(temp_working_dir, "/SA_temp_in"))
      dir.create(paste0(temp_working_dir, "/SA_temp_in/cbh_files"))
      file.copy(
        from = paste0(dir_default_output, "/cbh_files/", cbh_src),
        to = paste0(temp_working_dir, "/SA_temp_in/cbh_files/", cbh_src)
      )

      # Copy other model run files
      # Note that the parameter file run tag is dropped to be put into pws
      ctrl_src <- file.path(dir_dynamic_input, "default_input/control.control")
      # Seaarches for file input/trial_xxx/myparam_#.param
      param_src <- file.path(dir_dynamic_input, param_file)
      python_src <- file.path(dir_python)

      file.copy(
        from = ctrl_src,
        to = paste0(temp_working_dir, "/SA_temp_in/control.control"),
        overwrite = FALSE
      )
      file.copy(
        from = param_src,
        to = paste0(temp_working_dir, "/SA_temp_in/myparam.param"),
        overwrite = TRUE
      )
      file.copy(
        from = python_src,
        to = temp_working_dir,
        overwrite = FALSE
      )

      # Set the working directory to the temporary directory
      old_wd <- setwd(temp_working_dir)

      reticulate::py_run_file(dir_python) # MODEL RUNS HERE
      msg <- paste("Successfully completed simulation for run", run)
      log_info(msg)

      # Create output directory
      model <- paste0("run_", run) # run folder name
      model_dir <- file.path(dir_dynamic_output, model)
      dir.create(model_dir, recursive = TRUE)

      .temp_to_output(out_var_names, temp_working_dir, model_dir)

      # Restore the original working directory
      setwd(old_wd)
      unlink(temp_working_dir, recursive = TRUE)
      gc()

      run_end_time <- Sys.time()
      run_duration <- as.numeric(run_end_time - run_start_time, units = "secs")
      return(run_duration)
    },
    error = function(e) {
      message(paste("Error in run", run, ":", e$message))
      return(invisible(NULL))
    }
  )
}


#' Helper to setup a parallel plan, wrapper for .run_simulation
#' @import future
#' @import furrr
.run_parallel <- function(dir_root,
                          project_name,
                          trial_number,
                          runs = NULL,
                          pwsenvname = "pws",
                          out_var_names = c(
                            "seg_outflow", "pkwater_equiv",
                            "snowcov_area", "soil_rechr",
                            "hru_actet"
                          ),
                          performance = "medium") {
  # Get directories
  dir_list <- pwsMultiObsR:::fm_trial_set(dir_root, project_name, trial_number)
  nruns <- dir_list$nruns
  dir_python <- dir_list$dir_python
  dir_default_output <- dir_list$dir_default_output
  dir_dynamic_input <- dir_list$dir_dynamic_input
  dir_dynamic_output <- dir_list$dir_dynamic_output

  performance <- match.arg(performance, c("low", "medium", "high"))
  worker_num <- .set_workers(performance)
  future::plan(future::multisession, workers = worker_num)

  # OVERWRITE PROTECTION ----------------------------------------------------

  if (is.null(runs)) {
    runs <- seq(1, nruns, by = 1)
  } else if (!is.vector(runs) || !is.vector(runs)) {
    msg <- "Input 'runs' must be a numeric vector"
    log_error(msg)
    stop(msg)
  }

  runs <- .truncate_runs(runs, nruns)
  runs_to_execute <- .check_existing_runs(runs, dir_dynamic_output)

  if (length(runs_to_execute) == 0) {
    msg <- "All specified runs already exist. Nothing to run."
    log_error(msg)
    stop(msg)
  }

  # PARALLEL BATCHES W/ RUNTIME MONITORING ----------------------------------

  # Monitor performance and execute in batches
  current_run <- 1
  base_run_duration <- NULL

  while (current_run <= length(runs_to_execute)) {
    # Take the next batch of runs
    chunk_runs <- runs_to_execute[current_run:min(
      current_run + worker_num - 1,
      length(runs_to_execute)
    )]
    msg <- paste("Executing chunk:", paste(chunk_runs, collapse = ", "))
    log_info(msg)

    # Apply run_simulation in parallel for this batch
    run_durations <- furrr::future_map(
      chunk_runs,
      pwsMultiObsR:::.run_simulation,
      dir_dynamic_input = dir_dynamic_input,
      dir_dynamic_output = dir_dynamic_output,
      dir_default_output = dir_default_output,
      dir_python = dir_python,
      pwsenvname = pwsenvname,
      out_var_names = out_var_names,
      .options = furrr::furrr_options(seed = TRUE),
      .progress = FALSE
    )

    valid_durations <- as.numeric(unlist(run_durations))
    valid_durations <- valid_durations[!is.na(valid_durations)]

    if (length(valid_durations) == 0) {
      msg <- "All runs in the chunk failed. Aborting execution."
      log_error(msg)
      stop(msg)
    }

    # If it's the first batch, calculate the base run duration
    if (is.null(base_run_duration)) {
      base_run_duration <- mean(valid_durations) # average time of first batch
      msg <- paste(
        "Base run duration (first chunk):",
        base_run_duration, "seconds"
      )
      log_info(msg)
    }

    # Check each run's duration for performance decay
    for (i in seq_along(chunk_runs)) {
      run <- chunk_runs[i]
      run_duration <- as.numeric(run_durations[[i]])
      msg <- paste("Run", run, "duration:", run_duration, "seconds")
      log_info(msg)

      # Stop the script if run times exceed 2x the base run duration
      if (!is.na(run_duration) && run_duration >= 2 * base_run_duration) {
        msg <- paste(
          "Performance has degraded significantly.
                     Aborting further runs. Run",
          run, "took", run_duration,
          "seconds, exceeding threshold of 2*", base_run_duration,
          "seconds."
        )
        log_error(msg)
        stop(msg)
      }
    }

    current_run <- current_run + worker_num
  }

  future::plan(future::sequential) # Reset the future plan

  return()
}


#' Run pywatershed in parallel batches
#'
#' WARNING: this is not intended to be used on high performance
#' compute clusters or multi-tenant systems.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param trial_number integer: trial number, leading zeros not needed
#' @param runs numeric: a sequence of simulation indices to execute. The
#' indices correspond to the parameter file indices. If left as NULL, it will
#' default to seq(1, nruns, 1). Nruns is determined by the project_name that
#' is passed.
#' @param pwsenvname character: The name of your python environment where the
#' python 'pywatershed' package is installed.
#' @param out_var_names character: a vector of the output file names that should
#' be saved.
#' @param performance character:'high', 'medium', or 'low' to select the number
#' of cores to use. WARNING: this is not intended to be used on high performance
#' compute clusters or multi-tenant systems.
#' @param batch_size integer: the number of of simulations before memory
#' cleanup. Set to 48 for optimal 4 or 6 core allocations.
#' @param pause_time integer: time in seconds to shut down the previous
#' operation and do memory cleanup.
#' @export
run_batches <- function(dir_root,
                        project_name,
                        trial_number,
                        runs = NULL,
                        pwsenvname = "pws",
                        out_var_names = c(
                          "seg_outflow", "pkwater_equiv",
                          "snowcov_area", "soil_rechr",
                          "hru_actet"
                        ),
                        performance = "medium",
                        batch_size = 48,
                        pause_time = 60) {
  .init_logger(dir_root, project_name, trial_number)

  dir_list <- pwsMultiObsR:::fm_trial_set(dir_root, project_name, trial_number)
  dir_dynamic_output <- dir_list$dir_dynamic_output
  nruns <- dir_list$nruns


  batch_file <- file.path(dir_dynamic_output, "!batch_durations.csv")

  # If file exists, read it in; otherwise create empty data frame
  if (file.exists(batch_file)) {
    batch_log <- read.csv(batch_file)
    msg <- paste0("Appending to existing batch log: ", batch_file)
    log_info(msg)
  } else {
    batch_log <- data.frame(
      batch_idx = integer(),
      start_time = as.POSIXct(character()),
      end_time = as.POSIXct(character()),
      duration_sec = numeric(),
      stringsAsFactors = FALSE
    )
  }


  # Determine where to start
  completed_batches <- nrow(batch_log)
  batch_idx <- completed_batches + 1

  for (batch_start_run in seq(1, nruns, by = batch_size)) {
    batch_end_run <- min(batch_start_run + batch_size - 1, nruns)

    start_time <- Sys.time()

    # Run the model for the current batch
    pwsMultiObsR:::.run_parallel(
      dir_root = dir_root,
      project_name = project_name,
      trial_number = trial_number,
      runs = seq(batch_start_run, batch_end_run, by = 1),
      pwsenvname = pwsenvname,
      out_var_names = out_var_names,
      performance = performance
    )

    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    msg <- paste0(
      "Batch ", batch_idx, " duration is: ",
      duration, " seconds"
    )
    log_info(msg)

    batch_log <- rbind(batch_log, data.frame(
      batch_idx = batch_idx,
      start_time = start_time,
      end_time = end_time,
      duration_sec = duration,
      stringsAsFactors = FALSE
    ))
    write.csv(batch_log, batch_file, row.names = FALSE)

    # Pause before the next batch unless it's the last batch
    if (batch_end_run < nruns) {
      msg <- sprintf("Pausing for %d seconds before next batch...", pause_time)
      log_info(msg)
      Sys.sleep(pause_time)
    }

    batch_idx <- batch_idx + 1
  }

  # Optional: plot durations
  plot(batch_log$duration_sec,
    type = "b", main = "Batch Durations",
    xlab = "Batch Index", ylab = "Duration (s)"
  )

  return(invisible(NULL))
}


#' Run a default pywatershed simulation
#'
#' WARNING: this is not intended to be used on high performance
#' compute clusters or multi-tenant systems.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param trial_number integer: trial number, leading zeros not needed
#' @param runs numeric: a sequence of simulation indices to execute. The
#' indices correspond to the parameter file indices. E.g. seq(1, nruns, 1).
#' @param pwsenvname character: The name of your python environment where the
#' python 'pywatershed' package is installed.
#' @export
run_default <- function(dir_root,
                        project_name,
                        trial_number,
                        pwsenvname = "pws") {
  .init_logger(dir_root, project_name, trial_number)
  msg <- "Starting default run"
  log_info(msg)

  # Start timer
  start_time <- Sys.time()

  dir_list <- pwsMultiObsR:::fm_trial_set(dir_root, project_name, trial_number)
  dir_python <- dir_list$dir_python
  dir_default_output <- dir_list$dir_default_output
  dir_dynamic_input <- dir_list$dir_dynamic_input

  # Create a unique temporary working directory for this run
  temp_working_dir <- tempfile(pattern = paste0("default_run"))
  dir.create(temp_working_dir, recursive = TRUE)

  # Copy input files into the temp dir
  dir.create(paste0(temp_working_dir, "/default_input"))
  input_src <- list.files(file.path(dir_dynamic_input, "default_input"))
  file.copy(
    from = paste0(dir_dynamic_input, "/default_input/", input_src),
    to = paste0(temp_working_dir, "/default_input/", input_src),
    overwrite = FALSE
  )
  file.copy(
    from = dir_python,
    to = temp_working_dir,
    overwrite = FALSE
  )

  # Set the working directory to the temporary directory
  old_wd <- setwd(temp_working_dir)

  reticulate::use_condaenv(pwsenvname, required = TRUE)
  reticulate::py_run_file(dir_python)
  msg <- "Python script completed for default run"
  log_info()

  # Copy output files to dir_default_output
  dir.create(paste0(dir_default_output, "/nhm"))
  dir.create(paste0(dir_default_output, "/cbh_files"))

  nhm_src <- list.files(file.path(temp_working_dir, "default_output/nhm"))
  cbh_src <- list.files(file.path(temp_working_dir, "default_output/cbh_files"))

  file.copy(
    from = paste0(temp_working_dir, "/default_output/nhm/", nhm_src),
    to = paste0(dir_default_output, "/nhm/", nhm_src),
    overwrite = FALSE
  )
  file.copy(
    from = paste0(temp_working_dir, "/default_output/cbh_files/", cbh_src),
    to = paste0(dir_default_output, "/cbh_files/", cbh_src),
    overwrite = TRUE
  )

  # Restore the original working directory
  setwd(old_wd)
  unlink(temp_working_dir, recursive = TRUE)
  gc()

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  msg <- paste("Total elapsed time:", elapsed_time)
  log_info(msg)

  return(invisible(NULL))
} # close fn
