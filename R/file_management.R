#' Create a pwsMultiObsR project
#'
#' This function takes your specified working directory and creates a child
#' directory specific to the project name and relevant input/output directories
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @return character: project directory
#' @examples
#' fm_project_create(
#'   dir_root = "/Users/me/myworkingdir/",
#'   project_name = "my_project"
#' )
#'
#' @export
fm_project_create <- function(dir_root, project_name) {
  main_folder <- file.path(dir_root, "projects")

  # Create the main projects folder if it doesn't exist
  if (!dir_exists(main_folder)) {
    dir_create(main_folder)
    cat("Main projects folder created successfully:\n")
    cat(" - ", main_folder, "\n")
  }

  project_path <- file.path(main_folder, project_name)

  if (dir_exists(project_path)) {
    stop("Project already exists")
  }

  # Create project sub-folders here
  dir_create(project_path)
  dir_create(file.path(project_path, "default_input"))
  dir_create(file.path(project_path, "default_output"))
  dir_create(file.path(project_path, "input"))
  dir_create(file.path(project_path, "output"))
  dir_create(file.path(project_path, "gis"))
  dir_create(file.path(project_path, "plot"))
  dir_create(file.path(project_path, "obs"))
  dir_create(file.path(project_path, "obs/aso"))
  dir_create(file.path(project_path, "obs/snotel"))
  dir_create(file.path(project_path, "obs/usgs"))
  dir_create(file.path(project_path, "obs/mod10a1"))
  dir_create(file.path(project_path, "obs/openet"))
  dir_create(file.path(project_path, "obs/smapl4"))


  # Create the metadata file
  logs_path <- dir_create(file.path(project_path, "logs"))
  metadata_file <- file.path(logs_path, "project_metadata.txt")
  metadata_file_project <- paste0("# Project Name: ", project_name)
  write(metadata_file_project, file = metadata_file)
  metadata_file_header <- paste("number",
    "type",
    "nruns",
    "time",
    sep = "\t"
  )
  write(metadata_file_header, file = metadata_file, append = TRUE)

  # Print some messages
  cat("Project created successfully:\n")
  cat(" - ", project_path, "\n")
  cat(" - ", file.path(project_path, "default_input"), "\n")
  cat(" - ", file.path(project_path, "default_output"), "\n")
  cat(" - ", file.path(project_path, "input"), "\n")
  cat(" - ", file.path(project_path, "output"), "\n")
  cat(" - ", file.path(project_path, "gis"), "\n")
  cat(" - ", file.path(project_path, "plot"), "\n")
  cat(" - ", file.path(project_path, "obs"), "\n")
  cat(" - Metadata file: ", metadata_file, "\n")

  return(project_path)
}


#' Search pwsMultiObsR projects
#'
#' Print the names of your existing projects
#'
#' @param dir_root character: parent working directory path
#' @param verbose logical: prints the contents of the metadata files
#' @return character vector: list of project names
#' @examples
#' fm_project_search("/Users/me/myworkingdir/", verbose = TRUE)
#' @export
fm_project_search <- function(dir_root, verbose = FALSE) {
  main_folder <- file.path(dir_root, "projects")

  if (!dir_exists(main_folder)) {
    stop("Main projects folder does not exist, use pws.create.project first.")
  }

  project_folders <- dir_ls(main_folder, type = "directory")

  cat("projects:\n")
  for (project in project_folders) {
    cat(" - ", basename(project), "\n")

    if (verbose) {
      metadata_file <- file.path(project, "logs/project_metadata.txt")
      if (file_exists(metadata_file)) {
        cat("\nContents of ", metadata_file, ":\n", sep = "")
        cat(readLines(metadata_file), sep = "\n")
      } else {
        cat("No metadata file found in ", project, "\n", sep = "")
      }
    }
  }

  if (length(project_folders) == 0) {
    cat("No projects found.\n")
  }

  return(project_folders)
}


#' Create a pwsMultiObsR trial directory
#'
#' Create a trial child directory for a project and generate parameter files.
#' Note that for large morris or mmlhs trials, it may take a long time for the
#' function to complete.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param type character: string of either "default", "morris", or "mmlhs"
#' @param param_names character vector: vector of parameter names to be sampled
#' @param nruns integer: number of desired simulations for morris or mmlhs. For
#' morris, the preferred nruns is k*(1+k), where k is the number of parameters,
#' For mmlhs, the preferred nruns in k*1000, hence why this follows the
#' sensitivity analysis.
#' @return list: list of project directories and metadata
#' @examples
#' # for a default run
#' trial_directories <- fm_trial_create(
#'   dir_root = "/Users/me/myworkingdir/",
#'   project_name = "my_project",
#' )
#'
#' # for morris or mmlhs
#' trial_directories <- fm_trial_create(
#'   dir_root = "/Users/me/myworkingdir/",
#'   project_name = "my_project",
#'   type = "morris",
#'   param_names = c("tmax_allsnow", "jh_coef"),
#'   nruns = 12
#' )
#' @export
#' @import fs
fm_trial_create <- function(dir_root,
                            project_name,
                            type,
                            param_names = NULL,
                            nruns = NULL) {
  # Initialize directory list for return
  list_trial <- list()

  # CHECKS ------------------------------------------------------------------

  # Require valid run type
  if (!type %in% c("default", "morris", "mmlhs")) {
    stop("Invalid trial type. Must be 'default', 'morris', or 'MMLHS'.")
  }

  # Require existing project
  main_folder <- file.path(dir_root, "projects")
  project_path <- file.path(main_folder, project_name)

  if (!dir_exists(project_path)) {
    stop("Project does not exist")
  }

  # Require param_names for SA
  if (type %in% c("morris", "mmlhs")) {
    if (is.null(param_names)) {
      stop("Argument 'param_names' is required when 'type' is 'morris',
           or 'MMLHS'.")
    }
  }

  # Require nruns for SA
  if (type %in% c("morris", "mmlhs")) {
    if (is.null(nruns)) {
      stop("Argument 'nruns' is required when 'type' is 'morris','groups',
           or 'MMLHS'.")
    }
  }

  if (type == "default") {
    if (!is.null(nruns) || !is.null(param_names)) {
      warning("Argument 'nruns' or 'param_names' not applicable to type
              'default'")
    }
  }

  # DYNAMIC DIRS ------------------------------------------------------------

  # Paths for input and output directories
  input_base_path <- file.path(project_path, "input", "trial_")
  output_base_path <- file.path(project_path, "output", "trial_")
  plot_base_path <- file.path(project_path, "plot", "trial_")

  # Create unique trial folder in the i/o directories with leading zeros
  trial_counter <- 1
  trial_num <- sprintf("%03d", trial_counter)
  dir_dynamic_input <- paste0(input_base_path, trial_num)
  dir_dynamic_output <- paste0(output_base_path, trial_num)
  dir_plot <- paste0(plot_base_path, trial_num)

  while (dir_exists(dir_dynamic_input) || dir_exists(dir_dynamic_output)) {
    trial_counter <- trial_counter + 1
    trial_num <- sprintf("%03d", trial_counter)
    dir_dynamic_input <- paste0(input_base_path, trial_num)
    dir_dynamic_output <- paste0(output_base_path, trial_num)
    dir_plot <- paste0(plot_base_path, trial_num)
  }

  # Create dynamic directories
  dir_create(dir_dynamic_input)
  dir_create(dir_dynamic_output)
  dir_create(dir_plot)

  # DEFAULT DIRS ------------------------------------------------------------

  dir_default_input <- file.path(project_path, "default_input")

  # Copy the default_input directory into the dynamic input dir
  # so that defaults are tracked as trials are created
  dir.create(paste0(dir_dynamic_input, "/default_input"))
  input_src <- list.files(dir_default_input) # list files in from
  file.copy(
    from = paste0(dir_default_input, "/", input_src),
    to = paste0(dir_dynamic_input, "/default_input/", input_src),
    overwrite = FALSE
  )

  dir_default_output <- file.path(project_path, "default_output")

  # PYTHON DIRS ------------------------------------------------------------

  # Assign the python_dir based on the type (pulls files from inst/)
  if (type %in% c("morris", "mmlhs")) {
    dir_python <- system.file("python", "pws_parallel.py",
      package = "pwsMultiObsR"
    )
  } else if (type == "default") { # DIFFERENT ONLY FOR DEFAULT
    dir_dynamic_output <- file.path(project_path, "default_output")
    dir_python <- system.file("python", "pws_default.py",
      package = "pwsMultiObsR"
    )
  }

  # ASSIGN DIRS ------------------------------------------------------------

  list_trial["dir_dynamic_input"] <- dir_dynamic_input
  list_trial["dir_dynamic_output"] <- dir_dynamic_output
  list_trial["dir_plot"] <- dir_plot
  list_trial["dir_default_input"] <- dir_default_input
  list_trial["dir_default_output"] <- dir_default_output
  list_trial["dir_python"] <- dir_python
  list_trial["dir_gis"] <- file.path(project_path, "gis")
  list_trial["dir_obs"] <- file.path(project_path, "obs")
  list_trial["dir_project"] <- project_path
  list_trial["project_name"] <- project_name
  list_trial["trial_type"] <- type
  list_trial["trial_number"] <- trial_counter

  metadata_file <- file.path(project_path, "logs/project_metadata.txt")

  # SAMPLE FNs --------------------------------------------------------------

  if (type %in% c("morris", "mmlhs")) {
    param_attributes <- .ig_create_attributes(param_names = param_names)
  }

  # CONDITIONS FOR MORRIS
  if (type == "morris") {
    m <- nrow(param_attributes)
    r <- nruns / (m + 1)

    log_info("For Morris sampling, nruns = r(M+1) where M is number of factors")
    log_info("The recommended value for r is at least the number of factors")
    msg <- paste0("For trial ", trial_num, ", r = ", r)
    log_info(msg)

    if (r != as.integer(r)) {
      msg <- ("nruns does not result in an integer for r.
              Coercing r to the nearest integer.")
      log_warn(msg)
      warning(msg)
      r <- round(r)
    }

    if (r < 1) {
      msg <- "r is less than 1. Coercing r to the minimum of 1."
      log_warn(msg)
      warning(msg)
      r <- 1
    }

    if (r > 100) {
      msg <- ("r is greater than 100. Consider using a value within the
              recommended range of 10 to 100.")
      log_warn(msg)
      warning(msg)
    }

    # Run the morris sampling function
    nruns <- r * (m + 1)
    list_trial["nruns"] <- nruns

    # Record metadata
    current_time <- round(Sys.time())
    metadata_line <- paste(trial_counter, type, nruns, current_time, sep = "\t")
    write(metadata_line, file = metadata_file, append = TRUE)

    pwsMultiObsR:::.init_logger(dir_root, project_name, trial_counter)
    log_info("Trial folders created successfully")

    .ig_sample_morris(
      r = r,
      param_attributes = param_attributes,
      dir_dynamic_input = dir_dynamic_input
    )
  } # close morris if



  # CONDITIONS FOR MMLHS
  if (type == "mmlhs") {
    k <- nrow(param_attributes)

    log_info("For mmlhs sampling, nruns is the number of repetitions")
    log_info("The recommended nruns is > 1000*k where k is number of factors")
    msg <- paste0("For trial ", trial_num, ", nruns = ", nruns, "\n")
    log_info(msg)

    if (nruns != as.integer(nruns)) {
      msg <- "nruns in not an integer. Coercing nruns to the nearest integer."
      log_warn(msg)
      warning(msg)
      nruns <- round(nruns)
    }

    if (nruns != 1000 * k) {
      msg <- paste0("nruns is not the recommended amount: ", 1000 * k, "\n")
      log_warn(msg)
      warning(msg)
    }

    list_trial["nruns"] <- nruns

    # Record metadata
    current_time <- round(Sys.time())
    metadata_line <- paste(trial_counter, type, nruns, current_time, sep = "\t")
    write(metadata_line, file = metadata_file, append = TRUE)

    pwsMultiObsR:::.init_logger(dir_root, project_name, trial_counter)
    log_info("Trial folders created successfully")

    .ig_sample_mmlhs(
      nruns = nruns,
      param_attributes = param_attributes,
      dir_dynamic_input = dir_dynamic_input
    )
  }

  # CONDITIONS FOR DEFAULT
  # Assign nruns as 1 if trial type is default
  if (type == "default") {
    nruns <- 1
    list_trial["nruns"] <- nruns

    # Record metadata
    current_time <- round(Sys.time())
    metadata_line <- paste(trial_counter, type, nruns, current_time, sep = "\t")
    write(metadata_line, file = metadata_file, append = TRUE)

    pwsMultiObsR:::.init_logger(dir_root, project_name, trial_counter)
    log_info("Trial folders created successfully")
  }

  return(list_trial)
}



#' Set trial directories
#'
#' Assign a list of trial directories. Used after a trial is created to
#' continue modeling or processing outputs.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param trial_number integer: trial number, leading zeros not needed
#' @return list: list of project directories and metadata
#' @examples
#' fm_trial_set(
#'   dir_root = "/Users/me/myworkingdir/",
#'   project_name = "my_project",
#'   trial_number = 2
#' )
#'
#' @export
#' @import fs
fm_trial_set <- function(dir_root, project_name, trial_number) {
  # Initialize list for directories
  list_trial <- list()

  dir_root <- file.path(dir_root)

  main_folder <- file.path(dir_root, "projects")
  project_path <- file.path(main_folder, project_name)

  if (!dir_exists(project_path)) {
    stop("Project does not exist")
  }

  # Format the trial number with leading zeros
  trial_number <- as.integer(trial_number)
  trial_num <- sprintf("%03d", trial_number)


  # Extract nruns and type from metadata_log ----------------------------------

  # Parse nruns and type from the metadata_log.txt file
  metadata_file <- file.path(project_path, "logs/project_metadata.txt")
  if (!file.exists(metadata_file)) {
    stop("Metadata file does not exist")
  }

  # Read the metadata_log.txt file
  metadata_log <- readLines(metadata_file)

  # Find the line corresponding to the trial number
  trial_line <- metadata_log[grep(
    paste0("^", trial_number, "\t"),
    metadata_log
  )]

  if (length(trial_line) == 0) {
    stop(paste0("Trial number:", trial_number, " not found in the trial log"))
  }

  # Extract the nruns and type values from the line
  trial_data <- strsplit(trial_line, "\t")[[1]] # Returns a list
  nruns <- as.integer(trial_data[3]) # Third element for nruns
  type <- trial_data[2] # Second element for type

  # Assign directories --------------------------------------------

  dir_default_input <- file.path(project_path, "default_input")
  dir_default_output <- file.path(project_path, "default_output")
  dir_dynamic_input <- file.path(
    project_path, "input",
    paste0("trial_", trial_num)
  )
  dir_dynamic_output <- file.path(
    project_path, "output",
    paste0("trial_", trial_num)
  )
  dir_plot <- file.path(project_path, "plot", paste0("trial_", trial_num))
  dir_python <- system.file("python", "pws_parallel.py",
    package = "pwsMultiObsR"
  )

  if (type == "default") { # DIFFERENT ONLY FOR DEFAULT
    dir_dynamic_output <- file.path(project_path, "default_output")
    dir_python <- system.file("python", "pws_default.py",
      package = "pwsMultiObsR"
    )
  }


  if (!dir_exists(dir_dynamic_input) || !dir_exists(dir_dynamic_output)) {
    stop("Trial does not exist")
  }

  # Other trial agnostic dirs
  dir_gis <- file.path(project_path, "gis")
  dir_obs <- file.path(project_path, "obs")

  # Assign the appropriate directories to the list
  list_trial["dir_dynamic_input"] <- dir_dynamic_input
  list_trial["dir_dynamic_output"] <- dir_dynamic_output
  list_trial["dir_plot"] <- dir_plot
  list_trial["dir_default_input"] <- dir_default_input
  list_trial["dir_default_output"] <- dir_default_output
  list_trial["dir_python"] <- dir_python
  list_trial["dir_gis"] <- dir_gis
  list_trial["dir_obs"] <- dir_obs
  list_trial["dir_project"] <- project_path
  list_trial["project_name"] <- project_name
  list_trial["trial_type"] <- type
  list_trial["trial_number"] <- trial_number
  list_trial["nruns"] <- nruns

  return(list_trial)
}
