# Module for general utilities



.read_json <- function(list_type, var, metric, dir_dynamic_output){

  # Check (redundant)
  if (!list_type %in% c("gof", "eet")) {
    stop("Invalid list type. Must be 'gof' or 'eet'.")
  }

  # Set list path structure
  if (list_type == "gof"){
    path <- file.path(
      dir_dynamic_output, paste0("/!list_", metric, "_", var, ".json"))
  } else if (list_type == "eet"){
    path <- file.path(
      dir_dynamic_output, paste0("/!eet_list_", metric, "_", var, ".json"))
  }

  if (file.exists(path)) {
    return(jsonlite::fromJSON(path))
  } else {
    warning(paste("File not found:", path))
    return(NULL)
  }

}


#' Read output lists
#'
#' Recursively compute morris elementary effects and sensitivity indices along
#' the model outputs (GOF or general statistics)
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param trial_number integer: trial number, leading zeros not needed
#' @param var_names character: vector of types e.g. c("q", "swe", "smapsfwt")
#' @param metrics character: "gof" of "metric" to specify objective or
#' predictive functions.
#' @param list_type character: "gof" of "eet" to pull objective results or
#' sensitivity results.
#' @return list: contains corresponding parameters and nested list of outputs.
#' @examples
#'
#' # Get data from a single project
#' gof_data = read_output(
#'   dir_root = "/Users/me/mywork/",
#'   project_name = "final_EastRiv",
#'   trial_number = 3,
#'   var_names = c("swe","q"),
#'   metrics = "gof",
#'   list_type = "gof")
#'
#' # Can use it in a loop to get data from multiple projects
#' my_proj_names <- c("final_BlueRiv", "final_DoloresRiv",
#'                    "final_EastRiv", "final_TaylorRiv")
#' my_trial_nums <- c(3,3,3,3)
#' all_output <- list()
#'
#' for (i in seq_len(my_proj_names)){
#'
#'   output_name <- paste0("output_",i)
#'
#'   all_output[output_name] <- read_output(
#'     dir_root = "/Users/me/mywork/",
#'     project_name = test_proj_names[i],
#'     trial_number = test_trial_nums[i],
#'     var_names = c("swe","q"),
#'     metrics = "gof",
#'     list_type = "gof")
#' }
#'
#' @export
read_output <- function(
    dir_root,
    project_name,
    trial_number,
    var_names,
    metrics,
    list_type)
{

  list_output <- list()

  directories <- pwsMultiObsR:::fm_trial_set(dir_root, project_name,
                                             trial_number)
  type <- directories$trial_type
  dir_dynamic_input <- directories$dir_dynamic_input
  dir_dynamic_output <- directories$dir_dynamic_output

  if (!type %in% c("morris", "mmlhs")) {
    stop("Invalid trial type. Must be 'morris' or 'mmlhs'.")
  }

  if (!list_type %in% c("gof", "eet")) {
    stop("Invalid list type. Must be 'gof' or 'eet'.")
  }

  if (type == "mmlhs" && list_type == "eet"){
    stop("Invalid list type. No  lists for mmlhs trials.")
  }

  if (type == "mmlhs"){

    params <- read.csv(paste0(dir_dynamic_input,"/!mmlhs_params.csv"))

    list_data <- list()
    for (var in var_names) {
      for (metric in metrics) {
        list_data <- c(list_data, .read_json(
          list_type, var, metric, dir_dynamic_output))
      }
    }
  } else if (type == "morris"){

    params <- read.csv(paste0(dir_dynamic_input,"/!morris_params.csv"))

    list_data <- list()
    for (var in var_names) {
      for (metric in metrics) {
        list_data <- c(list_data, .read_json(
          list_type, var, metric, dir_dynamic_output))
      }
    }
  }

  return(list_data)
}


#' Load example pywatershed files and observations
#'
#' This package contains example model files and observations for the
#' Blue, Dolores, East, and Taylor river in Colorado, USA.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param example_catchment character: name of the example catchment, must be
#' either 'BlueRiv', 'DoloresRiv', 'EastRiv', or 'TaylorRiv'.
#' @export
load_example_inputs <- function(
    dir_root,
    project_name,
    example_catchment){

  if (!(example_catchment %in% c(
    'BlueRiv', 'DoloresRiv', 'EastRiv', 'TaylorRiv'))){
    stop("example_catchment must be named
         'BlueRiv', 'DoloresRiv', 'EastRiv', or 'TaylorRiv'")
  }

  dir_project <- file.path(dir_root, project_name)

  # Get source files
  src_aso <- system.file(
    file.path("example_obs", example_catchment, "aso_data_df.csv"),
    package = "pwsMultiObsR")

  src_mod10a1 <- system.file(
    file.path("example_obs", example_catchment, "mod10a1_data_df.csv"),
    package = "pwsMultiObsR")

  src_smap <- system.file(
    file.path("example_obs", example_catchment, "smapsfwt_data_df.csv"),
    package = "pwsMultiObsR")

  src_openet <- system.file(
    file.path("example_obs", example_catchment, "openet_data_df.csv"),
    package = "pwsMultiObsR")

  src_input <- list.files(
    system.file(
      file.path("example_input", example_catchment),
      package = "pwsMultiObsR"), full.names = TRUE)

  src_gis <- list.files(
    system.file(
      file.path("example_input", example_catchment, "GIS"),
      package = "pwsMultiObsR"), full.names = TRUE)

  # Copy source files
  file.copy(from = src_aso,
            to = file.path(dir_project, "obs/aso"),
            overwrite = FALSE)
  file.copy(from = src_mod10a1,
            to = file.path(dir_project, "obs/mod10a1"),
            overwrite = FALSE)

  file.copy(from = src_smap,
            to = file.path(dir_project, "obs/smap"),
            overwrite = FALSE)

  file.copy(from = src_openet,
            to = file.path(dir_project, "obs/openet"),
            overwrite = FALSE)

  file.copy(from = input_src,
            to = file.path(dir_project, "default_input"),
            overwrite = FALSE)

  file.copy(from = gis_src,
            to = file.path(dir_project, "gis"),
            overwrite = FALSE)

  return(invisible(NULL))
}


