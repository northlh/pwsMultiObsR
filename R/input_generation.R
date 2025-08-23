# This module generates parameter files and is only called internally in


#' Retrieve pws parameter attributes
#'
#' This function takes your specified working directory and creates a child
#' directory specific to the project name and relevant input/output directories
#'
#' @param param_names character vector: names of pws calibration parameters
#' @return data.frame: where row 1 and row 2 are min, max values respectively
.ig_create_attributes <- function(param_names) {
  # Define the full parameter attributes (hardcoded for now)

  param_attributes <- data.frame(
    # CLIMATE ---
    # Markstrom2016 & Mei2022
    adjmix_rain = c(0.6, 1.4, "Climate", NA), # [nhru, nmonths]
    #### tmax_allrain = c(-8, 60), #Markstrom 2016
    tmax_allrain_offset = c(-8, 60, "Climate", "˚F"), # [nhru, nmonths]
    tmax_allsnow = c(-10, 40, "Climate", "˚F"), # [nhru, nmonths]
    # Mei2022
    snow_cbh_adj = c(0.5, 2, "Climate", NA),
    rain_cbh_adj = c(0.5, 2, "Climate", NA),
    tmin_cbh_adj = c(-10, 10, "Climate", "˚F"),
    tmax_cbh_adj = c(-10, 10, "Climate", "˚F"),


    # SOLAR RADIATION ---
    # Markstrom2016
    dday_intcp = c(-60, 10, "Solar", "dday"), # [nhru, nmonths]
    dday_slope = c(0.2, 0.9, "Solar", "dday/˚F"), # [nhru, nmonths]
    ppt_rad_adj = c(0, 0.5, "Solar", "in"), # [nhru, nmonths]
    radj_sppt = c(0, 1, "Solar", NA), # [nhru] constant
    radj_wppt = c(0, 1, "Solar", NA), # [nhru] constant
    radmax = c(0.1, 1, "Solar", NA), # [nhru, nmonths]
    tmax_index = c(-10, 110, "Solar", "˚F"), # [nhru, nmonths]


    # POTETNIAL ET ---
    # Markstrom2016 & Mei2022
    jh_coef = c(0.005, 0.06, "PET", "1/˚F"), # [nhru, nmonths]
    jh_coef_hru = c(5, 25, "PET", "1/˚F"), # [nhru]


    # INTERCEPTION
    # Markstrom2016
    srain_intcp = c(0, 1, "Interception", "in"), # [nhru]
    wrain_intcp = c(0, 1, "Interception", "in"), # [nhru]
    # North2025
    #### cov_type = integer(0,1,2,3,4), #######
    # nhru #in the output transp_on, it is always on, uses only summer covden
    covden_sum = c(0, 1, "Interception", NA), # nhru
    covden_win = c(0, 1, "Interception", NA), # nhru
    snow_intcp = c(0, 1, "Interception", "in"), # nhru


    # SNOW ---
    # Markstrom2016 & Mei2022
    potet_sublim = c(0.1, 0.75, "Snow", NA), # [nhru]
    emis_noppt = c(0.757, 1, "Snow", NA), # [nhru]
    cecn_coef = c(2, 10, "Snow", "cal/˚C>0"), # [nhru, nmonths]
    # Markstrom2016
    freeh2o_cap = c(0.01, 0.2, "Snow", NA), # [nhru]
    # Mei2022
    den_init = c(0.01, 0.5, "Snow", "kg/L"),
    den_max = c(0.1, 0.8, "Snow", "kg/L"),
    settle_const = c(0.01, 0.5, "Snow", NA),
    # Hay 23
    rad_trncf = c(0, 1, "Snow", NA),
    # North2025
    albset_rna = c(0.4, 1, "Snow", NA),
    albset_rnm = c(0.4, 1, "Snow", NA),
    albset_sna = c(0.01, 1.0, "Snow", "in"),
    albset_snm = c(0.1, 1, "Snow", "in"),
    # snarea_thresh = c(0, 200), #nhru
    ### hru_deplcrv = seq(1, 22, by = 1), #####nhru
    ### melt_force = c(1, 366), integer
    ### melt_look = c(1, 366), integer
    ### snarea_curve = seq(0, 1, by = 0.1), #ndeplval
    ### tstorm_mo = (0 OR 1), #nmonths



    # SURFACE RUNOFF
    # Markstrom2016 & Mei2022
    carea_max = c(0, 1, "Runoff", NA), # [nhru]
    smidx_coef = c(0.001, 0.06, "Runoff", NA), # [nhru]
    smidx_exp = c(0.1, 0.5, "Runoff", "1/in"), # [nhru]
    # Mei2022
    snowinfil_max = c(0, 20, "Runoff", "in/day"),
    imperv_stor_max = c(0, 0.1, "Runoff", "in"), # [nhru]

    # SOIL ZONE
    # Markstrom2016 & Mei2022
    fastcoef_lin = c(0.001, 0.8, "Soil", "frac/day"), # [nhru]
    fastcoef_sq = c(0.001, 1, "Soil", NA), # [nhru]
    pref_flow_den = c(0, 0.1, "Soil", NA), # [nhru]
    sat_threshold = c(1, 999, "Soil", "in"), # [nhru]
    slowcoef_lin = c(0.001, 0.5, "Soil", "frac/day"), # [nhru]
    slowcoef_sq = c(0.001, 1, "Soil", NA), # [nhru]
    soil2gw_max = c(0, 0.5, "Soil", "in"), # [nhru]
    soil_moist_max = c(0.001, 10, "Soil", "in"), # [nhru]
    # soil_rechr_max = c(0.001, 5), #[nhru] # deprecated? see hay 23
    soil_rechr_max_frac = c(0.1, 1, "Soil", NA), # [nhru]
    ssr2gw_exp = c(0, 3, "Soil", NA), # [nssr]
    ssr2gw_rate = c(0.05, 0.8, "Soil", "frac/day"), # [nssr]
    # Markstrom2016
    transp_tmax = c(0, 1000, "Soil", "˚F"), # [nhru]


    # GROUNDWATER
    # Markstrom2016 & Mei2022
    gwflow_coef = c(0.001, 0.5, "Groundwater", "frac/day"), # [ngw]

    stringsAsFactors = FALSE
  )

  # Warn if there are duplicate parameter names
  duplicate_params <- param_names[duplicated(param_names)]
  if (length(duplicate_params) > 0) {
    msg <- paste0(
      "The parameters were provided as duplicates : ",
      paste(unique(duplicate_params), collapse = ", ")
    )
    log_error(msg)
    stop(msg)
  }

  # Validate and filter param_attributes based on user-provided param_names
  valid_params <- param_names[param_names %in% colnames(param_attributes)]
  invalid_params <- setdiff(param_names, valid_params)

  # Warn about any invalid parameter names
  if (length(invalid_params) > 0) {
    msg <- paste0(
      "The following parameters are not found in param_attributes: ",
      paste(invalid_params, collapse = ", ")
    )
    log_error(msg)
    stop(msg)
  }

  # Filter the param_attributes dataframe for valid parameters
  filtered_params <- param_attributes[, valid_params, drop = FALSE]

  rownames(filtered_params) <- c("min", "max", "module", "units")
  long <- as.data.frame(t(filtered_params), stringsAsFactors = FALSE)
  long$name <- rownames(long)

  # Reorder + coerce types
  long <- long[, c("name", "min", "max", "units", "module")]
  long$min <- as.numeric(long$min)
  long$max <- as.numeric(long$max)
  long$units <- as.character(long$units)
  long$module <- as.character(long$module)

  return(long)
}


#' Read .param file dimensions
#'
#' This function parses the dimensions section of a legacy PRMS .param file
#'
#' @param file_path character: string containing full path of target param file
#' @return dimensions list: a list containing the dimensions section
.ig_dims_read <- function(file_path) {
  lines <- readLines(file_path)
  dim_sect_upr <- which(lines == "** Dimensions **") # start of dims section
  dim_sect_lowr <- which(lines == "** Parameters **") # end of dims section

  dimensions <- list()
  for (i in dim_sect_upr:dim_sect_lowr) {
    if (lines[i] == "####") { # delimiter in param file
      dim_name <- lines[i + 1]
      dim_value <- as.numeric(lines[i + 2])

      # store in nested list
      dimensions[[dim_name]] <- list(
        dim_name = dim_name,
        dim_value = dim_value
      )
    }
  }
  return(dimensions)
}



#' Write .param file dimensions
#'
#' This function writes the dimensions section of a legacy PRMS .param file.
#' This section is not modified during parameter file generation.
#'
#' @param file_path character: string containing full path of target param file
#' @param dimensions list: contains the dimensions from .ig_read_dims
#' @param run integer: declares run number when run inside .ig_sample functions
#' @return dimensions list: a list containing the dimensions section
.ig_dims_write <- function(file_path, dimensions, run) {
  script_path <- normalizePath(".")
  timestamp <- Sys.time()

  # Param file header
  lines <- c(
    paste("Written by", script_path),
    paste("Timestamp:", timestamp, "Model Run:", run),
    "** Dimensions **"
  )

  for (dim_name in names(dimensions)) {
    dim <- dimensions[[dim_name]]
    lines <- c(lines, "####", dim$dim_name, as.character(dim$dim_value))
  }

  lines <- c(lines, "** Parameters **")
  writeLines(lines, file_path)
  return(invisible(NULL))
}



#' Read .param file parameters
#'
#' This function parses the parameters section of a legacy PRMS .param file.
#' The format is standardized, based on USGS documentation
#' https://pubs.usgs.gov/tm/6b7/pdf/tm6-b7.pdf page 129
#'
#' @param file_path character: string containing full path of target param file
#' @param dimensions list: contains info on dimension sizes from .ig_read_dims
#' @return parameters list: a list containing the parameters section
.ig_params_read <- function(file_path, dimensions) {
  lines <- readLines(file_path)
  param_sect_upr <- which(lines == "** Parameters **")
  param_sect_lowr <- length(lines)

  parameters <- list()
  for (i in param_sect_upr:param_sect_lowr) {
    if (lines[i] == "####") {
      i <- i + 1 # Parameter names
      param_name <- lines[i]

      i <- i + 1 # Number of dimensions
      num_dims <- as.numeric(lines[i])

      i <- i + 1 # Dimension names
      param_dim_names <- c()
      for (j in 1:num_dims) {
        param_dim_names <- c(param_dim_names, lines[i])
        i <- i + 1
      }

      num_values <- as.numeric(lines[i]) # Number of values

      i <- i + 1 # Data class
      data_class <- as.integer(lines[i])

      # Get dimension sizes from dimensions object
      param_dim_values <- c()
      for (k in param_dim_names) {
        param_dim_values[k] <- as.numeric(dimensions[[k]]$dim_value)
      }

      # Extract values (1D and 2D are the only)
      # See PRMS doc page 129, 2D arrays are nhru rows by nmonths columns
      if (num_dims == 2) {
        param_value_matrix <- as.matrix(as.numeric(
          matrix(
            data = lines[(i + 1):(i + num_values)], # gets all values
            nrow = param_dim_values[1],
            ncol = param_dim_values[2]
          )
        ))
      }

      if (num_dims == 1) {
        param_value_matrix <- as.numeric(lines[(i + 1):(i + num_values)])
      }

      # Store in a list
      parameters[[param_name]] <- list(
        num_dims = num_dims,
        param_dim_names = param_dim_names,
        num_values = num_values,
        data_class = data_class,
        param_dim_values = param_dim_values,
        param_value_matrix = param_value_matrix
      )
    }
  }
  return(parameters)
}



#' Write .param file parameters
#'
#' This function writes the parameters section of a legacy PRMS .param file.
#'
#' @param file_path character: string containing full path of target param file
#' @param parameters list: contains the parameters from .ig_read_params
#' @return parameters list: a list containing the parameters section
.ig_params_write <- function(parameters, file_path) {
  lines <- readLines(file_path) # Read the existing lines
  param_sect_start <- which(lines == "** Parameters **") + 0

  param_lines <- c()
  for (param_name in names(parameters)) {
    param <- parameters[[param_name]]
    param_lines <- c(param_lines, "####", param_name)
    param_lines <- c(param_lines, as.character(param$num_dims))
    param_lines <- c(param_lines, param$param_dim_names)
    param_lines <- c(param_lines, as.character(param$num_values))
    param_lines <- c(param_lines, as.character(param$data_class))

    if (param$num_dims == 2) {
      values <- as.character(as.vector(t(param$param_value_matrix)))
      param_lines <- c(param_lines, values)
    }

    if (param$num_dims == 1) {
      values <- as.character(param$param_value_matrix)
      param_lines <- c(param_lines, values)
    }
  }

  new_lines <- c(lines[1:param_sect_start], param_lines)
  writeLines(new_lines, file_path)
  return(invisible(NULL))
}



#' Normalize absolute parameter values
#'
#' This function normalizes parameter values between 0 and 1
#'
#' @param x_a numeric: vector of absolute param values
#' @param max_x_a numeric: the max value of x_a
#' @param min_x_a numeric: the min value of x_a
#' @return x_n numeric: a vector of param values normalized to 0 and 1
.ig_normalize_minmax <- function(x_a, max_x_a, min_x_a) {
  x_n <- (x_a - min_x_a) / (max_x_a - min_x_a)
  return(x_n)
}



#' Reverse 0 to 1 normalization
#'
#' This function reverses 0 to 1 normalization to absolute magnitudes
#'
#' @param x_n numeric: vector of normalized param values
#' @param max_x_a numeric: the max value of x_a
#' @param min_x_a numeric: the min value of x_a
#' @return x_a numeric: a vector of actual param values
.ig_normalize_reverse <- function(x_n, max_x_a, min_x_a) {
  x_a <- (x_n * (max_x_a * min_x_a)) + min_x_a
  return(x_a)
}




#' Perturb parameter values
#'
#' This function creates new parameter values based on sampling values
#' produced from morris or mmlhs functions.
#'
#' @param sampling_matrix numeric: 2D matrix of normalized parameters.
#' The dimensions are nruns (rows) by nparams (cols).
#' @param dimensions list: a list containing the dimensions section
#' @param parameters list: a list containing the parameters section
#' @param param_attributes data.frame: contains columns named min, max
#' @param dir_dynamic_input character: path to where the param file is written
#' values respectively. Columns must match that of the sampling_matrix.
#' @return NULL: written parameter file
.ig_perturb_params <- function(sampling_matrix,
                               dimensions,
                               parameters,
                               param_attributes,
                               dir_dynamic_input,
                               nruns) {
  for (run in seq_len(nrow(sampling_matrix))) { # each perturbation
    for (param_name in names(parameters)) { # each parameter

      param_data <- parameters[[param_name]] # extract data from parameters obj

      # Apply only to user specified parameters
      if (param_name %in% rownames(param_attributes)) {
        min_val <- as.numeric(param_attributes[param_name, "min"])
        max_val <- as.numeric(param_attributes[param_name, "max"])

        experiment <- sampling_matrix[run, param_name] # perturb val (0 to 1)

        # Handle scalar parameters
        if (param_data$num_values == 1) {
          new_vals <- .ig_normalize_reverse(
            x_n = experiment,
            max_x_a = max_val,
            min_x_a = min_val
          )

          # Handle non-scalar parameters
        } else {
          param_vals <- param_data$param_value_matrix # extracts default vals
          sigma <- sd(param_vals, na.rm = TRUE) # find default variance
          mu <- mean(param_vals) # find default mean

          if (is.na(sigma) || sigma == 0) { # case where values are constant
            mu_new <- .ig_normalize_reverse(
              x_n = experiment,
              max_x_a = max_val,
              min_x_a = min_val
            )
            new_vals <- matrix(
              mu_new,
              nrow = param_data$num_values,
              ncol = length(param_vals) / param_data$num_values
            ) # just 1
          } else { # case where params vary spatially and temporally
            # IMPORTANT METHODOLOGICAL CHOICE, Use the mean page 11
            # https://pubs.usgs.gov/of/2006/1323/pdf/OF06-1323_508.pdf
            const <- abs(min_val) + 10 # eqn2
            actual_min <- ((((min_val + const) * (mu + const)) /
              (min(param_vals) + const)) - const) # eqn3
            actual_max <- ((((max_val + const) * (mu + const)) /
              (max(param_vals) + const)) - const) # eqn4
            mu_new <- .ig_normalize_reverse(
              x_n = experiment,
              max_x_a = actual_max,
              min_x_a = actual_min
            )
            new_vals <- ((((mu_new + const) * (param_vals + const)) /
              (mu + const)) - const) # eqn1
          }
        }
        # append modified params to "parameters" object
        parameters[[param_name]]$param_value_matrix <- new_vals
      }
    }

    # Write to the dynamic directory for this specific run
    run_tagged_path <- file.path(
      dir_dynamic_input,
      paste0("myparam_", run, ".param")
    )
    .ig_dims_write(
      dimensions = dimensions,
      file_path = run_tagged_path,
      run = run
    )
    .ig_params_write(
      parameters = parameters,
      file_path = run_tagged_path
    )

    cat("\rWriting file:", run, " of ", nruns)
  }

  cat("\n")
  log_info("Parameter file writing complete")
}



#' Create morris parameter files
#'
#' This function create perturbed parameter files via the Morris Elementary
#' Effects method
#'
#' @param r numeric: 2D matrix of normalized parameters.
#' @param param_attributes data.frame: contains columns named min, max
#' @param dir_defualt_input character: path where the original param file exists
#' @param dir_dynamic_input character: path where the param files are written
#' @return list_morris list: Sampling design, param values, sample matrix,
#' @import sensitivity
.ig_sample_morris <- function(r,
                              param_attributes,
                              dir_dynamic_input) {

  # Load default parameter file
  dimensions <- .ig_dims_read(paste0(
    dir_dynamic_input,
    "/default_input/myparam.param"
  ))
  parameters <- .ig_params_read(
    paste0(dir_dynamic_input, "/default_input/myparam.param"),
    dimensions = dimensions
  )

  # Create nruns object for parallel runs
  nruns <- r * (nrow(param_attributes) + 1)

  # CREATE EET OBJECT

  morris_design <- sensitivity::morris(
    model = NULL,
    factors = nrow(param_attributes), # number of factors
    r = r, # num trajectories for each var 10-100 pianosi
    design = list(
      type = "oat",
      levels = 8, # num of discrete vals, 4-8 pianosi et. al.
      grid.jump = 4
    ), # levels/2
    binf = param_attributes$min,
    bsup = param_attributes$max,
    scale = TRUE
  )

  colnames(morris_design$X) <- rownames(param_attributes) # rename advised

  # ON THE "SCALE" ARGUMENT:
  # despite having no apparent effect on the values in $X,
  # the scale argument shows up in tell.morris when calculating EE
  # see source code: https://rdrr.io/cran/sensitivity/src/R/morris.R
  # use TRUE for values of different units:
  # https://www.rdocumentation.org/packages/sensitivity/versions/1.30.1/topics/morris

  # Setup abbreviations to scale to 0 and 1 (the fn argument does not work?)
  # ms <- as.data.frame(morris_design$X) #matrix to df
  ms <- morris_design$X
  pa <- param_attributes

  # For morris, we turn parameters into the normalized 0 to 1 samp matrix
  sampling_matrix <- matrix(NA, nrow = nrow(ms), ncol = ncol(ms))

  for (j in seq_len(ncol(ms))) {
    sampling_matrix[, j] <- .ig_normalize_minmax(
      x_a = ms[, j],
      max_x_a = pa[j, "max"],
      min_x_a = pa[j, "min"]
    )
  }

  colnames(sampling_matrix) <- rownames(param_attributes)

  saveRDS(morris_design, paste0(dir_dynamic_input, "/!morris_design.rds"))
  write.csv(sampling_matrix, paste0(dir_dynamic_input, "/!morris_samples.csv"),
    row.names = FALSE
  )
  write.csv(ms, paste0(dir_dynamic_input, "/!morris_params.csv"),
    row.names = FALSE
  )
  write.csv(
    param_attributes, paste0(dir_dynamic_input, "/!param_attributes.csv"),
    row.names = FALSE
  )

  msg <- paste0(
    "morris R object written to: ", dir_dynamic_input,
    "/!morris_design.rds"
  )
  log_info(msg)

  msg <- paste0(
    "morris sampling matrix written to: ", dir_dynamic_input,
    "/!morris_samples.csv"
  )
  log_info(msg)

  msg <- paste0(
    "morris parameter matrix written to: ", dir_dynamic_input,
    "/!morris_params.csv"
  )
  log_info(msg)

  msg <- paste0(
    "parameter attributes written to: ", dir_dynamic_input,
    "/!param_attributes.csv"
  )
  log_info(msg)

  # Perturb and write the parameter files
  .ig_perturb_params(
    sampling_matrix = sampling_matrix,
    dimensions = dimensions,
    parameters = parameters,
    param_attributes = param_attributes,
    dir_dynamic_input = dir_dynamic_input,
    nruns = nruns
  )

  return(invisible(NULL))
}




#' Create mmlhs parameter files
#'
#' This function create perturbed parameter files via the Maximin Latin
#' Hypercube Sampling method
#'
#' @param nruns integer: Number of desired runs (should be 1000 * param number)
#' @param param_attributes data.frame: contains columns named min, max
#' @param dir_defualt_input character: path where the original param file exists
#' @param dir_dynamic_input character: path where the param files are written
#' @return list_mmlhs list: Param values, sample matrix,
#' @import lhs
.ig_sample_mmlhs <- function(nruns,
                             param_attributes,
                             dir_dynamic_input) {

  # Load default parameter file
  dimensions <- .ig_dims_read(
    paste0(dir_dynamic_input, "/default_input/myparam.param")
  )
  parameters <- .ig_params_read(
    paste0(dir_dynamic_input, "/default_input/myparam.param"),
    dimensions = dimensions
  )

  # CREATE LHS OBJECT

  # Give the user some info on this function
  time_start <- Sys.time()
  msg <- paste("Starting lhs::maximinLHS at:", time_start)
  log_info(msg)

  # CREATE LHS OBJECT
  sampling_matrix <- lhs::maximinLHS(
    n = nruns,
    k = nrow(param_attributes)
  )

  time_lhs_finish <- Sys.time()
  msg <- paste("lhs::maximinLHS completed at:", time_lhs_finish)
  log_info(msg)
  time_lhs_total <- time_lhs_finish - time_start
  msg <- paste(
    "Total time for lhs:: object creation:", time_lhs_total,
    "for", nruns, "samples"
  )
  log_info(msg)


  # Reverse normalization to store actual param values
  mmlhs <- sampling_matrix
  pa <- param_attributes

  mmlhs_params <- matrix(NA, nrow = nrow(mmlhs), ncol = ncol(mmlhs))
  for (j in seq_len(ncol(mmlhs))) {
    mmlhs_params[, j] <- .ig_normalize_reverse(
      x_n = mmlhs[, j],
      max_x_a = pa[j, "max"],
      min_x_a = pa[j, "min"]
    )
  }

  colnames(sampling_matrix) <- rownames(param_attributes)
  colnames(mmlhs_params) <- rownames(param_attributes)

  write.csv(sampling_matrix, paste0(dir_dynamic_input, "/!mmlhs_samples.csv"),
    row.names = FALSE
  )
  write.csv(mmlhs_params, paste0(dir_dynamic_input, "/!mmlhs_params.csv"),
    row.names = FALSE
  )
  write.csv(
    param_attributes, paste0(dir_dynamic_input, "/!param_attributes.csv"),
    row.names = FALSE
  )

  msg <- paste0(
    "mmlhs sampling matrix written to: ", dir_dynamic_input,
    "/!mmlhs_samples.csv"
  )
  log_info(msg)

  msg <- paste0(
    "mmlhs parameter matrix written to: ", dir_dynamic_input,
    "/!mmlhs_params.csv"
  )
  log_info(msg)

  msg <- paste0(
    "parameter attributes written to: ", dir_dynamic_input,
    "/!param_attributes.csv"
  )
  log_info(msg)

  # Perturb and write the parameter files
  .ig_perturb_params(
    sampling_matrix = sampling_matrix,
    dimensions = dimensions,
    parameters = parameters,
    param_attributes = param_attributes,
    dir_dynamic_input = dir_dynamic_input,
    nruns = nruns
  )

  # End timer and display elapsed time
  time_end <- Sys.time()
  time_write_total <- time_end - time_lhs_finish
  time_total <- time_end - time_start
  msg <- paste("Total write time:", time_write_total)
  log_info(msg)
  msg <- paste("Total elapsed time:", time_total, "for", nruns, "model runs")
  log_info(msg)

  return(invisible(NULL))
}
