# This module houses post-processing tools for the Morris sensitivity analysis


# TODO change datastructure to nd array instead of deeply nested lists?
# TODO maybe separate the plotting? Give optional plot write.
# TODO add flexibility for logis initial params


#' Function to load JSON files conditionally based on variables and metrics
#' @import jsonlite
.load_json_if_exists <- function(var, metric, dir_dynamic_output) {
  path <- file.path(
    dir_dynamic_output, paste0("/!list_", metric, "_", var, ".json")
  )
  if (file.exists(path)) {
    return(jsonlite::fromJSON(path))
  } else {
    stop(paste("File not found:", path))
  }
}


#' Helper function to split lists into vectors of length nruns
#'
#' For 3D arrays, the dims are nruns, nyears, nhru
.split_to_vectors <- function(lst) {
  if (is.array(lst) && length(dim(lst)) == 3) {
    nyears <- dim(lst)[2]
    nhru <- dim(lst)[3]
    return(lapply(
      1:nyears,
      function(i) lapply(1:nhru, function(j) lst[, i, j])
    ))
  } else if (is.matrix(lst)) {
    return(lapply(seq_len(ncol(lst)), function(i) lst[, i]))
  } else if (is.list(lst)) {
    return(lapply(lst, .split_to_vectors))
  } else {
    return(lst)
  }
}


#' Extract Elementary Effects (EEs)
#' @import sensitivity
.extract_ee <- function(y, morris_design) {
  if (length(y) == nrow(morris_design$X)) {
    ee_matrix <- sensitivity::tell(x = morris_design, y = y)$ee
    return(ee_matrix)
  } else {
    stop("Vector length mismatch with morris_design")
  }
}


#' Compute mu, mu_star, and sigma for EEs
#'
#' Based on documentation in the sensitivity package
#' https://cran.r-project.org/web/packages/sensitivity/sensitivity.pdf
#' page 31
.compute_measures <- function(ee_matrix) {
  mu <- apply(ee_matrix, 2, mean)
  mu_star <- apply(ee_matrix, 2, function(x) mean(abs(x)))
  sigma <- apply(ee_matrix, 2, sd)
  eta_star <- mu_star / max(mu_star) # From Herman, Cuntz, etc
  nu_star <- mu_star / sum(mu_star) # Normalized by total, called nu_star by me
  return(list(
    mu = mu, mu_star = mu_star, sigma = sigma,
    eta_star = eta_star, nu_star = nu_star
  ))
}


#' replicate samples of the the ee vector
.bootstrap_ee <- function(y, n_boot, sd_level, morris_design) {
  # Compute the initial elementary effects matrix
  ee_matrix <- .extract_ee(y, morris_design)

  # Initialize storage for bootstrap results
  n_params <- ncol(ee_matrix)
  bootstrap_samples <- vector("list", n_params)

  # Resample the EE of each parameter independently
  # This returns a list containing morris results for each B sample
  for (param_idx in 1:n_params) {
    param_values <- ee_matrix[, param_idx]
    bootstrap_samples[[param_idx]] <- replicate(n_boot,
      {
        sampled_values <- sample(
          param_values, length(param_values),
          replace = TRUE
        )
        mu <- mean(sampled_values)
        mu_star <- mean(abs(sampled_values))
        sigma <- sd(sampled_values)
        list(mu = mu, mu_star = mu_star, sigma = sigma)
      },
      simplify = FALSE
    )
  }

  # Combine results into matrices for mu, mu_star, and sigma
  mu_boot <- do.call(cbind, lapply(
    bootstrap_samples, function(res) sapply(res, `[[`, "mu")
  ))
  mu_star_boot <- do.call(cbind, lapply(
    bootstrap_samples, function(res) sapply(res, `[[`, "mu_star")
  ))
  sigma_boot <- do.call(cbind, lapply(
    bootstrap_samples, function(res) sapply(res, `[[`, "sigma")
  ))

  # Compute mean and standard deviation for each parameter
  # This is equation 29 and 30 in Campolongo, 1997
  mu_mean <- colMeans(mu_boot, na.rm = TRUE)
  mu_star_mean <- colMeans(mu_star_boot, na.rm = TRUE)
  sigma_mean <- colMeans(sigma_boot, na.rm = TRUE)

  mu_sd <- apply(mu_boot, 2, sd, na.rm = TRUE)
  mu_star_sd <- apply(mu_star_boot, 2, sd, na.rm = TRUE)
  sigma_sd <- apply(sigma_boot, 2, sd, na.rm = TRUE)

  # Calculate the confidence interval as Mean ± SD × sd_level
  mu_lower <- mu_mean - mu_sd * sd_level
  mu_upper <- mu_mean + mu_sd * sd_level

  mu_star_lower <- mu_star_mean - mu_star_sd * sd_level
  mu_star_upper <- mu_star_mean + mu_star_sd * sd_level

  sigma_lower <- sigma_mean - sigma_sd * sd_level
  sigma_upper <- sigma_mean + sigma_sd * sd_level

  # Calculate statistics dependent on mu_star estimates
  # NOTE: bound relative to max(mean)
  # upper bound on most sensi param is >1 in Cuntz, 2015
  eta_star_mean <- mu_star_mean / max(mu_star_mean)
  eta_star_lower <- mu_star_lower / max(mu_star_mean)
  eta_star_upper <- mu_star_upper / max(mu_star_mean)

  nu_star_mean <- mu_star_mean / sum(mu_star_mean)
  nu_star_lower <- mu_star_lower / sum(mu_star_mean)
  nu_star_upper <- mu_star_upper / sum(mu_star_mean)

  # Create the result matrices
  ci_results <- list(
    mu = cbind(
      lower = mu_lower,
      mean = mu_mean,
      upper = mu_upper
    ),
    mu_star = cbind(
      lower = mu_star_lower,
      mean = mu_star_mean,
      upper = mu_star_upper
    ),
    sigma = cbind(
      lower = sigma_lower,
      mean = sigma_mean,
      upper = sigma_upper
    ),
    eta_star = cbind(
      lower = eta_star_lower,
      mean = eta_star_mean,
      upper = eta_star_upper
    ),
    nu_star = cbind(
      lower = nu_star_lower,
      nean = nu_star_mean,
      upper = nu_star_upper
    )
  )

  # Assign column names for clarity
  rownames(ci_results$mu) <-
    rownames(ci_results$mu_star) <-
    rownames(ci_results$sigma) <-
    rownames(ci_results$eta_star) <-
    rownames(ci_results$nu_star) <- colnames(ee_matrix)

  return(ci_results)
}


#' Recursive function to apply EE and optionally bootstrap
.recursive_apply <- function(lst, n_boot, sd_level, morris_design) {
  if (is.list(lst)) {
    return(lapply(
      lst,
      .recursive_apply,
      n_boot = n_boot,
      sd_level = sd_level,
      morris_design = morris_design
    ))
  } else if (is.numeric(lst) && length(lst) == nrow(morris_design$X)) {
    ee_matrix <- .extract_ee(lst, morris_design)
    baseline <- .compute_measures(ee_matrix)

    if (!is.na(n_boot) && !is.na(sd_level)) { # run boot if not NA
      bootstrap <- .bootstrap_ee(lst, n_boot, sd_level, morris_design)
      return(list(baseline = baseline, bootstrap = bootstrap))
    } else {
      return(list(baseline = baseline))
    }
  } else {
    stop("Encountered unexpected datatype in output lists")
  }
}



#' Compute Morris Elementary effect metrics
#'
#' Recursively compute morris elementary effects and sensitivity indices along
#' the model outputs (GOF or general statistics)
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param trial_number integer: trial number, leading zeros not needed
#' @param variables character: single or vector of observation types. Must be
#' one of c("swe", "q", "sms2", "sms8", "sms20", "smapsfwt", "aso", "mod10a1",
#'  "openet")
#' @param metrics chracter: single or vector of ouptut list type. Must be one
#'  of c("gof", "metric")
#' @param n_boot integer: number of bootstrap replicates of the elementary
#'  effects vector. Set to NA to retrieve baseline results
#'  without bootstrapping (about 100x faster).
#' @param sd_level numeric: number of standard devaitions for bootstrap upper
#' and lower bounds. Should be 1 to match Cuntz et al., 2015, or 1.95 to
#'  match Campolongo et al., 1997. Set to NA to retrieve baseline results
#'  without bootstrapping (about 100x faster).
#' @return list: nested list of metrics for each observation type specified
#'
#' @import jsonlite
#' @export
sensi_eet <- function(dir_root,
                      project_name,
                      trial_number,
                      variables = c("swe", "q"),
                      metrics = c("gof", "metric"),
                      n_boot = 1000,
                      sd_level = 1) {
  directories <- pwsMultiObsR:::fm_trial_set(
    dir_root, project_name,
    trial_number
  )
  dir_dynamic_input <- directories["dir_dynamic_input"]
  dir_dynamic_output <- directories["dir_dynamic_output"]

  # Load the morris design object
  morris_design <- readRDS(file.path(dir_dynamic_input, "/!morris_design.rds"))

  # Load specified data based on `variables` and `metrics`
  list_data <- list()
  for (var in variables) {
    for (metric in metrics) {
      # list_data[[paste0(var, "_", metric)]] <- (
      #   .load_json_if_exists(var, metric, dir_dynamic_output))
      list_data <- c(list_data, .load_json_if_exists(
        var, metric, dir_dynamic_output
      ))
    }
  }

  # Apply the recursive_apply function to each loaded dataset
  results <- list()
  for (key in names(list_data)) {
    if (!is.null(list_data[[key]])) {
      split_data <- .split_to_vectors(list_data[[key]])
      results[[key]] <- .recursive_apply(
        split_data, n_boot,
        sd_level, morris_design
      )
    }
  }

  # Add eet_ flag to the key
  new_names <- paste0("eet_", names(results))
  names(results) <- new_names

  # Save results
  for (key in names(results)) {
    path <- file.path(dir_dynamic_output, paste0("!", key, ".json"))
    jsonlite::write_json(results[key], path, auto_unbox = TRUE, pretty = TRUE)
    message("Morris EET results written at: ", path)
  }

  return(results)
}



# See appendix of Cuntz et al., 2015
# https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1002/2015WR016907
# Computationally inexpensive identification of noninformative model
# parameters by sequential screening


#' Logistic function with offset
#'
#' See appendix of Cuntz et al., 2015
#' https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1002/2015WR016907
#' "Computationally inexpensive identification of noninformative model
#' parameters by sequential screening"
#'
#' @param x integer: param rank
#' @param lo numeric: min y value (eta* value)
#' @param aa numeric: max(y) - min(y)
#' @param k numeric: sigmoid steepness (calibrate from 1)
#' @param x0 numeric: sigmoid midpoint (calibrate from 0.8)
#' @return numeric: fitted y value
.logis_offset <- function(x, lo, aa, k, xo) {
  yy <- lo + (aa / (1 + exp(-k * (x - xo)))) # eqn B1
  return(yy)
}

#' Logistic function signed curvature
#'
#' See appendix of Cuntz et al., 2015
#' https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1002/2015WR016907
#' "Computationally inexpensive identification of noninformative model
#' parameters by sequential screening"
#'
#' @param x integer: param rank
#' @param lo numeric: min y value (eta* value)
#' @param aa numeric: max(y) - min(y)
#' @param k numeric: sigmoid steepness (calibrate from 1)
#' @param x0 numeric: sigmoid midpoint (calibrate from 0.8)
#' @return numeric: signed curvature of the sigmoid
.logis_offset_curv <- function(x, lo, aa, k, xo) {
  deriv1 <- (k * aa) / (2 * (cosh(k * (x - xo)) + 1)) # eqn B2
  deriv2_numer <- (-(k^2) * aa * sinh(k * (x - xo)))
  deriv2_denom <- ((2 * (cosh(k * (x - xo)) + 1))^2)
  deriv2 <- (deriv2_numer / deriv2_denom) # eqn B3
  kappa <- deriv2 / (1 + deriv1^2)^(3 / 2) # eqn C1
  return(kappa)
}


#' Plot logistic function and find statistical errors in parameter sensitivity
#'
#' @description
#' Visualize the parameter rankings relative to eta star and identify the
#' type 1 (false negative) and type 2 (false positive) statistical error in
#' identifying sensitive parameters.
#'
#' @details
#' The threshold for parameter identification is determined
#' by the logistic function, but it generally 0.2 in most cases.
#' The type 1 error is when the bootstrap mean is above the threshold, and the
#' lower bound is below the threshold.Type 2 errors are when the bootstrap mean
#' (mu*) is less than the 0.2 threshold but the upper bound is greater than the
#' threshold (based on +- standard deviation). The type 1 error is when the
#' bootstrap mean is above the threshold, and the lower bound is below the
#' threshold. Generally, a robust sensitivity analysis should be robust to type
#' 2 errors. When the occur, it is recommended to include them as sensitive.
#' Type 1 errors may also remain included.
#'
#' @param df_mean data.frame: bootstrap mean eta* valeus where column names
#' correspond to the observation type and nrows in the the number of parameters.
#' The rows should be named by the parameters for more tangible results.
#' @param df_lower data.frame: bootstrap lower bound
#' @param df_upper data.frame: bootstrap upper bound
#' @param ncol_plot integer: number of columns in the faceted plot
#' @param dir_plot character: path where plot is saved. Obtain using
#' fm_set_trial and inputting the "dir_plot" object.
#' @param plot_filename character: the name of the plot including the .png
#' extension.
#' @return list: nested list of the identified parameters, errors, thresholds,
#' and plots.
#' @import tidyverse
#' @export
fit_logis <- function(df_mean, # of eta star values, nrow = nparams
                      df_lower,
                      df_upper,
                      ncol_plot, #
                      dir_plot,
                      start_xo = 0.8,
                      start_k = 1,
                      plot_filename = "plot_sensi_logis.png") {
  # FIT LOGISTIC FN  ---------------------------------------------------------

  # Initialize lists
  output_list <- list()
  plot_list <- list() # initialize list to store plots
  plot_list_fitted <- list() # initialize list to store plots
  sensi_param_list <- list() # to ID informative parameters
  type1_param_list <- list() # to ID false positives
  type2_param_list <- list() # to ID false negatives
  sensi_thresholds <- list()

  # Loop through columns to fit logistic function to eta_star_df
  for (column_name in colnames(df_mean)) {
    # Extract the data column by column, as the criteria are independent
    # The dataframes should all have the same column names
    original_names <- rownames(df_mean)
    column_mean <- df_mean[[column_name]]
    column_lower <- df_lower[[column_name]]
    column_upper <- df_upper[[column_name]]

    # Rank the sensitivity indices by mean
    rank1 <- rank(column_mean, ties.method = "first")
    sorted_indices <- order(rank1)

    # Sort the columns by the rankings
    column_mean <- column_mean[sorted_indices]
    column_lower <- column_lower[sorted_indices]
    column_upper <- column_upper[sorted_indices]
    rank1 <- rank1[sorted_indices]
    sorted_names <- original_names[sorted_indices]

    # Isolate the numeric column for analysis, needed due to weird nls struct
    response <- column_mean

    # Fit the logistic function with offset using nls
    fit_logis <- tryCatch(
      {
        nls(
          response ~ .logis_offset(
            x = rank1,
            lo = min(response),
            aa = max(response) - min(response),
            k, xo
          ),
          start = list(
            k = start_k, xo = start_xo * length(response)), # Calibrating inits
          control = nls.control(maxiter = 100, tol = 1e-05)
        )
      },
      error = function(e) {
        stop(paste("Error in nls fitting for column:", column_name))
      }
    )

    if (is.null(fit_logis)) {
      stop("Logistic function fitting failed. Try adjusting start parameters.")
    }


    # Generate x-values for smooth curve and curvature calculation
    # X_vales are along the parameter rank
    x_vals <- seq(min(rank1), max(rank1), by = 0.01)

    # Calculate curvatures
    curvatures <- sapply(x_vals, function(x) {
      .logis_offset_curv(
        x,
        lo = min(column_mean),
        aa = max(column_mean) - min(column_mean),
        k = coef(fit_logis)["k"],
        xo = coef(fit_logis)["xo"]
      )
    })

    # Calculate fitted logistic values
    # Y values are L, the sensitivity indices
    fitted_indices <- sapply(x_vals, function(x) {
      .logis_offset(
        x,
        lo = min(column_mean),
        aa = max(column_mean) - min(column_mean),
        k = coef(fit_logis)["k"],
        xo = coef(fit_logis)["xo"]
      )
    })

    # Data frame for plotting
    df_mean_fitted <- data.frame(
      x_vals = x_vals,
      fitted_indices = fitted_indices,
      curvatures = curvatures
    )


    # Calculate the threshold (based on max curvature)
    threshold <- df_mean_fitted$fitted_indices[which.max(curvatures)]
    if (threshold > 0.2) {
      threshold <- 0.2 # Coerce threshold to 0.2 if necessary
    }

    # Store thresholds
    sensi_thresholds[[column_name]] <- threshold

    # Find row names where the values are above the threshold
    params_above_threshold <- sorted_names[column_mean >= threshold]
    params_type_1 <- sorted_names[(column_mean >= threshold) &
      (column_lower < threshold)]
    params_type_2 <- sorted_names[(column_mean < threshold) &
      (column_upper >= threshold)]

    # Append the identifed parameters to the lists
    sensi_param_list[[column_name]] <- params_above_threshold
    type1_param_list[[column_name]] <- params_type_1
    type2_param_list[[column_name]] <- params_type_2

    # Now explicitly pass the data as a data frame for ggplot
    plot_data <- data.frame(
      rank1 = rank1, # sorted x axis
      column_mean = column_mean, # blue points
      column_lower = column_lower, # err bars
      column_upper = column_upper, # err bars
      data_type = rep(column_name, length(column_mean)), # input df, facet vars
      threshold_val = threshold
    ) # threshold value

    plot_data_fitted <- data.frame(
      fitted_x = df_mean_fitted$x_vals, # x of fitted line
      fitted_y = df_mean_fitted$fitted_indices, # y of fitted line)
      data_type = rep(column_name, length(df_mean_fitted$x_vals))
    )

    # Append to list
    plot_list[[column_name]] <- plot_data
    plot_list_fitted[[column_name]] <- plot_data_fitted
  } # close for (column_name in...)



  # AUTO FACET PLOT ----------------------------------------------------

  # Bind into long df
  plot_data_long <- bind_rows(plot_list)
  plot_data_long_fitted <- bind_rows(plot_list_fitted)

  # Replace underscores with spaces in the x-axis labels
  plot_data_long <- plot_data_long %>%
    mutate(data_type = gsub("_", " ", data_type)) # Replace "_" with " "

  plot_data_long_fitted <- plot_data_long_fitted %>%
    mutate(data_type = gsub("_", " ", data_type)) # Replace "_" with " "


  # Create faceted plot...how to specify column num?
  p <- ggplot(plot_data_long) +
    # Add the error bars
    geom_errorbar(aes(x = rank1, ymin = column_lower, ymax = column_upper),
      width = 0.2, color = "gray"
    ) +
    geom_point(aes(x = rank1, y = column_mean), size = 0.5, color = "blue") +
    geom_line(
      data = plot_data_long_fitted, aes(x = fitted_x, y = fitted_y),
      color = "black", linewidth = 0.25
    ) + # Logistic curve
    geom_hline(aes(yintercept = threshold_val),
      linetype = "dashed",
      color = "red", linewidth = 0.5
    ) + # Threshold line
    facet_wrap(~data_type, ncol = ncol_plot) + # or nrow = 2
    labs(x = "Param. Rank", y = "Sensitivity Index (η*)") +
    theme_light(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text.x = element_text(color = "black", size = 6, face = "bold")
    )

  # Display the combined plot
  print(p)

  # ID UNIQUE ALL PARAMS ----------------------------------------------------

  # Find all unique row names from the rowname_list
  all_unique_params <- unique(unlist(sensi_param_list))
  all_unique_type1 <- unique(unlist(type1_param_list))
  all_unique_type2 <- unique(unlist(type2_param_list))

  # Append the unique row names to the rowname_list
  sensi_param_list[["all_unique"]] <- all_unique_params
  type1_param_list[["all_unique_type1"]] <- all_unique_type1
  type2_param_list[["all_unique_type2"]] <- all_unique_type2

  output_list[["sensi_params"]] <- sensi_param_list
  output_list[["type1_params"]] <- type1_param_list
  output_list[["type2_params"]] <- type2_param_list
  output_list[["thresholds"]] <- sensi_thresholds
  output_list[["plot"]] <- p


  # Check if the output plot file already exists
  if (file.exists(file.path(dir_plot, plot_filename))) {
    warning(paste(
      "The file:", plot_filename,
      "already exists. Aborting to avoid overwriting."
    ))
    return(output_list)
  }

  # Save the plot as PNG
  ggsave(
    filename = file.path(dir_plot, plot_filename),
    plot = p, width = 6.5, height = 4, dpi = 300
  )
  cat(
    "Logistic plot has been saved as",
    file.path(dir_plot, plot_filename), " \n"
  )

  return(output_list)
}
