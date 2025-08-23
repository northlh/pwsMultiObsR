# This module houses the analysis workflows for each observation type

# TODO remove hardcode obs data filenames in module wrapper
# TODO function for checks
# TODO function for list/gof calculations
# TODO wrapper for sms functions, avoid repeating
# TODO add checks to swe
# TODO generic functions


.calc_aweighted_avg <- function(row, weights) {
  weighted_avg <- sum(weights * row) / sum(weights)
  return(weighted_avg)
} # close fn


.op_gof_q <- function(usgs_stations,
                      obs, # usgs DATA LIST
                      sim,
                      start_year,
                      end_year,
                      nruns = 1,
                      run = 1,
                      rolling_window = 10,
                      list_gof_q = NULL) {
  # Initialize list_gof_q if it is not provided
  if (is.null(list_gof_q)) {
    list_gof_q <- list()
  }

  # Calculate the total years
  total_years <- end_year - start_year + 1
  perform_rolling <- total_years >= rolling_window

  if (!perform_rolling) {
    warning(paste0(
      "The range of years is less than ", rolling_window,
      ". Rolling ", rolling_window, "-year analysis will be skipped."
    ))
  }

  # Determine rolling window analysis windows if applicable
  if (perform_rolling) {
    rolling_windows <- if (total_years == rolling_window) {
      list(c(start_year, end_year)) # Only one window
    } else {
      lapply(0:(
        total_years - rolling_window), function(i) {
        c(
          start_year + i, start_year + i + rolling_window - 1
        )
      })
    }
  }

  for (i in seq_along(usgs_stations$site_ids)) {
    # Defining IDs in the list
    usgs_site_id <- usgs_stations$site_ids[i] # i object is used in "q_obs"
    seg_id <- usgs_stations$seg_ids[i] # same index as the site
    colname_sim <- paste0("seg_", seg_id) # Dynamically create the column name

    # Checks on sim and obs data ------------------------------------------
    # Need to assign to new variable in looped structure
    sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
    obs_data <- obs[[i]] %>%
      filter(waterYear %in% seq(start_year, end_year))

    # Check for missing values
    num_na <- sum(is.na(obs_data$Q) | is.na(sim_data[[paste0("seg_", seg_id)]]))
    if (num_na > 0) {
      warning(paste(
        "There are", num_na, "NA values for usgs site ID",
        usgs_site_id, "during the analysis period. Removing NAs"
      ))
      # Remove lines with missing values in the intended columns
      sim_data <- sim_data[!is.na(sim_data[[colname_sim]]), ]
      obs_data <- obs_data[!is.na(obs_data$Q_cfs), ] # static colname
    }

    # Check for data presence within the analysis period
    if (nrow(obs_data) == 0) {
      warning(paste("No observations for Q within the analysis period
               (", start_year, "-", end_year, ")."))
      next # Go to next site (instead of aborting)
    }

    # Check if only part of the analysis period has data
    sim_years <- unique(sim_data$waterYear)
    if (min(sim_years) > start_year || max(sim_years) < end_year) {
      warning(paste("Simulation for seg_outflow only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
    }

    # Check if only part of the analysis period has data
    obs_years <- unique(obs_data$waterYear)
    if (min(obs_years) > start_year || max(obs_years) < end_year) {
      warning(paste(
        "Observations for Q only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  For usgs site ID", usgs_site_id,
        ", calculations will be based on this available range."
      ))
    }


    # Subset sim_data and obs_data based on shared dates ------------------

    # Find the shared dates
    obs_data$Date <- as.Date(obs_data$Date) # charc if read through .csv
    sim_data$Date <- as.Date(sim_data$Date)
    shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

    # Check if there are any shared dates
    if (length(shared_dates) == 0) {
      warning("No overlapping dates between simulated and observed data.
            Skipping this site.")
      next # go to next site
    } else {
      # Subset both data frames by shared dates (unique cols for point obs)
      sim_data <- sim_data %>% filter(Date %in% shared_dates)
      obs_data <- obs_data %>% filter(Date %in% shared_dates)
      # These are simple vectors
      q_sim <- sim_data[[colname_sim]]
      q_obs <- obs_data$Q_cfs
    }

    # Initialize objects for analysis -------------------------------------

    # Create a "month" column in the full data
    obs_data <- obs_data %>%
      mutate(month = format(Date, "%m")) # Extract month as a two-digit string
    sim_data <- sim_data %>%
      mutate(month = format(Date, "%m"))

    months <- sprintf("%02d", 1:12) # Ensure consistent month names (01 to 12)
    water_years <- sort(unique(obs_data$waterYear))
    nwateryears <- length(water_years)

    # Ensure initialization of results list for one-off runs
    if (!is.list(list_gof_q)) {
      list_gof_q <- list()
    }
    # Initialize results storage for the site
    if (!is.list(list_gof_q[[usgs_site_id]])) {
      list_gof_q[[usgs_site_id]] <- list(
        total = list(
          nse = numeric(nruns),
          nrmse = numeric(nruns),
          rsq = numeric(nruns),
          pbias = numeric(nruns),
          kge = numeric(nruns)
        ),
        annual = list(
          nse = matrix(NA, nrow = nruns, ncol = nwateryears),
          nrmse = matrix(NA, nrow = nruns, ncol = nwateryears),
          rsq = matrix(NA, nrow = nruns, ncol = nwateryears),
          pbias = matrix(NA, nrow = nruns, ncol = nwateryears),
          kge = matrix(NA, nrow = nruns, ncol = nwateryears)
        ),
        monthly = list(
          nse = matrix(NA, nrow = nruns, ncol = length(months)),
          nrmse = matrix(NA, nrow = nruns, ncol = length(months)),
          rsq = matrix(NA, nrow = nruns, ncol = length(months)),
          pbias = matrix(NA, nrow = nruns, ncol = length(months)),
          kge = matrix(NA, nrow = nruns, ncol = length(months))
        )
      )
    }

    # Calculate statistics ------------------------------------------------

    # Calculate total fit statistics
    list_gof_q[[usgs_site_id]]$total$nse[run] <- hydroGOF::NSE(q_sim, q_obs)
    list_gof_q[[usgs_site_id]]$total$nrmse[run] <- hydroGOF::nrmse(q_sim, q_obs)
    list_gof_q[[usgs_site_id]]$total$rsq[run] <- hydroGOF::R2(q_sim, q_obs)
    list_gof_q[[usgs_site_id]]$total$pbias[run] <- hydroGOF::pbias(q_sim, q_obs)
    list_gof_q[[usgs_site_id]]$total$kge[run] <- hydroGOF::KGE(q_sim, q_obs)

    # Calculate annual fit statistics
    for (wy_idx in seq_along(water_years)) {
      wy <- water_years[wy_idx]

      q_sim_annual <- q_sim[sim_data$waterYear == wy]
      q_obs_annual <- q_obs[obs_data$waterYear == wy]

      if (length(q_sim_annual) > 0 && length(q_obs_annual) > 0) {
        list_gof_q[[usgs_site_id]]$annual$nse[run, wy_idx] <- hydroGOF::NSE(
          q_sim_annual, q_obs_annual
        )
        list_gof_q[[usgs_site_id]]$annual$nrmse[run, wy_idx] <- hydroGOF::nrmse(
          q_sim_annual, q_obs_annual
        )
        list_gof_q[[usgs_site_id]]$annual$rsq[run, wy_idx] <- hydroGOF::R2(
          q_sim_annual, q_obs_annual
        )
        list_gof_q[[usgs_site_id]]$annual$pbias[run, wy_idx] <- hydroGOF::pbias(
          q_sim_annual, q_obs_annual
        )
        list_gof_q[[usgs_site_id]]$annual$kge[run, wy_idx] <- hydroGOF::KGE(
          q_sim_annual, q_obs_annual
        )
      }
    } # close for wy_idx

    # Calculate monthly stats
    for (month_idx in seq_along(months)) {
      month <- months[month_idx]

      q_sim_month <- q_sim[sim_data$month == month]
      q_obs_month <- q_obs[obs_data$month == month]

      if (length(q_sim_month) > 0 && length(q_obs_month) > 0) {
        list_gof_q[[usgs_site_id]]$monthly$nse[
          run, month_idx
        ] <- hydroGOF::NSE(
          q_sim_month, q_obs_month
        )
        list_gof_q[[usgs_site_id]]$monthly$nrmse[
          run, month_idx
        ] <- hydroGOF::nrmse(
          q_sim_month, q_obs_month
        )
        list_gof_q[[usgs_site_id]]$monthly$rsq[
          run, month_idx
        ] <- hydroGOF::R2(
          q_sim_month, q_obs_month
        )
        list_gof_q[[usgs_site_id]]$monthly$pbias[
          run, month_idx
        ] <- hydroGOF::pbias(
          q_sim_month, q_obs_month
        )
        list_gof_q[[usgs_site_id]]$monthly$kge[
          run, month_idx
        ] <- hydroGOF::KGE(
          q_sim_month, q_obs_month
        )
      }
    } # close for month_idx

    # Skip rolling 20-year analysis if the period is insufficient
    if (!perform_rolling) {
      next
    }

    # Calculate rolling 20-year statistics
    for (window_idx in seq_along(rolling_windows)) {
      window <- rolling_windows[[window_idx]]
      start_window <- window[1]
      end_window <- window[2]

      q_sim_rolling <- q_sim[sim_data$waterYear >= start_window &
        sim_data$waterYear <= end_window]
      q_obs_rolling <- q_obs[obs_data$waterYear >= start_window &
        obs_data$waterYear <= end_window]

      if (length(q_sim_rolling) > 0 && length(q_obs_rolling) > 0) {
        if (is.null(
          list_gof_q[[usgs_site_id]]$rolling[[
            paste0(start_window, "-", end_window)
          ]]
        )) {
          list_gof_q[[usgs_site_id]]$rolling[[
            paste0(start_window, "-", end_window)
          ]] <- list(
            nse = numeric(nruns),
            nrmse = numeric(nruns),
            rsq = numeric(nruns),
            pbias = numeric(nruns),
            kge = numeric(nruns)
          )
        }

        list_gof_q[[usgs_site_id]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$nse[run] <- hydroGOF::NSE(
          q_sim_rolling, q_obs_rolling
        )
        list_gof_q[[usgs_site_id]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$nrmse[run] <- hydroGOF::nrmse(
          q_sim_rolling, q_obs_rolling
        )
        list_gof_q[[usgs_site_id]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$rsq[run] <- hydroGOF::R2(
          q_sim_rolling, q_obs_rolling
        )
        list_gof_q[[usgs_site_id]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$pbias[run] <- hydroGOF::pbias(
          q_sim_rolling, q_obs_rolling
        )
        list_gof_q[[usgs_site_id]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$kge[run] <- hydroGOF::KGE(
          q_sim_rolling, q_obs_rolling
        )
      }
    }
  } # site loop

  return(list_gof_q)
}





.op_gof_swe <- function(nrcs_stations,
                        obs, # nrcs DATA LIST
                        sim,
                        start_year,
                        end_year,
                        nruns = 1,
                        run = 1,
                        list_gof_swe = NULL) {
  # Initialize list_gof_swe if it is not provided
  if (is.null(list_gof_swe)) {
    list_gof_swe <- list()
  }

  for (state_id in names(nrcs_stations)) {
    nrcs_site_ids <- nrcs_stations[[state_id]]$site_ids
    hru_ids <- nrcs_stations[[state_id]]$hru_ids

    for (i in seq_along(nrcs_site_ids)) {
      # Defining IDs in the list
      nrcs_site_id <- nrcs_site_ids[i] # i object is used in "swe_obs" below
      hru_id <- hru_ids[i] # same index as the site

      # Defining observed and simulated datasets
      # HARDCODED TO EXTRACT JUST WTEQ FROM THE READ.nrcs OUTPUT
      obs_data <- obs[[state_id]][[i]] %>%
        filter(waterYear %in% seq(start_year, end_year))
      obs_data$Date <- as.Date(obs_data$Date) # ensure the correct date class

      sim_data <- suppressMessages(sim %>%
        filter(waterYear %in% seq(start_year, end_year)) %>%
        select(Date, !!sym(paste0("hru_", hru_id))))

      # Merging the datasets by date
      merged_data <- full_join(obs_data, sim_data, by = "Date")

      # Removes NAs where there are no obs or sim data in the  date range
      merged_data <- merged_data %>% filter(!is.na(WTEQ) &
        !is.na(!!sym(
          paste0("hru_", hru_id)
        )))

      swe_obs <- merged_data$WTEQ
      swe_sim <- merged_data[[paste0("hru_", hru_id)]]

      # Filter to periods where sim or obs has non-zero values
      valid_indices <- (swe_obs > 0) | (swe_sim > 0)
      swe_obs <- swe_obs[valid_indices]
      swe_sim <- swe_sim[valid_indices]

      # Objective functions from hydrogof
      swe_nse_value <- hydroGOF::NSE(swe_sim, swe_obs)
      swe_nrmse_value <- hydroGOF::nrmse(swe_sim, swe_obs)
      swe_rsq_value <- hydroGOF::R2(swe_sim, swe_obs)
      swe_pbias_value <- hydroGOF::pbias(swe_sim, swe_obs)
      swe_kge_value <- hydroGOF::KGE(swe_sim, swe_obs)
      swe_kgenp_value <- hydroGOF::KGEnp(swe_sim, swe_obs)

      # Ensure initialization of results list for one-off runs
      if (!is.list(list_gof_swe[[state_id]])) {
        list_gof_swe[[state_id]] <- list()
      }
      if (!is.list(list_gof_swe[[state_id]][[paste0(nrcs_site_id)]])) {
        list_gof_swe[[state_id]][[paste0(nrcs_site_id)]] <- list(
          nse = numeric(nruns),
          nrmse = numeric(nruns),
          rsq = numeric(nruns),
          pbias = numeric(nruns),
          kge = numeric(nruns),
          kgenp = numeric(nruns)
        )
      }

      # Store the results
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nse[run] <- swe_nse_value
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nrmse[run] <- swe_nrmse_value
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$rsq[run] <- swe_rsq_value
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$pbias[run] <- swe_pbias_value
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$kge[run] <- swe_kge_value
      list_gof_swe[[state_id]][[
        paste0(nrcs_site_id)
      ]]$kgenp[run] <- swe_kgenp_value
    } # site loop
  } # state loop

  return(list_gof_swe)
} # close fn





.op_gof_aso <- function(obs, # aso_data_df from pws.read.aso
                        sim, # from analyze.outputs
                        nhru_area_weights,
                        start_year,
                        end_year,
                        nruns = 1,
                        run = 1,
                        list_gof_aso = NULL) {
  # Initialize list_gof_aso if it is not provided
  if (is.null(list_gof_aso)) {
    list_gof_aso <- list()
  }

  # Checks on sim and obs data ----------------------------------------------
  # Trim simulated data frame to selected start and end date
  sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
  obs_data <- obs %>% filter(waterYear %in% seq(start_year, end_year))

  # Check for missing values in all relevant columns (sim_data)
  # Different between point obs and spatially distributed obs
  num_na <- suppressMessages(
    sum(rowSums(is.na(sim_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  simulation during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    sim_data <- sim_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }

  # Check for missing values in all relevant columns (obs_data)
  num_na <- suppressMessages(
    sum(rowSums(is.na(obs_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  observations during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    obs_data <- obs_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }


  # Check for data presence within the analysis period
  if (nrow(obs_data) == 0) {
    warning(paste("No observations for aso within the analysis period
               (", start_year, "-", end_year, "). Aborting .op_gof_.asov2"))
    return(invisible(NULL)) # quitely exit this function
  }

  # Check if only part of the analysis period has data
  sim_years <- unique(sim_data$waterYear)
  if (min(sim_years) > start_year || max(sim_years) < end_year) {
    warning(paste("Simulation for pkwater_equiv only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
  }

  # Check if only part of the analysis period has data
  obs_years <- unique(obs_data$waterYear)
  if (min(obs_years) > start_year || max(obs_years) < end_year) {
    warning(paste("Observations for aso only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  Calculations will be based on this available range."))
  }

  # Add area-weighted averages of the HRU based sim and obs -----------------

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim_data))
  hru_cols_obs <- grep("^hru_", names(obs_data))

  # Apply row-wise weighted average for `sim`
  sim_data$A_weighted_avg <- apply(
    sim_data[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Apply row-wise weighted average for `obs`
  obs_data$A_weighted_avg <- apply(
    obs_data[, hru_cols_obs], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Optionally relocate the new column (adjust column order)
  sim_data <- sim_data %>% relocate(A_weighted_avg, .before = hru_1)
  obs_data <- obs_data %>% relocate(A_weighted_avg, .before = hru_1)

  # Subset sim_data and obs_data based on shared dates ----------------------

  # Find the shared dates
  obs_data$Date <- as.Date(obs_data$Date) # sometimes charc if read through .csv
  sim_data$Date <- as.Date(sim_data$Date)
  shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

  # Check if there are any shared dates
  if (length(shared_dates) == 0) {
    warning("No overlapping dates between simulated and observed data.
            Aborting .op_gof_.asov2")
    return(invisible(NULL)) # quitely exit this function
  } else {
    # Subset both data frames by shared dates
    aso_sim <- sim_data[sim_data$Date %in% shared_dates, ] # all columns
    aso_obs <- obs_data[obs_data$Date %in% shared_dates, ]

    aso_sim_matrix <- aso_sim %>%
      ungroup() %>%
      select(starts_with("hru_"))

    aso_obs_matrix <- aso_obs %>%
      ungroup() %>%
      select(starts_with("hru_"))

    err_abs <- abs(aso_sim_matrix - aso_obs_matrix)
    err_sq <- (aso_sim_matrix - aso_obs_matrix)^2
  }

  # Initialize objects for analysis ------------------------------------------

  # Find dimensions for this analysis
  nhru <- length(nhru_area_weights)
  nflights <- nrow(obs_data) # MIGHT NEED TO FIND A WAY TO NAME THE FLIGHTS

  # Initialize list elements for gof metrics if the first one is not there
  if (!is.numeric(list_gof_aso[["MAE_bybasin"]])) {
    list_gof_aso <- list(
      MAE_bybasin = rep(NA, nruns), # scalar per model run
      MAE_byhru = matrix(NA, nrow = nruns, ncol = nhru), # 10 hru stats per run
      MAE_byflight_AW = matrix(NA, nrow = nruns, ncol = nflights),
      nrmse_byflight_AW = matrix(NA, nrow = nruns, ncol = nflights),
      kge = rep(NA, nruns),
      R2 = rep(NA, nruns)
    )
  }

  # Calculate statistics --------------------------------------------------

  # MAE by basin (pre-area weighted average depth)
  aso_MAE_bybasin_val <- hydroGOF::mae(
    sim = aso_sim$A_weighted_avg,
    obs = aso_obs$A_weighted_avg
  )

  # Takes MAE by column (vectorized operation, faster than loop)
  aso_MAE_byhru_vect <- hydroGOF::mae(
    sim = aso_sim_matrix,
    obs = aso_obs_matrix
  )

  # Vectorized operation
  aso_MAE_byflight_AW_vect <- rowSums(err_abs * matrix(nhru_area_weights,
    nrow = nrow(err_abs),
    ncol = length(err_abs),
    byrow = TRUE
  )) / sum(nhru_area_weights)
  # AREA WEIGHTED nrmse
  flight_sd <- apply(aso_obs_matrix, 1, sd)

  aso_rmse_byflight_AW_vect <- sqrt(rowSums(err_sq * matrix(nhru_area_weights,
    nrow = nrow(err_sq),
    ncol = length(err_sq),
    byrow = TRUE
  ) / sum(nhru_area_weights)))
  # nrmse in percentage like hydroGOF::nrmse
  aso_nrmse_byflight_AW_vect <- (aso_rmse_byflight_AW_vect / flight_sd) * 100

  # gof stats over the entire dataset
  # This is due to the limitations of only have a few timesteps
  aso_sim_vect <- aso_sim_matrix %>%
    as.matrix() %>%
    as.vector()

  aso_obs_vect <- aso_obs_matrix %>%
    as.matrix() %>%
    as.vector()

  aso_kge_val <- hydroGOF::KGE(
    sim = aso_sim_vect,
    obs = aso_obs_vect
  )
  aso_R2_val <- hydroGOF::R2(
    sim = aso_sim_vect,
    obs = aso_obs_vect
  )

  # Append to list and write -----------------------------------------------
  list_gof_aso$MAE_bybasin[run] <- aso_MAE_bybasin_val
  list_gof_aso$MAE_byhru[run, ] <- aso_MAE_byhru_vect
  list_gof_aso$MAE_byflight_AW[run, ] <- aso_MAE_byflight_AW_vect
  list_gof_aso$nrmse_byflight_AW[run, ] <- aso_nrmse_byflight_AW_vect
  list_gof_aso$kge[run] <- aso_kge_val
  list_gof_aso$R2[run] <- aso_R2_val

  return(list_gof_aso)
} # close fn





.op_gof_mod10a1 <- function(obs, # mod10a1_data_df from pws.read.mod10a1
                            sim, # from analyze.outputs
                            nhru_area_weights,
                            start_year,
                            end_year,
                            nruns = 1,
                            run = 1,
                            list_gof_mod10a1 = NULL) {
  # Initialize list_gof_mod10a1 if it is not provided
  if (is.null(list_gof_mod10a1)) {
    list_gof_mod10a1 <- list()
  }

  # Checks on sim and obs data ----------------------------------------------

  # Trim simulated data frame to selected start and end date
  sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
  obs_data <- obs %>% filter(waterYear %in% seq(start_year, end_year))

  # Check for missing values in all relevant columns (sim_data)
  # Different between point obs and spatially distributed obs
  num_na <- suppressMessages(
    sum(rowSums(is.na(sim_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  simulation during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    sim_data <- sim_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }

  # Check for missing values in all relevant columns (obs_data)
  num_na <- suppressMessages(
    sum(rowSums(is.na(obs_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  observations during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    obs_data <- obs_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }

  # Check for obs data presence within the analysis period
  if (nrow(obs_data) == 0) {
    warning(paste("No observations for mod10a1 within the analysis period
               (", start_year, "-", end_year, "). Aborting .op_gof_.mod10a1"))
    return(invisible(NULL)) # quitely exit this function
  }

  # Check if only part of the analysis period has data
  sim_years <- unique(sim_data$waterYear)
  if (min(sim_years) > start_year || max(sim_years) < end_year) {
    warning(paste("Simulation for snowcov_area only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
  }

  # Check if only part of the analysis period has data
  obs_years <- unique(obs_data$waterYear)
  if (min(obs_years) > start_year || max(obs_years) < end_year) {
    warning(paste("Observations for mod10a1 only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  Calculations will be based on this available range."))
  }

  # Add area-weighted averages of the HRU based sim and obs -----------------

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim_data))
  hru_cols_obs <- grep("^hru_", names(obs_data))

  # Apply row-wise weighted average for `sim`
  sim_data$A_weighted_avg <- apply(
    sim_data[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Apply row-wise weighted average for `obs`
  obs_data$A_weighted_avg <- apply(
    obs_data[, hru_cols_obs], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Optionally relocate the new column (adjust column order)
  sim_data <- sim_data %>% relocate(A_weighted_avg, .before = hru_1)
  obs_data <- obs_data %>% relocate(A_weighted_avg, .before = hru_1)

  # Subset sim_data and obs_data based on shared dates ----------------------
  # Has a unqiue component of filtering out values where all are zeros

  # Find the shared dates
  obs_data$Date <- as.Date(obs_data$Date) # sometimes charc if read through .csv
  sim_data$Date <- as.Date(sim_data$Date)
  shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

  # Check if there are any shared dates
  if (length(shared_dates) == 0) {
    warning("No overlapping dates between simulated and observed data.
            Aborting .op_gof_.mod10a1")
    return(invisible(NULL)) # quitely exit this function
  } else {
    # Subset both data frames by shared dates
    mod10a1_sim <- sim_data[sim_data$Date %in% shared_dates, ] # all columns
    mod10a1_obs <- obs_data[obs_data$Date %in% shared_dates, ]

    # Date related work is done, subset the matrices and
    mod10a1_sim_matrix <- mod10a1_sim %>%
      ungroup() %>%
      select(starts_with("hru_"))
    mod10a1_sim_matrix <- as.data.frame(mod10a1_sim_matrix) # different class

    mod10a1_obs_matrix <- mod10a1_obs %>%
      ungroup() %>%
      select(starts_with("hru_"))

    # UNIQUE TO mod10a1
    # Identify non-zero rows in the observed matrix (returns logical)
    # Subset these in the data
    # THIS EXCLUDES ROWS WHERE OBS = 0, I.E. SUMMER MONTHS OR ERRORS
    # Step 1: Filter out non-zero rows
    non_zero_rows <- rowSums(mod10a1_obs_matrix != 0) > 0

    mod10a1_sim <- mod10a1_sim[non_zero_rows, ]
    mod10a1_obs <- mod10a1_obs[non_zero_rows, ]

    mod10a1_sim_matrix <- mod10a1_sim_matrix[non_zero_rows, ]
    mod10a1_obs_matrix <- mod10a1_obs_matrix[non_zero_rows, ]

    # STEPS 2 and 3 WERE NOT IN THE PREVIOUS VERSION
    # Step 2: Cap values greater than 1 to 1 in matrices
    mod10a1_sim_matrix[mod10a1_sim_matrix > 1] <- 1
    mod10a1_obs_matrix[mod10a1_obs_matrix > 1] <- 1

    # Step 3: Filter rows where all values are <= 0.1
    valid_rows <- rowSums(mod10a1_obs_matrix > 0.1) > 0

    mod10a1_sim <- mod10a1_sim[valid_rows, ]
    mod10a1_obs <- mod10a1_obs[valid_rows, ]

    mod10a1_sim_matrix <- mod10a1_sim_matrix[valid_rows, ]
    mod10a1_obs_matrix <- mod10a1_obs_matrix[valid_rows, ]

    # Calculate absolute error
    err_abs <- abs(mod10a1_sim_matrix - mod10a1_obs_matrix)
  }

  # Initialize objects for analysis ------------------------------------------

  # Find dimensions for this analysis
  nhru <- length(nhru_area_weights)

  # Initialize list elements for gof metrics if the first one is not there
  if (!is.numeric(list_gof_mod10a1[["nrmse_bybasin"]])) {
    list_gof_mod10a1 <- list(
      nrmse_bybasin = rep(NA, nruns), # scalar per model run
      nrmse_byhru = matrix(NA, nrow = nruns, ncol = nhru), # nhru stats per run
      kge_bybasin = rep(NA, nruns),
      kge_byhru = matrix(NA, nrow = nruns, ncol = nhru)
    )
  }

  # Calculate statistics --------------------------------------------------

  # By BASIN
  # Important to do for basin average instead of whole matrix
  # so errors are weighted by area
  mod10a1_nrmse_bybasin_val <- hydroGOF::nrmse(
    sim = mod10a1_sim$A_weighted_avg,
    obs = mod10a1_obs$A_weighted_avg
  )
  mod10a1_kge_bybasin_val <- hydroGOF::KGE(
    sim = mod10a1_sim$A_weighted_avg,
    obs = mod10a1_obs$A_weighted_avg
  )

  # BY HRU (performs operation column-wise)
  mod10a1_nrmse_byhru_vect <- hydroGOF::nrmse(
    sim = mod10a1_sim_matrix,
    obs = mod10a1_obs_matrix
  )
  mod10a1_kge_byhru_vect <- hydroGOF::KGE(
    sim = mod10a1_sim_matrix,
    obs = mod10a1_obs_matrix
  )

  # Append to list and write -----------------------------------------------

  list_gof_mod10a1$nrmse_bybasin[run] <- mod10a1_nrmse_bybasin_val
  list_gof_mod10a1$kge_bybasin[run] <- mod10a1_kge_bybasin_val
  list_gof_mod10a1$nrmse_byhru[run, ] <- mod10a1_nrmse_byhru_vect
  list_gof_mod10a1$kge_byhru[run, ] <- mod10a1_kge_byhru_vect

  return(list_gof_mod10a1)
} # close fn





.op_gof_sms2 <- function(nrcs_stations,
                         obs, # snotel DATA LIST
                         sim,
                         start_year,
                         end_year,
                         nruns = 1,
                         run = 1,
                         list_gof_sms2 = NULL) {
  # Initialize list_gof_sms2 if it is not provided
  if (is.null(list_gof_sms2)) {
    list_gof_sms2 <- list()
  }

  # Loop through states and stations
  for (state_id in names(nrcs_stations)) {
    nrcs_site_ids <- nrcs_stations[[state_id]]$site_ids
    hru_ids <- nrcs_stations[[state_id]]$hru_ids

    for (i in seq_along(nrcs_site_ids)) {
      # Defining IDs in the list
      nrcs_site_id <- nrcs_site_ids[i] # i object is used in "sms2_obs" below
      hru_id <- hru_ids[i] # same index as the site

      # Checks on sim and obs data ------------------------------------------

      # Trim simulated data frame to selected start and end date
      # Different when obs is _data_list rather than _data_df
      # Need to assign to new variable in looped structure
      sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
      obs_data <- obs[[state_id]][[i]] %>%
        filter(waterYear %in% seq(start_year, end_year))

      # Check for missing values
      num_na <- sum(is.na(obs_data$sms2) | is.na(sim_data[[
        paste0("hru_", hru_id)
      ]]))
      if (num_na > 0) {
        warning(paste(
          "There are", num_na, "NA values for nrcs site ID",
          nrcs_site_id, "during the analysis period. Removing NAs"
        ))
        # Remove lines with missing values in the intended columns
        colname_sim <- paste0("hru_", hru_id) # Dynamically create the col name
        sim_data <- sim_data[!is.na(sim_data[[colname_sim]]), ]
        obs_data <- obs_data[!is.na(obs_data$sms2), ] # static colname
      }

      # Check for data presence within the analysis period
      if (nrow(obs_data) == 0) {
        warning(paste("No observations for sms2 within the analysis period
               (", start_year, "-", end_year, "). "))
        next # Go to next site (instead of aborting)
      }

      # Check if only part of the analysis period has data
      sim_years <- unique(sim_data$waterYear)
      if (min(sim_years) > start_year || max(sim_years) < end_year) {
        warning(paste("Simulation for soil_lower_ratio only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
      }

      # Check if only part of the analysis period has data
      obs_years <- unique(obs_data$waterYear)
      if (min(obs_years) > start_year || max(obs_years) < end_year) {
        warning(paste(
          "Observations for sms2 only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  For nrcs site ID", nrcs_site_id,
          ", calculations will be based on this available range."
        ))
      }

      # UNIQUE TO THE sms WORKFLOW - Transform VWC to wetness (saturation)
      # VWC to sat ----------------------------------------------------------

      # Data cleaning for sms, remove anything with values greater than 50%
      # There are some odd values on the order of 10^5
      # There are also a few values around 0.7 in sms20
      # Could filter out outliers after filtering by 100
      obs_data <- obs_data[!(obs_data$sms2 > 50), ] # only works when NAs gone

      # Equation 7.8 in Dingman,
      # Assume porosity is the maximum observed VWC (around 0.4)
      # Assumes that the soil has been saturated at some point in time

      # sat = VWC/porosity (normalized by max, ranges 0 to 1)
      # This is the same as the smap normalization, min is 0 for snotel obs
      porosity <- max(obs_data$sms2)
      obs_data$sms2 <- obs_data$sms2 / porosity

      # NORMALIZE SIMULATED DATA
      # As called out in Hay, 2023 "parameterization of..."
      sim_data <- sim_data %>%
        mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

      # Subset sim_data and obs_data based on shared dates ------------------

      # Find the shared dates
      obs_data$Date <- as.Date(obs_data$Date) # charc if read through .csv
      sim_data$Date <- as.Date(sim_data$Date)
      shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

      # Check if there are any shared dates
      if (length(shared_dates) == 0) {
        warning(paste0(
          "No overlapping dates in simulated and observed data at station:",
          nrcs_site_id
        ))
        next # skip to next station
      } else {
        sms2_sim <- sim_data %>%
          filter(Date %in% shared_dates) %>%
          pull(!!sym(paste0("hru_", hru_id)))

        sms2_obs <- obs_data %>%
          filter(Date %in% shared_dates) %>%
          pull(sms2)
      }

      # Initialize objects for analysis -------------------------------------

      # Ensure initialization of results list for one-off runs
      if (!is.list(list_gof_sms2[[state_id]])) {
        list_gof_sms2[[state_id]] <- list()
      }
      if (!is.list(list_gof_sms2[[state_id]][[paste0(nrcs_site_id)]])) {
        list_gof_sms2[[state_id]][[paste0(nrcs_site_id)]] <- list(
          nse = numeric(nruns),
          nrmse = numeric(nruns),
          rsq = numeric(nruns),
          pbias = numeric(nruns),
          kge = numeric(nruns)
        )
      }

      # Calculate statistics ------------------------------------------------

      # Objective functions from hydrogof
      sms2_nse_value <- hydroGOF::NSE(sms2_sim, sms2_obs)
      sms2_nrmse_value <- hydroGOF::nrmse(sms2_sim, sms2_obs)
      sms2_rsq_value <- hydroGOF::R2(sms2_sim, sms2_obs)
      sms2_pbias_value <- hydroGOF::pbias(sms2_sim, sms2_obs)
      sms2_kge_value <- hydroGOF::KGE(sms2_sim, sms2_obs)

      # Append to list ------------------------------------------------------

      # Store the results
      list_gof_sms2[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nse[run] <- sms2_nse_value
      list_gof_sms2[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nrmse[run] <- sms2_nrmse_value
      list_gof_sms2[[state_id]][[
        paste0(nrcs_site_id)
      ]]$rsq[run] <- sms2_rsq_value
      list_gof_sms2[[state_id]][[
        paste0(nrcs_site_id)
      ]]$pbias[run] <- sms2_pbias_value
      list_gof_sms2[[state_id]][[
        paste0(nrcs_site_id)
      ]]$kge[run] <- sms2_kge_value
    } # site loop
  } # state loop
  return(list_gof_sms2)
} # close fn






.op_gof_sms8 <- function(nrcs_stations,
                         obs, # snotel DATA LIST
                         sim,
                         start_year,
                         end_year,
                         nruns = 1,
                         run = 1,
                         list_gof_sms8 = NULL) {
  # Initialize list_gof_sms8 if it is not provided
  if (is.null(list_gof_sms8)) {
    list_gof_sms8 <- list()
  }

  # Loop through states and stations
  for (state_id in names(nrcs_stations)) {
    nrcs_site_ids <- nrcs_stations[[state_id]]$site_ids
    hru_ids <- nrcs_stations[[state_id]]$hru_ids

    for (i in seq_along(nrcs_site_ids)) {
      # Defining IDs in the list
      nrcs_site_id <- nrcs_site_ids[i] # i object is used in "sms8_obs" below
      hru_id <- hru_ids[i] # same index as the site

      # Checks on sim and obs data ------------------------------------------

      # Trim simulated data frame to selected start and end date
      # Different when obs is _data_list rather than _data_df
      # Need to assign to new variable in looped structure
      sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
      obs_data <- obs[[state_id]][[i]] %>%
        filter(waterYear %in% seq(start_year, end_year))

      # Check for missing values
      num_na <- sum(is.na(obs_data$sms8) | is.na(sim_data[[
        paste0("hru_", hru_id)
      ]]))
      if (num_na > 0) {
        warning(paste(
          "There are", num_na, "NA values for nrcs site ID",
          nrcs_site_id, "during the analysis period. Removing NAs"
        ))
        # Remove lines with missing values in the intended columns
        colname_sim <- paste0("hru_", hru_id) # Dynamically create the col name
        sim_data <- sim_data[!is.na(sim_data[[colname_sim]]), ]
        obs_data <- obs_data[!is.na(obs_data$sms8), ] # static colname
      }

      # Check for data presence within the analysis period
      if (nrow(obs_data) == 0) {
        warning(paste("No observations for sms8 within the analysis period
               (", start_year, "-", end_year, "). "))
        next # Go to next site (instead of aborting)
      }

      # Check if only part of the analysis period has data
      sim_years <- unique(sim_data$waterYear)
      if (min(sim_years) > start_year || max(sim_years) < end_year) {
        warning(paste("Simulation for soil_lower_ratio only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
      }

      # Check if only part of the analysis period has data
      obs_years <- unique(obs_data$waterYear)
      if (min(obs_years) > start_year || max(obs_years) < end_year) {
        warning(paste(
          "Observations for sms8 only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  For nrcs site ID", nrcs_site_id,
          ", calculations will be based on this available range."
        ))
      }

      # UNIQUE TO THE sms WORKFLOW - Transform VWC to wetness (saturation)
      # VWC to sat ----------------------------------------------------------

      # Data cleaning for sms, remove anything with values greater than 50%
      # There are some odd values on the order of 10^5
      # There are also a few values around 0.7 in sms80
      # Could filter out outliers after filtering by 100
      obs_data <- obs_data[!(obs_data$sms8 > 50), ] # only works when NAs gone

      # Equation 7.8 in Dingman,
      # Assume porosity is the maximum observed VWC (around 0.4)
      # Assumes that the soil has been saturated at some point in time

      # sat = VWC/porosity (normalized by max, ranges 0 to 1)
      porosity <- max(obs_data$sms8)
      obs_data$sms8 <- obs_data$sms8 / porosity

      # NORMALIZE SIMULATED DATA
      # As called out in Hay, 2023 "parameterization of..."
      sim_data <- sim_data %>%
        mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

      # Subset sim_data and obs_data based on shared dates ------------------

      # Find the shared dates
      obs_data$Date <- as.Date(obs_data$Date) # charc if read through .csv
      sim_data$Date <- as.Date(sim_data$Date)
      shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

      # Check if there are any shared dates
      if (length(shared_dates) == 0) {
        warning(paste0(
          "No overlapping dates in simulated and observed data at station:",
          nrcs_site_id
        ))
        next # skip to next station
      } else {
        # Subset both data frames by shared dates (unique cols for point obs)
        sms8_sim <- sim_data %>%
          filter(Date %in% shared_dates) %>%
          pull(!!sym(paste0("hru_", hru_id)))

        sms8_obs <- obs_data %>%
          filter(Date %in% shared_dates) %>%
          pull(sms8)
      }

      # Initialize objects for analysis -------------------------------------

      # Ensure initialization of results list for one-off runs
      if (!is.list(list_gof_sms8[[state_id]])) {
        list_gof_sms8[[state_id]] <- list()
      }
      if (!is.list(list_gof_sms8[[state_id]][[paste0(nrcs_site_id)]])) {
        list_gof_sms8[[state_id]][[paste0(nrcs_site_id)]] <- list(
          nse = numeric(nruns),
          nrmse = numeric(nruns),
          rsq = numeric(nruns),
          pbias = numeric(nruns),
          kge = numeric(nruns)
        )
      }

      # Calculate statistics ------------------------------------------------

      # Objective functions from hydrogof
      sms8_nse_value <- hydroGOF::NSE(sms8_sim, sms8_obs)
      sms8_nrmse_value <- hydroGOF::nrmse(sms8_sim, sms8_obs)
      sms8_rsq_value <- hydroGOF::R2(sms8_sim, sms8_obs)
      sms8_pbias_value <- hydroGOF::pbias(sms8_sim, sms8_obs)
      sms8_kge_value <- hydroGOF::KGE(sms8_sim, sms8_obs)

      # Append to list ------------------------------------------------------

      # Store the results
      list_gof_sms8[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nse[run] <- sms8_nse_value
      list_gof_sms8[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nrmse[run] <- sms8_nrmse_value
      list_gof_sms8[[state_id]][[
        paste0(nrcs_site_id)
      ]]$rsq[run] <- sms8_rsq_value
      list_gof_sms8[[state_id]][[
        paste0(nrcs_site_id)
      ]]$pbias[run] <- sms8_pbias_value
      list_gof_sms8[[state_id]][[
        paste0(nrcs_site_id)
      ]]$kge[run] <- sms8_kge_value
    } # site loop
  } # state loop
  return(list_gof_sms8)
} # close fn






.op_gof_sms20 <- function(nrcs_stations,
                          obs, # snotel DATA LIST
                          sim,
                          start_year,
                          end_year,
                          nruns = 1,
                          run = 1,
                          list_gof_sms20 = NULL) {
  # Initialize list_gof_sms20 if it is not provided
  if (is.null(list_gof_sms20)) {
    list_gof_sms20 <- list()
  }

  # Loop through states and stations
  for (state_id in names(nrcs_stations)) {
    nrcs_site_ids <- nrcs_stations[[state_id]]$site_ids
    hru_ids <- nrcs_stations[[state_id]]$hru_ids

    for (i in seq_along(nrcs_site_ids)) {
      # Defining IDs in the list
      nrcs_site_id <- nrcs_site_ids[i] # i object is used in "sms20_obs" below
      hru_id <- hru_ids[i] # same index as the site

      # Checks on sim and obs data ------------------------------------------

      # Trim simulated data frame to selected start and end date
      # Different when obs is _data_list rather than _data_df
      # Need to assign to new variable in looped structure
      sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
      obs_data <- obs[[state_id]][[i]] %>%
        filter(waterYear %in% seq(start_year, end_year))

      # Check for missing values
      num_na <- sum(is.na(obs_data$sms20) | is.na(sim_data[[
        paste0("hru_", hru_id)
      ]]))
      if (num_na > 0) {
        warning(paste(
          "There are", num_na, "NA values for nrcs site ID",
          nrcs_site_id, "during the analysis period. Removing NAs"
        ))
        # Remove lines with missing values in the intended columns
        colname_sim <- paste0("hru_", hru_id) # Dynamically create the col name
        sim_data <- sim_data[!is.na(sim_data[[colname_sim]]), ]
        obs_data <- obs_data[!is.na(obs_data$sms20), ] # static colname
      }

      # Check for data presence within the analysis period
      if (nrow(obs_data) == 0) {
        warning(paste("No observations for sms20 within the analysis period
               (", start_year, "-", end_year, "). "))
        next # Go to next site (instead of aborting)
      }

      # Check if only part of the analysis period has data
      sim_years <- unique(sim_data$waterYear)
      if (min(sim_years) > start_year || max(sim_years) < end_year) {
        warning(paste("Simulation for soil_lower_ratio only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
      }

      # Check if only part of the analysis period has data
      obs_years <- unique(obs_data$waterYear)
      if (min(obs_years) > start_year || max(obs_years) < end_year) {
        warning(paste(
          "Observations for sms20 only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  For nrcs site ID", nrcs_site_id,
          ", calculations will be based on this available range."
        ))
      }

      # UNIQUE TO THE sms WORKFLOW - Transform VWC to wetness (saturation)
      # VWC to sat ----------------------------------------------------------

      # Data cleaning for sms, remove anything with values greater than 50%
      # There are some odd values on the order of 10^5
      # There are also a few values around 0.7 in sms20
      # Could filter out outliers after filtering by 100
      obs_data <- obs_data[!(obs_data$sms20 > 50), ] # only works when NAs gone


      # Equation 7.8 in Dingman,
      # Assume porosity is the maximum observed VWC (around 0.4)
      # Assumes that the soil has been saturated at some point in time

      # sat = VWC/porosity (normalized by max, ranges 0 to 1)
      porosity <- max(obs_data$sms20)
      obs_data$sms20 <- obs_data$sms20 / porosity

      # NORMALIZE SIMULATED DATA
      # As called out in Hay, 2023 "parameterization of..."
      sim_data <- sim_data %>%
        mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

      # Subset sim_data and obs_data based on shared dates ------------------

      # Find the shared dates
      obs_data$Date <- as.Date(obs_data$Date) # charc if read through .csv
      sim_data$Date <- as.Date(sim_data$Date)
      shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

      # Check if there are any shared dates
      if (length(shared_dates) == 0) {
        warning(paste0(
          "No overlapping dates between simulated in observed data at station:",
          nrcs_site_id
        ))
        next # skip to next station
      } else {
        # Subset both data frames by shared dates (unique cols for point obs)
        sms20_sim <- sim_data %>%
          filter(Date %in% shared_dates) %>%
          pull(!!sym(paste0("hru_", hru_id)))

        sms20_obs <- obs_data %>%
          filter(Date %in% shared_dates) %>%
          pull(sms20)
      }

      # Initialize objects for analysis -------------------------------------

      # Ensure initialization of results list for one-off runs
      if (!is.list(list_gof_sms20[[state_id]])) {
        list_gof_sms20[[state_id]] <- list()
      }
      if (!is.list(list_gof_sms20[[state_id]][[paste0(nrcs_site_id)]])) {
        list_gof_sms20[[state_id]][[paste0(nrcs_site_id)]] <- list(
          nse = numeric(nruns),
          nrmse = numeric(nruns),
          rsq = numeric(nruns),
          pbias = numeric(nruns),
          kge = numeric(nruns)
        )
      }

      # Calculate statistics ------------------------------------------------

      # Objective functions from hydrogof
      sms20_nse_value <- hydroGOF::NSE(sms20_sim, sms20_obs)
      sms20_nrmse_value <- hydroGOF::nrmse(sms20_sim, sms20_obs)
      sms20_rsq_value <- hydroGOF::R2(sms20_sim, sms20_obs)
      sms20_pbias_value <- hydroGOF::pbias(sms20_sim, sms20_obs)
      sms20_kge_value <- hydroGOF::KGE(sms20_sim, sms20_obs)

      # Append to list ------------------------------------------------------

      # Store the results
      list_gof_sms20[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nse[run] <- sms20_nse_value
      list_gof_sms20[[state_id]][[
        paste0(nrcs_site_id)
      ]]$nrmse[run] <- sms20_nrmse_value
      list_gof_sms20[[state_id]][[
        paste0(nrcs_site_id)
      ]]$rsq[run] <- sms20_rsq_value
      list_gof_sms20[[state_id]][[
        paste0(nrcs_site_id)
      ]]$pbias[run] <- sms20_pbias_value
      list_gof_sms20[[state_id]][[
        paste0(nrcs_site_id)
      ]]$kge[run] <- sms20_kge_value
    } # site loop
  } # state loop
  return(list_gof_sms20)
} # close fn





# Smap surface wetness (0-5cm depth)
.op_gof_smapsfwt <- function(obs, # smapsfwt_data_df from pws.read.smapsfwt
                             sim, # from analyze.outputs
                             nhru_area_weights,
                             start_year,
                             end_year,
                             nruns = 1,
                             run = 1,
                             list_gof_smapsfwt = NULL) {
  # Initialize list_gof_smapsfwt if it is not provided
  if (is.null(list_gof_smapsfwt)) {
    list_gof_smapsfwt <- list()
  }

  # Checks on sim and obs data ----------------------------------------------

  # Trim simulated data frame to selected start and end date
  sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
  obs_data <- obs %>% filter(waterYear %in% seq(start_year, end_year))


  # Check for missing values in all relevant columns (sim_data)
  # Different between point obs and spatially distributed obs
  num_na <- suppressMessages(
    sum(rowSums(is.na(sim_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  simulation during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    sim_data <- sim_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }


  # Check for missing values in all relevant columns (obs_data)
  num_na <- suppressMessages(
    sum(rowSums(is.na(obs_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  observations during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    obs_data <- obs_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }


  # Check for data presence within the analysis period
  if (nrow(obs_data) == 0) {
    warning(paste("No observations for smapsfwt within the analysis period
               (", start_year, "-", end_year, "). Aborting .op_gof_.smapsfwt"))
    return(invisible(NULL)) # quitely exit this function
  }


  # Check if only part of the analysis period has data
  sim_years <- unique(sim_data$waterYear)
  if (min(sim_years) > start_year || max(sim_years) < end_year) {
    warning(paste("Simulation for soil_lower_ratio only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
  }


  # Check if only part of the analysis period has data
  obs_years <- unique(obs_data$waterYear)
  if (min(obs_years) > start_year || max(obs_years) < end_year) {
    warning(paste("Observations for sms2 only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  Calculations will be based on this available range."))
  }


  # UNIQUE TO THE smap WORKFLOW - NORMALIZE
  #  -------------------------------------------------------------------------

  # As called out in Hay, 2023 "parameterization of..."
  # soil_rechr is the preferred output
  # NORMALIZE OBSERVED DATA (see diagnostic_SM.R for more details)
  obs_data <- obs_data %>%
    mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

  # NORMALIZE SIMULATED DATA
  sim_data <- sim_data %>%
    mutate(across(
      starts_with("hru_"),
      ~ ifelse(max(.) == min(.), ., # returns the constant values
        (. - min(.)) / (max(.) - min(.))
      )
    )) # normalize

  # Add area-weighted averages of the HRU based sim and obs -----------------

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim_data))
  hru_cols_obs <- grep("^hru_", names(obs_data))

  # Apply row-wise weighted average for `sim`
  sim_data$A_weighted_avg <- apply(
    sim_data[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Apply row-wise weighted average for `obs`
  obs_data$A_weighted_avg <- apply(
    obs_data[, hru_cols_obs], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Optionally relocate the new column (adjust column order)
  sim_data <- sim_data %>% relocate(A_weighted_avg, .before = hru_1)
  obs_data <- obs_data %>% relocate(A_weighted_avg, .before = hru_1)

  # Subset sim_data and obs_data based on shared dates ----------------------

  # Find the shared dates
  obs_data$Date <- as.Date(obs_data$Date) # sometimes charc if read through .csv
  sim_data$Date <- as.Date(sim_data$Date)
  shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

  # Check if there are any shared dates
  if (length(shared_dates) == 0) {
    warning("No overlapping dates between simulated and observed data.
            Aborting .op_gof_.smapsfwt")
    return(invisible(NULL)) # quitely exit this function
  } else {
    # Subset both data frames by shared dates
    smapsfwt_sim <- sim_data[sim_data$Date %in% shared_dates, ] # all columns
    smapsfwt_obs <- obs_data[obs_data$Date %in% shared_dates, ]

    # Date related work is done, subset the matrices and
    smapsfwt_sim_matrix <- smapsfwt_sim %>%
      ungroup() %>%
      select(starts_with("hru_"))
    smapsfwt_sim_matrix <- as.data.frame(smapsfwt_sim_matrix) # different class

    smapsfwt_obs_matrix <- smapsfwt_obs %>%
      ungroup() %>%
      select(starts_with("hru_"))

    err_abs <- abs(smapsfwt_sim_matrix - smapsfwt_obs_matrix)
  }

  # Initialize objects for analysis ------------------------------------------

  # Find dimensions for this analysis
  nhru <- length(nhru_area_weights)

  # Initialize list elements for gof metrics if the first one is not there
  if (!is.numeric(list_gof_smapsfwt[["nrmse_bybasin"]])) {
    list_gof_smapsfwt <- list(
      nrmse_bybasin = rep(NA, nruns), # scalar per model run
      nrmse_byhru = matrix(NA, nrow = nruns, ncol = nhru), # nhru stats per run
      kge_bybasin = rep(NA, nruns),
      kge_byhru = matrix(NA, nrow = nruns, ncol = nhru)
    )
  }

  # Calculate statistics --------------------------------------------------

  # By BASIN
  # Important to do for basin average instead of whole matrix
  # so errors are weighted by area
  smapsfwt_nrmse_bybasin_val <- hydroGOF::nrmse(
    sim = smapsfwt_sim$A_weighted_avg,
    obs = smapsfwt_obs$A_weighted_avg
  )
  smapsfwt_kge_bybasin_val <- hydroGOF::KGE(
    sim = smapsfwt_sim$A_weighted_avg,
    obs = smapsfwt_obs$A_weighted_avg
  )

  # BY HRU (performs operation column-wise)
  smapsfwt_nrmse_byhru_vect <- hydroGOF::nrmse(
    sim = smapsfwt_sim_matrix,
    obs = smapsfwt_obs_matrix
  )
  smapsfwt_kge_byhru_vect <- hydroGOF::KGE(
    sim = smapsfwt_sim_matrix,
    obs = smapsfwt_obs_matrix
  )

  # Append to list and write -----------------------------------------------

  list_gof_smapsfwt$nrmse_bybasin[run] <- smapsfwt_nrmse_bybasin_val
  list_gof_smapsfwt$kge_bybasin[run] <- smapsfwt_kge_bybasin_val
  list_gof_smapsfwt$nrmse_byhru[run, ] <- smapsfwt_nrmse_byhru_vect
  list_gof_smapsfwt$kge_byhru[run, ] <- smapsfwt_kge_byhru_vect

  return(list_gof_smapsfwt)
} # close fn







.op_gof_openet <- function(obs, # openet_data_df from pws.read.openet
                           sim, # from analyze.outputs
                           nhru_area_weights,
                           start_year,
                           end_year,
                           nruns = 1,
                           run = 1,
                           list_gof_openet = NULL) {
  # Initialize list_gof_openet if it is not provided
  if (is.null(list_gof_openet)) {
    list_gof_openet <- list()
  }

  # Checks on sim and obs data ----------------------------------------------

  # Trim simulated data frame to selected start and end date
  sim_data <- sim %>% filter(waterYear %in% seq(start_year, end_year))
  obs_data <- obs %>% filter(waterYear %in% seq(start_year, end_year))


  # Check for missing values in all relevant columns (sim_data)
  # Different between point obs and spatially distributed obs
  num_na <- suppressMessages(
    sum(rowSums(is.na(sim_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  simulation during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    sim_data <- sim_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }


  # Check for missing values in all relevant columns (obs_data)
  num_na <- suppressMessages(
    sum(rowSums(is.na(obs_data %>% select(starts_with("hru_")))) > 0)
  )
  if (num_na > 0) {
    warning(paste("There are", num_na, "dates with NA values for the
                  observations during the analysis period. Removing NAs"))
    # Remove rows with missing values in relevant columns
    obs_data <- obs_data %>%
      filter(if_all(starts_with("hru_"), ~ !is.na(.)))
  }

  # Check for obs data presence within the analysis period
  if (nrow(obs_data) == 0) {
    warning(paste("No observations for openet within the analysis period
               (", start_year, "-", end_year, "). Aborting .op_gof_.openet"))
    return(invisible(NULL)) # quitely exit this function
  }

  # Check if only part of the analysis period has data
  sim_years <- unique(sim_data$waterYear)
  if (min(sim_years) > start_year || max(sim_years) < end_year) {
    warning(paste("Simulation for hru_actet only cover part of the
                  analysis period (", min(sim_years), "-", max(sim_years), ").
                  Calculations will be based on this available range."))
  }

  # Check if only part of the analysis period has data
  obs_years <- unique(obs_data$waterYear)
  if (min(obs_years) > start_year || max(obs_years) < end_year) {
    warning(paste("Observations for openet only cover part of the
                  analysis period (", min(obs_years), "-", max(obs_years), ").
                  Calculations will be based on this available range."))
  }

  # Add area-weighted averages of the HRU based sim and obs -----------------

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim_data))
  hru_cols_obs <- grep("^hru_", names(obs_data))

  # Apply row-wise weighted average for `sim`
  sim_data$A_weighted_avg <- apply(
    sim_data[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Apply row-wise weighted average for `obs`
  obs_data$A_weighted_avg <- apply(
    obs_data[, hru_cols_obs], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  # Optionally relocate the new column (adjust column order)
  sim_data <- sim_data %>% relocate(A_weighted_avg, .before = hru_1)
  obs_data <- obs_data %>% relocate(A_weighted_avg, .before = hru_1)

  # UNIQUE TO THE OPENET WORKFLOW AGGREGATE SIMULATIONS BY MONTH
  # SUM SIMULATIONS BY MONTH -------------------------------------------

  sim_data <- sim_data %>%
    # Step 1: Adds a column YearMonth with the first of the month for each Date
    mutate(YearMonth = floor_date(Date, "month")) %>%
    # Step 2: Group by YearMonth and calculate monthly sums for relevant columns
    group_by(YearMonth) %>%
    summarize(
      Date = first(YearMonth), # Set Date to the first day of each month
      waterYear = first(waterYear), # Keep waterYear
      waterDay = first(waterDay), # Keep waterDay
      across(matches("^(A_weighted_avg|hru_)"), sum) # Sum columns w/o renaming
    ) %>%
    # Ungroup to return to regular dataframe structure
    ungroup() %>%
    # Select and reorder columns as in the original structure
    select(-YearMonth) %>%
    select(Date, waterYear, waterDay, everything())

  # Subset sim_data and obs_data based on shared dates ----------------------

  # Find the shared dates
  obs_data$Date <- as.Date(obs_data$Date) # sometimes charc if read through .csv
  sim_data$Date <- as.Date(sim_data$Date)
  shared_dates <- as.Date(intersect(sim_data$Date, obs_data$Date))

  # Check if there are any shared dates
  if (length(shared_dates) == 0) {
    warning("No overlapping dates between simulated and observed data.
            Aborting .op_gof_.openet")
    return(invisible(NULL)) # quitely exit this function
  } else {
    # Subset both data frames by shared dates
    openet_sim <- sim_data[sim_data$Date %in% shared_dates, ] # all columns
    openet_obs <- obs_data[obs_data$Date %in% shared_dates, ]

    # Date related work is done, subset the matrices and
    openet_sim_matrix <- openet_sim %>%
      ungroup() %>%
      select(starts_with("hru_"))
    openet_sim_matrix <- as.data.frame(openet_sim_matrix) # different class

    openet_obs_matrix <- openet_obs %>%
      ungroup() %>%
      select(starts_with("hru_"))

    err_abs <- abs(openet_sim_matrix - openet_obs_matrix)
  }

  # Initialize objects for analysis ------------------------------------------

  # Find dimensions for this analysis
  nhru <- length(nhru_area_weights)

  # Initialize list elements for gof metrics if the first one is not there
  if (!is.numeric(list_gof_openet[["nrmse_bybasin"]])) {
    list_gof_openet <- list(
      nrmse_bybasin = rep(NA, nruns), # scalar per model run
      nrmse_byhru = matrix(NA, nrow = nruns, ncol = nhru), # nhru stats per run
      kge_bybasin = rep(NA, nruns),
      kge_byhru = matrix(NA, nrow = nruns, ncol = nhru)
    )
  }

  # NOTE: WE USE nrmse IN THIS ANALYIS BECAUSE IT IS USED AS THE
  # CALIBRATION OBJECTIVE FUNCTION IN SEVERAL usgs STUDIES
  # SEE BELOW:
  # DOI: 10.3133/tm6B10
  # https://pubs.usgs.gov/publication/tm6B10 # equation 1

  # HOWEVER, kge has been used for multiple obseration by the Mai group
  # https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022WR032064

  # Calculate statistics --------------------------------------------------

  # By BASIN
  # Important to do for basin average instead of whole matrix
  # so errors are weighted by area
  openet_nrmse_bybasin_val <- hydroGOF::nrmse(
    sim = openet_sim$A_weighted_avg,
    obs = openet_obs$A_weighted_avg
  )
  openet_kge_bybasin_val <- hydroGOF::KGE(
    sim = openet_sim$A_weighted_avg,
    obs = openet_obs$A_weighted_avg
  )

  # BY HRU (performs operation column-wise)
  openet_nrmse_byhru_vect <- hydroGOF::nrmse(
    sim = openet_sim_matrix,
    obs = openet_obs_matrix
  )
  openet_kge_byhru_vect <- hydroGOF::KGE(
    sim = openet_sim_matrix,
    obs = openet_obs_matrix
  )

  # Append to list and write -----------------------------------------------

  list_gof_openet$nrmse_bybasin[run] <- openet_nrmse_bybasin_val
  list_gof_openet$kge_bybasin[run] <- openet_kge_bybasin_val
  list_gof_openet$nrmse_byhru[run, ] <- openet_nrmse_byhru_vect
  list_gof_openet$kge_byhru[run, ] <- openet_kge_byhru_vect

  return(list_gof_openet)
} # close fn





.op_metric_q <- function(sim,
                         start_year,
                         end_year,
                         nruns = 1,
                         run = 1,
                         rolling_window = 10,
                         list_metric_q = NULL) {
  # Initialize the metrics list if not provided
  if (is.null(list_metric_q)) {
    list_metric_q <- list()
  }

  # Extract seg cols dynamically (all cols except 'Date' and 'waterYear')
  segment_columns <- colnames(sim)[!(colnames(sim) %in% c(
    "Date", "waterYear", "waterDay"
  ))]

  # Calculate the total years
  total_years <- end_year - start_year + 1
  perform_rolling <- total_years >= rolling_window

  if (!perform_rolling) {
    warning(paste0(
      "The range of years is less than ", rolling_window,
      ". Rolling ", rolling_window, "-year analysis will be skipped."
    ))
  }

  # Determine rolling window analysis windows if applicable
  if (perform_rolling) {
    rolling_windows <- if (total_years == rolling_window) {
      list(c(start_year, end_year)) # Only one window
    } else {
      lapply(0:(total_years - rolling_window), function(i) {
        c(
          start_year + i, start_year + i + rolling_window - 1
        )
      })
    }
  }

  # Loop through each segment
  for (seg in segment_columns) {
    # Extract simulated data for the current segment
    sim_data <- sim %>%
      filter(waterYear %in% seq(start_year, end_year)) %>%
      select(Date, waterYear, !!sym(seg))

    # Check for missing values
    num_na <- sum(is.na(sim_data[[seg]]))
    if (num_na > 0) {
      warning(paste(
        "There are", num_na, "NA values for segment", seg,
        "during the analysis period. Removing NAs."
      ))
      sim_data <- sim_data[!is.na(sim_data[[seg]]), ]
    }

    # Check if data exists within the analysis period
    if (nrow(sim_data) == 0) {
      warning(paste(
        "No simulated data available for segment", seg,
        "within the analysis period (", start_year, "-", end_year, ").
        Skipping."
      ))
      next
    }

    # Initialize objects for analysis -------------------------------------

    # Create a "month" column in the full data
    sim_data <- sim_data %>%
      mutate(month = format(Date, "%m")) # Extract month as a two-digit string

    months <- sprintf("%02d", 1:12) # Ensure consistent month names (01 to 12)
    water_years <- sort(unique(sim_data$waterYear))
    nwateryears <- length(water_years)

    # Ensure initialization of results list for this segment
    if (!is.list(list_metric_q[[seg]])) {
      list_metric_q[[seg]] <- list(
        total = list(
          mean = numeric(nruns),
          median = numeric(nruns),
          max = numeric(nruns),
          min = numeric(nruns),
          sd = numeric(nruns),
          cv = numeric(nruns), # Coefficient of Variation added
          q10 = numeric(nruns), # 10th Quantile
          q90 = numeric(nruns) # 90th Quantile
        ),
        annual = list(
          mean = matrix(NA, nrow = nruns, ncol = nwateryears),
          median = matrix(NA, nrow = nruns, ncol = nwateryears),
          max = matrix(NA, nrow = nruns, ncol = nwateryears),
          min = matrix(NA, nrow = nruns, ncol = nwateryears),
          sd = matrix(NA, nrow = nruns, ncol = nwateryears),
          cv = matrix(NA, nrow = nruns, ncol = nwateryears), # CV for each wy
          q10 = matrix(NA, nrow = nruns, ncol = nwateryears), # 10th Quantile
          q90 = matrix(NA, nrow = nruns, ncol = nwateryears) # 90th Quantile
        ),
        monthly = list(
          mean = matrix(NA, nrow = nruns, ncol = length(months)),
          median = matrix(NA, nrow = nruns, ncol = length(months)),
          max = matrix(NA, nrow = nruns, ncol = length(months)),
          min = matrix(NA, nrow = nruns, ncol = length(months)),
          sd = matrix(NA, nrow = nruns, ncol = length(months)),
          cv = matrix(NA, nrow = nruns, ncol = length(months)), # CV for month
          q10 = matrix(NA, nrow = nruns, ncol = length(months)), # 10th Quantile
          q90 = matrix(NA, nrow = nruns, ncol = length(months)) # 90th Quantile
        )
      )
    }

    # Calculate statistics ------------------------------------------------

    # Total statistics
    mean_val <- mean(sim_data[[seg]])
    sd_val <- sd(sim_data[[seg]])
    q10_val <- quantile(sim_data[[seg]], 0.10)
    q90_val <- quantile(sim_data[[seg]], 0.90)

    list_metric_q[[seg]]$total$mean[run] <- mean_val
    list_metric_q[[seg]]$total$median[run] <- median(sim_data[[seg]])
    list_metric_q[[seg]]$total$max[run] <- max(sim_data[[seg]])
    list_metric_q[[seg]]$total$min[run] <- min(sim_data[[seg]])
    list_metric_q[[seg]]$total$sd[run] <- sd_val
    list_metric_q[[seg]]$total$cv[run] <- (sd_val / mean_val) * 100
    list_metric_q[[seg]]$total$q10[run] <- q10_val # 10th Quantile
    list_metric_q[[seg]]$total$q90[run] <- q90_val # 90th Quantile

    # Annual statistics
    for (wy_idx in seq_along(water_years)) {
      wy <- water_years[wy_idx]
      sim_annual <- sim_data[[seg]][sim_data$waterYear == wy]

      if (length(sim_annual) > 0) {
        annual_mean <- mean(sim_annual)
        annual_sd <- sd(sim_annual)
        annual_q10 <- quantile(sim_annual, 0.10)
        annual_q90 <- quantile(sim_annual, 0.90)

        list_metric_q[[seg]]$annual$mean[run, wy_idx] <- annual_mean
        list_metric_q[[seg]]$annual$median[run, wy_idx] <- median(sim_annual)
        list_metric_q[[seg]]$annual$max[run, wy_idx] <- max(sim_annual)
        list_metric_q[[seg]]$annual$min[run, wy_idx] <- min(sim_annual)
        list_metric_q[[seg]]$annual$sd[run, wy_idx] <- annual_sd
        list_metric_q[[seg]]$annual$cv[run] <- (annual_sd / annual_mean) * 100
        list_metric_q[[seg]]$annual$q10[run, wy_idx] <- annual_q10
        list_metric_q[[seg]]$annual$q90[run, wy_idx] <- annual_q90
      }
    }

    # Monthly statistics
    for (month_idx in seq_along(months)) {
      month <- months[month_idx]
      sim_monthly <- sim_data[[seg]][sim_data$month == month]

      if (length(sim_monthly) > 0) {
        monthly_mean <- mean(sim_monthly)
        monthly_sd <- sd(sim_monthly)
        monthly_q10 <- quantile(sim_monthly, 0.10)
        monthly_q90 <- quantile(sim_monthly, 0.90)

        list_metric_q[[seg]]$monthly$mean[run, month_idx] <- monthly_mean
        list_metric_q[[seg]]$monthly$median[
          run, month_idx
        ] <- median(sim_monthly)
        list_metric_q[[seg]]$monthly$max[run, month_idx] <- max(sim_monthly)
        list_metric_q[[seg]]$monthly$min[run, month_idx] <- min(sim_monthly)
        list_metric_q[[seg]]$monthly$sd[run, month_idx] <- monthly_sd
        list_metric_q[[seg]]$monthly$cv[
          run, month_idx
        ] <- (monthly_sd / monthly_mean) * 100
        list_metric_q[[seg]]$monthly$q10[run, month_idx] <- monthly_q10
        list_metric_q[[seg]]$monthly$q90[run, month_idx] <- monthly_q90
      }
    }

    # Skip rolling 20-year analysis if the period is insufficient
    if (!perform_rolling) {
      next
    }

    # Calculate rolling 20-year statistics
    for (window_idx in seq_along(rolling_windows)) {
      window <- rolling_windows[[window_idx]]
      start_window <- window[1]
      end_window <- window[2]

      sim_rolling <- sim_data[[seg]][sim_data$waterYear >= start_window &
        sim_data$waterYear <= end_window]

      if (length(sim_rolling) > 0) {
        if (is.null(
          list_metric_q[[seg]]$rolling[[paste0(
            start_window, "-", end_window
          )]]
        )) {
          list_metric_q[[seg]]$rolling[[
            paste0(start_window, "-", end_window)
          ]] <- list(
            mean = numeric(nruns),
            median = numeric(nruns),
            max = numeric(nruns),
            min = numeric(nruns),
            sd = numeric(nruns),
            cv = numeric(nruns), # Coefficient of Variation added
            q10 = numeric(nruns), # 10th Quantile
            q90 = numeric(nruns) # 90th Quantile
          )
        }

        rolling_mean <- mean(sim_rolling)
        rolling_sd <- sd(sim_rolling)
        rolling_q10 <- quantile(sim_rolling, 0.10)
        rolling_q90 <- quantile(sim_rolling, 0.90)

        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$mean[run] <- rolling_mean
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$median[
          run
        ] <- median(sim_rolling)
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$max[run] <- max(sim_rolling)
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$min[run] <- min(sim_rolling)
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$sd[run] <- rolling_sd
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$cv[
          run
        ] <- (rolling_sd / rolling_mean) * 100
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$q10[run] <- rolling_q10
        list_metric_q[[seg]]$rolling[[
          paste0(start_window, "-", end_window)
        ]]$q90[run] <- rolling_q90
      }
    }
  } # close for seg

  return(list_metric_q)
}






.op_metric_swe <- function(sim, # hru_output created in pws.analyze,output
                           run = 1,
                           nruns = 1,
                           start_year,
                           end_year,
                           dir_dynamic_input,
                           nhru_area_weights,
                           list_metric_swe = NULL) {
  # Initialize list_metric_swe if it is not provided
  if (is.null(list_metric_swe)) {
    list_metric_swe <- list()
  }

  # Loop through each day to create area-weighted swe -----------------------

  # Trim simulated data frame to selected start and end date
  sim <- sim %>% filter(waterYear %in% seq(start_year, end_year))

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim))

  # Apply row-wise weighted average for `sim`
  sim$A_weighted_avg <- apply(
    sim[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  sim <- sim %>% relocate(A_weighted_avg, .before = hru_1)

  # # Trim simulated data frame to selected start and end date
  # sim <- sim %>% filter(waterYear %in% seq(start_year, end_year))
  #
  # # Apply function to simulated df
  # sim$A_weighted_avg <- apply(
  #   sim[, (ncol(sim) - (length(nhru_area_ac) - 1)):ncol(sim)],
  #   1, # applys along the rows
  #   .calc_aweighted_avg
  # )
  #
  # # Relocate the A_weighted_avg column
  # sim <- sim %>% relocate(A_weighted_avg, .before = hru_1)

  # Loop through the df to calc annual metrics ---------------------

  # Find water years
  water_years <- unique(sim$waterYear)

  # Find number of HRUs
  nhru <- length(nhru_area_weights)

  # THIS STRUCUTRE FOR ARRAYS IS IMPORTANT TO FOLLOW IN .op_gof_. or pws.metric
  # vector dims: nruns (for basin_avg)
  # matrix dims: nruns*nyears OR nruns*nhru (for basin_ann and hru_avg)
  # array dims: nruns*nyears*nhru (for hru_ann)

  # Initialize dataframe for swe metrics
  metrics_swe <- list(
    waterYear = water_years,
    pk_basin_ann = rep(NA, length(water_years)), # vector of length water_years
    pk_basin_avg = NA, # scalar value per model run
    pk_hru_ann = matrix(NA, nrow = length(water_years), ncol = nhru),
    pk_hru_avg = rep(NA, nhru) # , # vector of length nhru
  )

  # Calculate annual statistics
  for (wy_index in seq_along(water_years)) {
    # Subset data for the current water year
    wy <- water_years[wy_index]
    annual_data <- sim %>% filter(waterYear == wy)

    # PEAK swe
    # Annual basin wide peak swe
    metrics_swe$pk_basin_ann[wy_index] <- max(annual_data$A_weighted_avg)
    # Annual hru-based peak swe, this subsets the last columns
    metrics_swe$pk_hru_ann[wy_index, ] <- apply(
      annual_data[, (ncol(annual_data) - (nhru - 1)):ncol(annual_data)], 2, max
    )
  } # close for wy_index

  # Calculate averages for all water years (entire simulation)
  metrics_swe$pk_basin_avg <- mean(metrics_swe$pk_basin_ann)
  metrics_swe$pk_hru_avg <- colMeans(metrics_swe$pk_hru_ann)

  # Store the results in a list, add dimension of nruns ---------------------

  # THIS STRUCUTRE FOR ARRAYS IS IMPORTANT TO FOLLOW IN .op_gof_. or pws.metric
  # vector dims: nruns
  # matrix dims: nruns*nyears OR nruns*nhru
  # array dim: nruns*nyears*nhru

  # Initializes list objects once when "pk_basin_ann" element does not exist
  if (!is.numeric(list_metric_swe[["pk_basin_ann"]])) {
    list_metric_swe <- list(
      # waterYear = matrix(NA, nrow = nruns, ncol = length(water_years)),
      pk_basin_ann = matrix(NA, nrow = nruns, ncol = length(water_years)),
      pk_basin_avg = numeric(nruns),
      pk_hru_ann = array(NA, dim = c(nruns, length(water_years), nhru)),
      pk_hru_avg = matrix(NA, nrow = nruns, ncol = nhru) # ,
    )
  }

  # Store the results for the current run
  # list_metric_swe$waterYear[run, ] <- metrics_swe$waterYear
  list_metric_swe$pk_basin_ann[run, ] <- metrics_swe$pk_basin_ann
  list_metric_swe$pk_basin_avg[run] <- metrics_swe$pk_basin_avg
  list_metric_swe$pk_hru_ann[run, , ] <- metrics_swe$pk_hru_ann
  list_metric_swe$pk_hru_avg[run, ] <- metrics_swe$pk_hru_avg

  return(list_metric_swe)
} # close fn






.op_metric_sms <- function(sim, # hru_output created in pws.analyze,output
                           run = 1,
                           nruns = 1,
                           start_year,
                           end_year,
                           dir_dynamic_input,
                           nhru_area_weights,
                           list_metric_sms = NULL) {
  # Initialize list_metric_sms if it is not provided
  if (is.null(list_metric_sms)) {
    list_metric_sms <- list()
  }

  # Loop through each day to create area-weighted swe -----------------------

  # Trim simulated data frame to selected start and end date
  sim <- sim %>% filter(waterYear %in% seq(start_year, end_year))

  # Dynamically find columns starting with "hru_"
  hru_cols_sim <- grep("^hru_", names(sim))

  # Apply row-wise weighted average for `sim`
  sim$A_weighted_avg <- apply(
    sim[, hru_cols_sim], 1,
    function(row) .calc_aweighted_avg(row, nhru_area_weights)
  )

  sim <- sim %>% relocate(A_weighted_avg, .before = hru_1)


  # # Apply function to simulated df
  # sim$A_weighted_avg <- apply(
  #   sim[, (ncol(sim) - (length(nhru_area_ac) - 1)):ncol(sim)], # the last cols
  #   1, # applys along the rows
  #   .calc_aweighted_avg
  # )
  #
  # # Relocate the A_weighted_avg column
  # # Hudely important step for proper column subsetting the in the following ops
  # sim <- sim %>% relocate(A_weighted_avg, .before = hru_1)

  # Loop through the df to calc annual metrics ---------------------

  # Find water years
  water_years <- unique(sim$waterYear)

  # Find number of HRUs
  nhru <- dimensions$nhru$dim_value

  # THIS STRUCUTRE FOR ARRAYS IS IMPORTANT TO FOLLOW IN .op_gof_. or pws.metric
  # vector dims: nruns (for basin_avg)
  # matrix dims: nruns*nyears OR nruns*nhru (for basin_ann and hru_avg)
  # array dims: nruns*nyears*nhru (for hru_ann)

  # Initialize dataframe for swe metrics
  metrics_sms <- list(
    waterYear = water_years,
    mean_basin_ann = rep(NA, length(water_years)), # vector of length nyears
    mean_basin_avg = NA, # scalar value per model run
    mean_hru_ann = matrix(NA, nrow = length(water_years), ncol = nhru),
    mean_hru_avg = rep(NA, nhru), # vector of length nhru

    cv_basin_ann = rep(NA, length(water_years)), # vector of length nyears
    cv_basin_avg = NA, # scalar value per model run
    cv_hru_ann = matrix(NA, nrow = length(water_years), ncol = nhru),
    cv_hru_avg = rep(NA, nhru) # , # vector of length nhru
  )

  # Calculate annual statistics
  for (wy_index in seq_along(water_years)) {
    # Subset data for the current water year
    wy <- water_years[wy_index]
    annual_data <- sim %>% filter(waterYear == wy)

    # MEAN sms
    # Annual hru-based peak Q, this subsets the last columns
    metrics_sms$mean_basin_ann[wy_index] <- mean(annual_data$A_weighted_avg)
    metrics_sms$mean_hru_ann[wy_index, ] <- apply(
      annual_data[, (ncol(annual_data) - (nhru - 1)):ncol(
        annual_data
      )], 2, mean
    )

    # COEFF OF VARIATION
    # metrics_sms$cv_basin_ann[wy_index] <- apply(annual_data$A_weighted_avg, 2,
    #                                             function(x){
    #                                               mean(x)/sd(x)*100
    #                                             })

    metrics_sms$cv_basin_ann[wy_index] <- (sd(
      annual_data$A_weighted_avg
    ) / mean(annual_data$A_weighted_avg)) * 100

    metrics_sms$cv_hru_ann[wy_index, ] <- apply(
      annual_data[, (ncol(annual_data) - (nhru - 1)):ncol(annual_data)], 2,
      function(x) {
        (sd(x) / mean(x)) * 100
      }
    )
  } # close for wy_index

  # CALCULATE AVERAGES FOR ALL WATERYEARS (entire simulation)
  metrics_sms$mean_basin_avg <- mean(sim$A_weighted_avg)

  metrics_sms$mean_hru_avg <- apply(
    sim[, (ncol(sim) - (nhru - 1)):ncol(sim)], 2, mean
  )

  metrics_sms$cv_basin_avg <- (sd(
    sim$A_weighted_avg
  ) / mean(sim$A_weighted_avg)) * 100

  metrics_sms$cv_hru_avg <- apply(
    sim[, (ncol(sim) - (nhru - 1)):ncol(sim)], 2, function(x) {
      (sd(x) / mean(x)) * 100
    }
  )

  # Store the results in a list, add dimension of nruns ---------------------

  # THIS STRUCUTRE FOR ARRAYS IS IMPORTANT TO FOLLOW IN .op_gof_. or pws.metric
  # vector dims: nruns
  # matrix dims: nruns*nyears OR nruns*nhru
  # array dim: nruns*nyears*nhru

  # Initializes list objects once when "pk_basin_ann" element does not exist
  if (!is.numeric(list_metric_sms[["mean_basin_ann"]])) {
    list_metric_sms <- list(
      # waterYear = matrix(NA, nrow = nruns, ncol = length(water_years)),
      mean_basin_ann = matrix(NA, nrow = nruns, ncol = length(water_years)),
      mean_basin_avg = numeric(nruns),
      mean_hru_ann = array(NA, dim = c(nruns, length(water_years), nhru)),
      mean_hru_avg = matrix(NA, nrow = nruns, ncol = nhru),
      cv_basin_ann = matrix(NA, nrow = nruns, ncol = length(water_years)),
      cv_basin_avg = numeric(nruns),
      cv_hru_ann = array(NA, dim = c(nruns, length(water_years), nhru)),
      cv_hru_avg = matrix(NA, nrow = nruns, ncol = nhru) # ,
    )
  }

  # Store the results for the current run
  # list_metric_sms$waterYear[run, ] <- metrics_sms$waterYear
  list_metric_sms$mean_basin_ann[run, ] <- metrics_sms$mean_basin_ann
  list_metric_sms$mean_basin_avg[run] <- metrics_sms$mean_basin_avg
  list_metric_sms$mean_hru_ann[run, , ] <- metrics_sms$mean_hru_ann
  list_metric_sms$mean_hru_avg[run, ] <- metrics_sms$mean_hru_avg

  list_metric_sms$cv_basin_ann[run, ] <- metrics_sms$cv_basin_ann
  list_metric_sms$cv_basin_avg[run] <- metrics_sms$cv_basin_avg
  list_metric_sms$cv_hru_ann[run, , ] <- metrics_sms$cv_hru_ann
  list_metric_sms$cv_hru_avg[run, ] <- metrics_sms$cv_hru_avg

  return(list_metric_sms)
} # close fn









#' Calculate Goodness of Fit for pws
#'
#' This function reads pws outputs and compares them to observations.
#'
#' @param dir_root character: parent working directory path.
#' @param project_name character: project name.
#' @param trial_number integer: trial number, leading zeros not needed.
#' @param start_year integer; first water year analyzed.
#' @param end_year integer: last water year analyzed.
#' @param hru_out_names character: vector of pws hru output file names (without
#' file extension e.g. "pkwater_equiv").
#' @param seg_out_names character: vector of pws segment output file names
#'  (without file extension e.g "seg_outflow").
#' @param default logical: set to TRUE when analyzing the single default run.
#' @param aso logical: set to TRUE when aso is to be included in analysis.
#' @param smapl4 logical: set to TRUE when smap is to be included in analysis.
#' @param overwrite logical: set to TRUE when existing analysis results are
#' to be overwritten.
#' @return list: nested list of goodness of fit results
#' @import dataRetrieval
#' @import tidyverse
#' @import ncdf4
#' @import hydroGOF
#' @import jsonlite
#' @export
calc_gof <- function(dir_root,
                     project_name,
                     trial_number,
                     start_year,
                     end_year,
                     hru_out_names = c(
                       "pkwater_equiv", "snowcov_area",
                       "hru_actet", "soil_rechr"
                     ),
                     seg_out_names = "seg_outflow",
                     default,
                     aso = FALSE,
                     smapl4 = FALSE,
                     overwrite = FALSE) {
  directories <- pwsMultiObsR:::fm_trial_set(
    dir_root, project_name,
    trial_number
  )
  dir_dynamic_output <- directories$dir_dynamic_output
  dir_dynamic_input <- directories$dir_dynamic_input
  dir_obs <- directories$dir_obs
  nruns <- directories$nruns

  # INITIALIZE LISTS --------------------------------------------------------

  output_list_names <- c()
  output_list_objects <- list()

  # Check which lists to write based on user input
  if ("pkwater_equiv" %in% hru_out_names) {
    output_list_names <- c(
      output_list_names, "list_gof_swe", "list_metric_swe"
    )
  }

  if ("soil_rechr" %in% hru_out_names) {
    output_list_names <- c(
      output_list_names, "list_gof_sms2", "list_gof_sms8",
      "list_gof_sms20", "list_metric_sms"
    )
  }

  if ("seg_outflow" %in% seg_out_names) {
    output_list_names <- c(output_list_names, "list_gof_q", "list_metric_q")
  }

  if ("hru_actet" %in% hru_out_names) {
    output_list_names <- c(output_list_names, "list_gof_openet")
  }

  if ("snowcov_area" %in% hru_out_names) {
    output_list_names <- c(output_list_names, "list_gof_mod10a1")
  }

  if (aso == TRUE) {
    output_list_names <- c(output_list_names, "list_gof_aso")
  }

  if (smapl4 == TRUE) {
    output_list_names <- c(output_list_names, "list_gof_smapsfwt")
  }

  # Construct output paths
  output_list_paths <- paste0(
    dir_dynamic_output, "/!", output_list_names, ".json"
  )
  # Give the paths the short name
  base::names(output_list_paths) <- output_list_names

  # Loop to load existing lists or initialize non-existing or overwrites
  for (i in seq_along(output_list_names)) {
    list_name <- output_list_names[i]
    list_path <- output_list_paths[i]

    if (file.exists(list_path) && !overwrite) {
      list_obj <- jsonlite::fromJSON(list_path)
      output_list_objects[[list_name]] <- list_obj
      message(paste("List:", list_name, "already exists. Skipping."))
    } else {
      output_list_objects[[list_name]] <- list()
      message(paste("Initialized new list:", list_name))
    }
  }

  # Check if all files exist and overwriting is disabled
  all_files_exist <- all(file.exists(output_list_paths))
  # If all files exist and overwrite is FALSE, abandon function early
  if (all_files_exist && !overwrite) {
    message("All files exist and overwrite is disabled. Abandoning process.")
    return(output_list_objects) # Exit the function or script early
  }

  # Helper function to check if file exists and whether to overwrite
  # Used in an if statement before calling .op_gof_ or pws.metric
  # Returns true if !file.exists = TRUE (does not) OR overwrite == TRUE
  should_compute <- function(output_list_name) {
    list_path <- output_list_paths[output_list_name]
    return(!file.exists(list_path) || overwrite)
  }

  # Give the user some info on this function
  start_time <- Sys.time()
  message(paste("Starting output analysis"))

  # LOAD OBS ---------------------------------------------------------------
  # Should throw error if these do not exist

  if (any(c("pkwater_equiv", "soil_rechr") %in% hru_out_names)) {
    nrcs_data_list <- jsonlite::fromJSON(file.path(
      dir_obs, "snotel/nrcs_data_list.json"
    ))
    nrcs_stations <- jsonlite::fromJSON(file.path(
      dir_obs, "snotel/nrcs_stations.json"
    ))
  }

  if ("seg_outflow" %in% seg_out_names) {
    usgs_data_list <- jsonlite::fromJSON(file.path(
      dir_obs, "usgs/usgs_data_list.json"
    ))
    usgs_stations <- jsonlite::fromJSON(file.path(
      dir_obs, "usgs/usgs_stations.json"
    ))
  }

  if ("hru_actet" %in% hru_out_names) {
    openet_data_df <- read.csv(file.path(dir_obs, "openet/openet_data_df.csv"))
    openet_data_df$Date <- as.Date(openet_data_df$Date)
  }

  if ("snowcov_area" %in% hru_out_names) {
    mod10a1_data_df <- read.csv(file.path(
      dir_obs,
      "mod10a1/mod10a1_data_df.csv"
    ))
    mod10a1_data_df$Date <- as.Date(mod10a1_data_df$Date)
  }

  if (aso == TRUE && "pkwater_equiv" %in% hru_out_names) {
    aso_data_df <- read.csv(file.path(
      dir_obs,
      "aso/aso_data_df.csv"
    ))
    aso_data_df$Date <- as.Date(aso_data_df$Date)
  }

  if (smapl4 == TRUE && "soil_rechr" %in% hru_out_names) {
    smapsfwt_data_df <- read.csv(file.path(
      dir_obs,
      "smapl4/smapsfwt_data_df.csv"
    ))
    smapsfwt_data_df$Date <- as.Date(smapsfwt_data_df$Date)
  }

  # CALC WEIGHTS ONCE -------------------------------------------------------

  dimensions <- .ig_dims_read(paste0(
    dir_dynamic_input, "/default_input/myparam.param"
  ))
  parameters <- .ig_params_read(
    paste0(dir_dynamic_input, "/default_input/myparam.param"),
    dimensions = dimensions
  )
  nhru_area_ac <- parameters$hru_area$param_value_matrix
  nhru_area_weights <- nhru_area_ac / sum(nhru_area_ac)


  # HRU VARIABLES -----------------------------------------------------------

  # Loops through every run folder in the trial specified by the output dir
  for (run in 1:nruns) {
    model <- paste0("run_", run)
    # Loops through the user specified user outputs
    for (j in seq_along(hru_out_names)) {
      # Add suffix based on model index
      output_name <- paste0(hru_out_names[j], "_", run)

      # Opens the nhm folder in the default_output_dir
      if (default == TRUE) {
        file_path <- file.path(
          dir_dynamic_output, "nhm",
          paste0(hru_out_names[j], ".nc")
        )
      } else {
        # Opens the run_xx folder in the trial dynamic output dir
        file_path <- file.path(
          dir_dynamic_output, model,
          paste0(hru_out_names[j], ".nc")
        ) # adds .nc suffix
      }

      nc <- ncdf4::nc_open(file_path) # Open the netCDF file
      variable <- ncdf4::ncvar_get(nc, hru_out_names[j]) # Get the variable
      variable <- t(variable) # Transpose
      time_dim <- ncdf4::ncvar_get(nc, "time") # Get the time variable
      ncdf4::nc_close(nc) # Close the netCDF file

      # Convert time variable to Date format, PRMS uses 1970-01-01 as reference
      time_date <- as.Date("1970-01-01") + time_dim

      # Add time vector to hru:time variable matrix
      hru_output <- data.frame(Date = time_date, variable)

      # Generate new column names # SENSITIVE TO DF STRUCTURE
      hru_names <- paste0("hru_", 1:(ncol(hru_output) - 1))

      # Rename the last 10 columns
      hru_output <- hru_output %>%
        rename_at(
          vars((ncol(hru_output) -
            (ncol(variable) - 1)):ncol(hru_output)),
          ~hru_names
        )

      # Add water year (dataRetrieval function)
      hru_output <- dataRetrieval::addWaterYear(hru_output)

      # Add water day (custom function)
      hru_output <- addWaterDay(
        df = hru_output,
        date_colname = Date,
        wy_colname = waterYear
      )

      # .op_gof_.swe
      if (output_name == paste0("pkwater_equiv_", run)) {
        # Helper fn should_compute defined in initialization section
        if (should_compute("list_gof_swe")) {
          output_list_objects[["list_gof_swe"]] <- .op_gof_swe(
            nrcs_stations = nrcs_stations,
            obs = nrcs_data_list,
            sim = hru_output, # Created in this script
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_swe = output_list_objects[["list_gof_swe"]]
          )
        }

        if (aso == TRUE) {
          if (should_compute("list_gof_aso")) {
            output_list_objects[["list_gof_aso"]] <- .op_gof_aso(
              obs = aso_data_df,
              sim = hru_output, # Created in this script
              nhru_area_weights = nhru_area_weights,
              start_year = start_year,
              end_year = end_year,
              nruns = nruns,
              run = run,
              list_gof_aso = output_list_objects[["list_gof_aso"]]
            )
          }
        } # close if aso

        if (should_compute("list_metric_swe")) {
          output_list_objects[["list_metric_swe"]] <- .op_metric_swe(
            sim = hru_output,
            run = run,
            nruns = nruns,
            start_year = start_year,
            end_year = end_year,
            dir_dynamic_input = dir_dynamic_input,
            nhru_area_weights = nhru_area_weights,
            list_metric_swe = output_list_objects[["list_metric_swe"]]
          )
        }
      } # close if (output_name == paste0("pkwater_equiv_", run))

      # .op_gof_.sms and smap
      # Changed from soil_lower_ratio_ to soil_moist_ or soil_rechr_
      if (output_name == paste0("soil_rechr_", run)) {
        if (should_compute("list_gof_sms2")) {
          output_list_objects[["list_gof_sms2"]] <- .op_gof_sms2(
            nrcs_stations = nrcs_stations,
            obs = nrcs_data_list,
            sim = hru_output, # Created in this script
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_sms2 = output_list_objects[["list_gof_sms2"]]
          ) # Created in this script
        }

        if (should_compute("list_gof_sms8")) {
          output_list_objects[["list_gof_sms8"]] <- .op_gof_sms8(
            nrcs_stations = nrcs_stations,
            obs = nrcs_data_list,
            sim = hru_output, # Created in this script
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_sms8 = output_list_objects[["list_gof_sms8"]]
          ) # Created in this script
        }

        if (should_compute("list_gof_sms20")) {
          output_list_objects[["list_gof_sms20"]] <- .op_gof_sms20(
            nrcs_stations = nrcs_stations,
            obs = nrcs_data_list,
            sim = hru_output, # Created in this script
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_sms20 = output_list_objects[["list_gof_sms20"]]
          ) # Created in this script
        }

        if (should_compute("list_metric_sms")) {
          output_list_objects[["list_metric_sms"]] <- .op_metric_sms(
            sim = hru_output,
            run = run,
            nruns = nruns,
            start_year = start_year,
            end_year = end_year,
            dir_dynamic_input = dir_dynamic_input,
            nhru_area_weights = nhru_area_weights,
            list_metric_sms = output_list_objects[["list_metric_sms"]]
          )
        }

        if (smapl4 == TRUE) {
          if (should_compute("list_gof_smapsfwt")) {
            output_list_objects[["list_gof_smapsfwt"]] <- .op_gof_smapsfwt(
              obs = smapsfwt_data_df,
              sim = hru_output,
              nhru_area_weights = nhru_area_weights,
              run = run,
              nruns = nruns,
              start_year = start_year,
              end_year = end_year,
              list_gof_smapsfwt = output_list_objects[["list_gof_smapsfwt"]]
            )
          }
        } # close if smapl4
      } # close if (output_name == paste0("soil_lower_ratio_", run))

      # .op_gof_.openet
      if (output_name == paste0("hru_actet_", run)) {
        # Helper fn should_compute define in initialization section
        if (should_compute("list_gof_openet")) {
          output_list_objects[["list_gof_openet"]] <- .op_gof_openet(
            obs = openet_data_df,
            sim = hru_output, # Created in this script
            nhru_area_weights = nhru_area_weights,
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_openet = output_list_objects[["list_gof_openet"]]
          ) # Created in this script
        }
      } # close if (output_name == paste0("hru_actet_", run))

      # .op_gof_.mod10a1
      if (output_name == paste0("snowcov_area_", run)) {
        # Helper fn should_compute define in initialization section
        if (should_compute("list_gof_mod10a1")) {
          output_list_objects[["list_gof_mod10a1"]] <- .op_gof_mod10a1(
            obs = mod10a1_data_df,
            sim = hru_output, # Created in this script
            nhru_area_weights = nhru_area_weights,
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_mod10a1 = output_list_objects[["list_gof_mod10a1"]]
          ) # Created in this script
        }
      } # close if (output_name == paste0("snowcov_area_", run))
    } # close hru_out_names loop

    # SEGMENT VARIABLES -----------------------------------------------------

    for (j in seq_along(seg_out_names)) {
      # Add suffix based on model index
      output_name <- paste0(seg_out_names[j], "_", run)

      # Opens the nhm folder in the default_output_dir
      if (default == TRUE) {
        file_path <- file.path(
          dir_dynamic_output, "nhm",
          paste0(seg_out_names[j], ".nc")
        )
      } else {
        # Opens the run_xx folder in the trial dynamic output dir
        file_path <- file.path(
          dir_dynamic_output, model,
          paste0(seg_out_names[j], ".nc")
        ) # adds .nc suffix
      }

      nc <- ncdf4::nc_open(file_path) # Open the netCDF file
      variable <- ncdf4::ncvar_get(nc, seg_out_names[j]) # Get the variable
      variable <- t(variable) # Transpose
      time_dim <- ncdf4::ncvar_get(nc, "time") # Get the time variable
      ncdf4::nc_close(nc) # Close the netCDF file

      # Convert time variable to Date format, PRMS uses 1970-01-01 as reference
      time_date <- as.Date("1970-01-01") + time_dim

      # Add time vector to hru:time variable matrix
      seg_output <- data.frame(Date = time_date, variable)

      # Generate new column names
      ### THIS IS THE ONLY DIFFERENCE FROM THE LOOP ABOVE
      # Changed "hru_names" to "seg_names"
      seg_names <- paste0("seg_", 1:(ncol(seg_output) - 1))

      # Rename the last 10 columns
      seg_output <- seg_output %>%
        rename_at(
          vars((ncol(seg_output) -
            (ncol(variable) - 1)):ncol(seg_output)),
          ~seg_names
        )

      # Add water year (dataRetrieval function)
      seg_output <- dataRetrieval::addWaterYear(seg_output)

      # Add water day (custom function)
      seg_output <- addWaterDay(
        df = seg_output,
        date_colname = Date,
        wy_colname = waterYear
      )

      # Call .op_gof_.Q for Q (hardcoded for now)
      if (output_name == paste0("seg_outflow_", run)) {
        if (should_compute("list_gof_q")) {
          output_list_objects[["list_gof_q"]] <- .op_gof_q(
            usgs_stations = usgs_stations,
            obs = usgs_data_list,
            sim = seg_output,
            start_year = start_year,
            end_year = end_year,
            nruns = nruns,
            run = run,
            list_gof_q = output_list_objects[["list_gof_q"]]
          )
        }

        if (should_compute("list_metric_q")) {
          output_list_objects[["list_metric_q"]] <- .op_metric_q(
            sim = seg_output,
            run = run,
            nruns = nruns,
            start_year = start_year,
            end_year = end_year,
            list_metric_q = output_list_objects[["list_metric_q"]]
          )
        }
      } # close
    } # close seg_out_names loop

    # Run counter
    # print(run)
    cat("\rProcessing:", run, "/", nruns)
  } # close run in nruns loop

  cat("\n")

  # WRITE LISTS -------------------------------------------------------------

  # Selectively write files (only overwrite when necessary)
  for (i in seq_along(output_list_names)) {
    list_name <- output_list_names[i]
    list_path <- output_list_paths[i]

    the_list <- output_list_objects[list_name]

    if (!file.exists(list_path) || overwrite) {
      # Write only if file doesn't exist or if overwrite is TRUE
      write_json(the_list, list_path)
      message(paste("List written:", list_name))
    } else {
      message(paste("Skipping writing list, already exists:", list_name))
    }
  }

  # ASSIGN LISTS ------------------------------------------------------------

  # for (list_name in output_list_names) {
  #   ouput_list_objects[list_name] <- get(list_name)
  # }

  # End timer and display elapsed time
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message(paste(
    "Total elapsed time:", elapsed_time,
    "for", nruns, "model runs"
  ))

  return(output_list_objects)
} # # close pws.analyze.output















# TODO integrate into workflow
#
# .check_inputs <- function(sim_data, obs_data,
#                          start_year, end_year,
#                          analysis, site_id = NULL){
#
#
#   # Follows identification and removal of NAs
#   if (nrow(obs_data) == 0) {
#     if is.null(site_id){
#       msg <- paste("No observations for" analysis, "within the analysis period
#                (", start_year, "-", end_year,").")
#     } else {
#       msg <- paste("No observations for" analysis,"site id:", site_id, "within
#                    the analysis period (", start_year, "-", end_year,").")
#     }
#     log_error(msg)
#     stop(msg)
#   }
#
#   sim_years <- unique(sim_data$waterYear)
#   if (min(sim_years) > start_year || max(sim_years) < end_year) {
#     if is.null(site_id){
#       paste("Observations for", analysis,
#             "only cover part of the analysis period (",
#             min(obs_years), "-", max(obs_years), ").
#             Calculations will be based on this available range."))
#     } else {
#       paste("Observations for", analysis, "site_id:", site_id,
#             "only cover part of the analysis period (",
#             min(obs_years), "-", max(obs_years), ").
#             Calculations will be based on this available range."))
#     }
#     log_warn(msg)
#     warning(msg)
#   }
#
#   obs_years <- unique(obs_data$waterYear)
#   if (min(obs_years) > start_year || max(obs_years) < end_year) {
#     if is.null(site_id){
#       paste("Observations for", analysis,
#             "only cover part of the analysis period (",
#             min(obs_years), "-", max(obs_years), ").
#             Calculations will be based on this available range."))
#     } else {
#       paste("Observations for", analysis, "site_id:", site_id,
#             "only cover part of the analysis period (",
#             min(obs_years), "-", max(obs_years), ").
#             Calculations will be based on this available range."))
#     }
#     log_warn(msg)
#     warning(msg)
#   }
# }
