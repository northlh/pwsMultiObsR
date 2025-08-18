# Module for lhs output processing



#' Find behavioral simulations
#'
#' Filter the goodness of fit results via user specified thresholds
#'
#' @param df_gof data.frame: A dataframe where the columns are observation
#' types, the rows are simulations, and values are GOF statistics.
#' @param gof_thresh numeric: A single value that you want to set as the
#' behavioral threshold.
#' @param cost logical: set to TRUE if using a cost function that should be
#' minimized (e.g. NRMSE, RMSE, PBIAS). Set to FALSE if using a likelihood
#' function that should be maximized (e.g. NSE, KGE).
#' @param alt_thresh numeric: An optional value if there are certain
#' observations that should follow and alternative threshold.
#' @param alt_colnames character: A vector specifying the column names in
#' 'df_gof' where the 'alt_thresh' should be applied.
#' @param diagnostic_hist logical: Set to TRUE to view a histogram of
#' goodness of fit metrics in 'df_gof'.
#' @return list: a nested list containing the indices and actual values of
#' behavioral simulations.
#' @export
find_behav <- function(
    df_gof,
    gof_thresh,
    cost,
    alt_thresh = NULL,
    alt_cols = NULL,
    diagnostic_hist = TRUE) {
  if (diagnostic_hist == TRUE) {
    for (i in 1:length(df_gof)) {
      xnames <- colnames(df_gof)
      hist(df_gof[, i], main = paste0("Histogram of ", xnames[i]))
    }
  }

  idx_behav <- list()
  idx_best <- list()
  gof_best <- list()
  gof_behav <- list()

  for (i in 1:length(df_gof)) {
    obs_name <- names(df_gof)[i]

    use_alt_thresh <- any(sapply(alt_cols, function(x) grepl(x, obs_name)))
    current_thresh <- ifelse(use_alt_thresh, alt_thresh, gof_thresh)

    if (cost == TRUE) {
      idx_behav[[obs_name]] <- which(df_gof[[obs_name]] <= current_thresh)
      idx_best[[obs_name]] <- which.min(df_gof[[obs_name]])
    } else if (cost == FALSE) {
      idx_behav[[obs_name]] <- which(df_gof[[obs_name]] >= current_thresh)
      idx_best[[obs_name]] <- which.max(df_gof[[obs_name]])
    } else {
      stop("Invalid input for kwarg 'cost'. Must be TRUE or FALSE")
    }

    gof_behav[[obs_name]] <- df_gof[[obs_name]][idx_behav[[obs_name]]]
    gof_best[[obs_name]] <- min(df_gof[[obs_name]])
  }

  return(list(
    "idx_behav" = idx_behav,
    "gof_behav" = gof_behav,
    "idx_best" = idx_best,
    "gof_best" = gof_best
  ))
}



#' Find behavioral simulation intersection
#'
#' Filter the goodness of fit results to intersections of multiple observations.
#'
#' @details
#' This should be run after find_behav and defining the multi-objective
#' criteria in a list.
#'
#' @param df_gof data.frame: A dataframe where the columns are observation
#' types, the rows are simulations, and values are GOF statistics.
#' @param criteria_names list: a list of the criteria, where each element of the
#' list contains a set of column names in 'df_gof' to intersect.
#' @param idx_behav data.frame: one of the outputs from find_beahv. It contains
#' vectors of the simulation indices for behavioral simulations.
#' @param q_col integer: The column index in 'df_gof' which is used to return
#' a dataframe of the behavioral goodness of fit for that column. Generally
#' expects that Q is of interest.
#' @return list: contains a dataframe if simulation indexes the meet the
#' intersection crtieria, as well as a dataframe of the GOF that correspond to
#' the selected 'q_col'.
#' @export
find_behav_intersect <- function(
    df_gof,
    criteria_names,
    idx_behav,
    q_col = 1) {
  idx_behav_joint <- list()
  gofq_behav_joint <- list()

  for (criterion in names(criteria_names)) {
    vectors_to_compare <- idx_behav[criteria_names[[criterion]]]

    # IMPORTANT METHODOLOGICAL CHOICE OF A HARD INTERSECTION
    behav_indices_criteria <- Reduce(intersect, vectors_to_compare)

    # Only process further if there are common indices
    if (length(behav_indices_criteria) > 0) {
      idx_behav_joint[[criterion]] <- behav_indices_criteria
      # Pulls q column of df
      gofq_behav_joint[[criterion]] <- (
        df_gof[[q_col]][idx_behav_joint[[criterion]]])
    }
  }

  return(list(
    "idx_behav_joint" = idx_behav_joint,
    "gof_behav_joint" = gofq_behav_joint
  ))
}
