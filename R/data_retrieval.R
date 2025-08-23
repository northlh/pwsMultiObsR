# This module hosts R-based data retireval methods, paticularly USGS and nrcs





#' Add water day
#'
#' Create a trial child directory for a project and generate parameter files.
#' Note that for large morris or mmlhs trials, it may take a long time for the
#' function to complete.
#'
#' @param df data.frame: dataframe to add waterday. Must have waterYear and
#' date column
#' @param date_colname name of the df column of class "Date"
#' @param wy_colname name of the df column of class "numeric". It is
#' suggested to use dataRetrieval;addWaterYear to create this column.
#' @return data.frame: original dataframe with waterDay column added
#' @examples
#'
#' df_w_waterday <- addWaterDay(df,
#'   date_colname = Date,
#'   wy_colname = waterYear
#' )
#'
#' @import dplyr
#' @import lubridate
#' @export
addWaterDay <- function(df, date_colname, wy_colname) {
  new_df <- df %>%
    group_by({{ wy_colname }}) %>%
    mutate(waterDay = as.integer(difftime(
      as.Date({{ date_colname }}),
      ymd(paste0({{ wy_colname }} - 1, "-09-30")),
      units = "days"
    ))) %>%
    relocate(waterDay, .after = {{ wy_colname }})

  return(new_df)
}



#' Retrieve USGS daily mean discharge
#'
#' Return and save a list of USGS discharge data
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param usgs_stations list: contains station IDs and respective pws segments
#' @param start_date character: in "YYYY-MM-DD" format
#' @param end_date character: in "YYYY-MM-DD" format
#' @param overwrite logical: TRUE or FALSE to overwrite existing files
#' @return list: retrieved data
#' @examples
#'
#' # Input list must follow this format
#' usgs_stations <- list(
#'   site_ids = c("09112500", "09112200"),
#'   seg_ids = c(5, 4)
#' )
#'
#'
#' start_date <- "1970-10-01"
#' end_date <- "2023-09-30"
#'
#' retrieve_q(
#'   dir_proj = "/Users/me/mywork/",
#'   project_name = "East_River",
#'   usgs_stations = usgs_stations,
#'   start_date = "1970-10-01",
#'   end_date = "2023-09-30",
#'   overwrite = FALSE
#' )
#'
#' @import dplyr
#' @import lubridate
#' @import dataRetrieval
#' @export
retrieve_q <- function(
    dir_root,
    project_name,
    usgs_stations,
    start_date,
    end_date,
    overwrite = FALSE) {
  # CHECKS -----------------------------------------------------------------

  directories <- pwsMultiObsR:::fm_trial_set(dir_root, project_name, 1)
  project_path <- directories$dir_project
  obs_path <- directories$dir_obs

  if (!dir_exists(project_path)) {
    stop("Project does not exist")
  }

  if (!dir_exists(obs_path)) {
    stop(paste("Observation directory:", obs_path, "does not exist"))
  }

  # Set output file name
  usgs_data_list_filepath <- file.path(obs_path, "usgs/usgs_data_list.json")

  # Flag to track whether the file already existed
  file_existed <- file.exists(usgs_data_list_filepath)

  # Check if the file already exists
  if (file.exists(usgs_data_list_filepath)) {
    if (!overwrite) {
      # Read the existing file before stopping
      usgs_data_list <- jsonlite::fromJSON(usgs_data_list_filepath)
      warning(
        paste(
          "File", usgs_data_list_filepath,
          "already exists. To overwrite, re-run with overwrite = TRUE."
        )
      )
      return(usgs_data_list)
    }
  }


  if (is.null(usgs_stations) || length(usgs_stations$site_ids) == 0) {
    stop("usgs_stations is NULL or empty. Please provide valid station data.")
  }

  # USE DATA RETRIEVAL PACKAGE -----------------------------------------------

  usgs_data_list <- list()

  for (i in seq_along(usgs_stations$site_ids)) {
    site_id <- usgs_stations$site_ids[i] # only place where the i object is used

    # Read in the data using dataRetrieval
    USGS_data <- dataRetrieval::readNWISdv(
      siteNumbers = site_id,
      parameterCd = "00060", # discharge (cfs)
      startDate = start_date,
      endDate = end_date,
      statCd = "00003"
    ) # daily mean discharge

    USGS_data <- data.frame(
      Date = USGS_data$Date,
      Q_cfs = USGS_data$X_00060_00003
    )

    USGS_data <- dataRetrieval::addWaterYear(USGS_data)
    USGS_data <- pwsMultiObsR::addWaterDay(
      df = USGS_data,
      date_colname = Date,
      wy_colname = waterYear
    )

    usgs_data_list[[site_id]] <- USGS_data
  }

  jsonlite::write_json(usgs_data_list, usgs_data_list_filepath)
  jsonlite::write_json(usgs_stations, file.path(
    obs_path,
    "usgs/usgs_stations.json"
  ))
  if (file_existed) {
    message("File is overwritten as 'overwrite' is set to TRUE.")
  } else {
    message("File created successfully.")
  }

  return(usgs_data_list)
}





#' Retrieve nrcs data from the SNOTEL network
#'
#' Return and save a list of SNOTEL data
#'
#' Data is accessed via the nrcs report generator:
#' https://wcc.sc.egov.usda.gov/reportGenerator/
#'
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param nrcs_stations list: contains station IDs and respective pws HRU's.
#'     state_id: the two-letter state abbreviations
#'       site_ids: numeric vector containing the site ID numbers for nrcs
#'       hru_ids: numeric vector of the corresponding hru number to site_id
#' @param overwrite logical: TRUE or FALSE to overwrite existing files
#' @return list: retrieved data of full period of record (not checked)
#' @examples
#'
#' # Input list must follow this format
#' nrcs_stations_test <- list(CO = list(
#'   site_ids = c(380, 737),
#'   #                                       hru_ids = c(8,10)),
#'
#'   CA = list(
#'     site_ids = c(356),
#'     #                                       hru_ids = c(8)),
#'
#'     UT = list(site_ids = c(766)),
#'     hru_ids = c(8)
#'   )
#' ))
#'
#' nrcs_data <- retrieve_nrcs(
#'   dir_proj = "/Users/me/mywork/",
#'   project_name = "East_River",
#'   nrcs_stations = nrcs_stations,
#'   overwrite = FALSE
#' )
#'
#' @import dplyr
#' @import lubridate
#' @export
retrieve_nrcs <- function(
    dir_root,
    project_name,
    nrcs_stations,
    overwrite = FALSE) {
  # TODO: add flexibility for variable selection

  # CHECKS ------------------------------------------------------------------

  directories <- pwsMultiObsR:::fm_trial_set(dir_root, project_name, 1)
  project_path <- directories$dir_project
  obs_path <- directories$dir_obs

  if (!dir_exists(as.character(project_path))) {
    stop("Project does not exist")
  }

  if (!dir_exists(as.character(obs_path))) {
    stop(paste("Observation directory:", obs_path, "does not exist"))
  }

  nrcs_data_list_filepath <- file.path(obs_path, "snotel/nrcs_data_list.json")

  file_existed <- file.exists(nrcs_data_list_filepath)


  if (file.exists(nrcs_data_list_filepath)) {
    if (!overwrite) {
      nrcs_data_list <- jsonlite::fromJSON(nrcs_data_list_filepath)
      warning(
        paste(
          "File", nrcs_data_list_filepath,
          "already exists. To overwrite it, re-run with overwrite = TRUE."
        )
      )
      return(nrcs_data_list)
    }
  }

  if (is.null(nrcs_stations) || length(nrcs_stations[1]) == 0) {
    stop("nrcs_stations is NULL or empty. Please provide valid station data.")
  }

  # USE nrcs REPORT GENERATOR -----------------------------------------------

  # This vector can be used to load all relevant data from the url
  obs_param_ids <- c("WTEQ:", "SMS:-2", "SMS:-8", "SMS:-20")
  # ("WTEQ:","PREC:","TMAX:","TMIN:","TAVG:","PRCP:","SNWD:",
  # "SMS:-2","SMS:-8","SMS:-20",
  # "STO:-2","STO:-8","STO:-20")

  # This is used to make the nrcs data column names nicer
  nrcs_colnames <- c("Date", "WTEQ", "SMS2", "SMS8", "SMS20")
  # "Date","WTEQ","PREC",
  # "TMAX","TMIN","TAVG","PRCP","SNWD",
  # "SMS2","SMS8","SMS20",
  # "STO2","STO8","STO20")

  nrcs_data_list <- list()

  for (state_id in names(nrcs_stations)) {
    site_ids <- nrcs_stations[[state_id]]$site_ids
    hru_ids <- nrcs_stations[[state_id]]$hru_ids

    for (i in seq_along(site_ids)) {
      site_id <- site_ids[i]
      hru_id <- hru_ids[i]

      # Construct the URL based on the provided arguments
      url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/",
        site_id, ":", state_id, ":SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/",
        paste0(obs_param_ids, ":value", collapse = ",")
      )

      nrcs_data <- read.csv(url, comment.char = "#")

      colnames(nrcs_data) <- nrcs_colnames

      nrcs_data <- addWaterYear(nrcs_data)
      nrcs_data <- addWaterDay(
        df = nrcs_data,
        date_colname = Date,
        wy_colname = waterYear
      )

      nrcs_data$Date <- as.Date(nrcs_data$Date)
      nrcs_data_list[[state_id]][[paste0(site_id)]] <- nrcs_data
    }
  }

  # Save the file (either a new file or overwrite the existing one)
  jsonlite::write_json(nrcs_data_list, nrcs_data_list_filepath)
  jsonlite::write_json(nrcs_stations, file.path(
    obs_path,
    "snotel/nrcs_stations.json"
  ))
  if (file_existed) {
    message("File is overwritten as 'overwrite' is set to TRUE.")
  } else {
    message("File created successfully.")
  }

  return(nrcs_data_list)
}
