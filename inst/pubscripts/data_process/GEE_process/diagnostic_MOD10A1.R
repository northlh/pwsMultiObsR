


# Set trial -----------------------------------------------------------

# Set trial
source("source/run_process/pws.set.trial.R")
pws.set.trial(project_name = "final_EastRiv",
              trial_number = 1) #if things went okay this should be 3


# Read in sim -----------------------------------------------------------
# Ratio from 0 to 1 (ie, saturation)

run = 1
nruns_dev = 1
#list_GOF_SMAPsfwt = NULL

# hru_out_names <- "soil_lower_ratio"
# hru_out_names <- "soil_moist" 
hru_out_names <- "snowcov_area"
default = TRUE

# Loops through every run folder in the trial specified by the output dir
for (run in 1:nruns_dev) {
  model <- paste0("run_",run)
  for (j in seq_along(hru_out_names)) {
    # Add suffix based on model index
    output_name <- paste0(hru_out_names[j], "_", run)
    
    # Opens the nhm folder in the default_output_dir
    if (default == TRUE){
      file_path <- file.path(dynamic_output_dir, "nhm",
                             paste0(hru_out_names[j],".nc"))
    } else {
      # Opens the run_xx folder in the trial dynamic output dir
      file_path <- file.path(dynamic_output_dir, model,
                             paste0(hru_out_names[j], ".nc")) #adds .nc suffix
    }
    
    nc <- nc_open(file_path) # Open the netCDF file
    variable <- ncvar_get(nc, hru_out_names[j]) # Get the variable
    variable <- t(variable) # Transpose
    time_dim <- ncvar_get(nc, "time") # Get the time variable
    nc_close(nc) # Close the netCDF file
    
    # Convert time variable to Date format, PRMS uses 1970-01-01 as reference
    time_date <- as.Date("1970-01-01") + time_dim
    
    # Add time vector to hru:time variable matrix
    hru_output <- data.frame(Date = time_date, variable)
    
    # Generate new column names
    hru_names <- paste0("hru_", 1:(ncol(hru_output)-1))
    
    # Rename the last 10 columns
    hru_output <- hru_output %>%
      rename_at(vars((ncol(hru_output) -
                        (ncol(variable)-1)):ncol(hru_output)),
                ~ hru_names)
    
    # Add water year (dataRetrieval function)
    hru_output <- addWaterYear(hru_output)
    
    # Add water day (custom function)
    hru_output <- addWaterDay(df = hru_output,
                              date_colname = Date,
                              wy_colname = waterYear)
    
    assign(output_name, hru_output, envir = .GlobalEnv)
  }
  
  
} # close loop to read in sims


# Read in SMAP -------------------------------

# ASO_obs_raw <- read.csv(file.path(obs_dir,"ASO/ASO_data_df.csv"))
# ASO_obs_raw$Date <- as.Date(ASO_obs_raw$Date)

MOD10A1_obs_raw <- read.csv(file.path(obs_dir,"MOD10A1/MOD10A1_data_df.csv"))
MOD10A1_obs_raw$Date <- as.Date(MOD10A1_obs_raw$Date)

MOD10A1_obs_raw <- MOD10A1_obs_raw %>%
  mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

# # Calc NSE for ASO
# shared_datesASO <- as.Date(intersect(pkwater_equiv_1$Date,
#                                   ASO_obs_raw$Date))
# 
# pkwater_equiv_1 <- pkwater_equiv_1 %>% filter(Date %in% shared_datesASO)
# ASO_obs_raw <- ASO_obs_raw %>% filter(Date %in% shared_datesASO)


# Calc NSE for SMS
shared_datesMOD10A1 <- as.Date(intersect(snowcov_area_1$Date,
                                     MOD10A1_obs_raw$Date))

snowcov_area_1 <- snowcov_area_1 %>% filter(Date %in% shared_datesMOD10A1)
MOD10A1_obs_raw <- MOD10A1_obs_raw %>% filter(Date %in% shared_datesMOD10A1)


MOD10A1_sim_matrix <- snowcov_area_1 %>%
  ungroup() %>%
  select(starts_with("hru_"))
MOD10A1_sim_matrix <- as.data.frame(MOD10A1_sim_matrix) #different class

MOD10A1_obs_matrix <- MOD10A1_obs_raw %>%
  ungroup() %>%
  select(starts_with("hru_"))


# UNIQUE TO MOD10A1
# Identify non-zero rows in the observed matrix (returns logical)
# Subset these in the data
# THIS EXCLUDES ROWS WHERE OBS = 0, I.E. SUMMER MONTHS OR ERRORS
# Step 1: Filter out non-zero rows
non_zero_rows <- rowSums(MOD10A1_obs_matrix != 0) > 0

MOD10A1_sim_matrix <- MOD10A1_sim_matrix[non_zero_rows, ]
MOD10A1_obs_matrix <- MOD10A1_obs_matrix[non_zero_rows, ]

# STEPS 2 and 3 WERE NOT IN THE PREVIOUS VERSION
# Step 2: Cap values greater than 1 to 1 in matrices
MOD10A1_sim_matrix[MOD10A1_sim_matrix > 1] <- 1
MOD10A1_obs_matrix[MOD10A1_obs_matrix > 1] <- 1

# Step 3: Filter rows where all values are <= 0.1
valid_rows <- rowSums(MOD10A1_obs_matrix > 0.1) > 0

MOD10A1_sim_matrix <- MOD10A1_sim_matrix[valid_rows, ]
MOD10A1_obs_matrix <- MOD10A1_obs_matrix[valid_rows, ]

# Calculate absolute error
err_abs <- abs(MOD10A1_sim_matrix - MOD10A1_obs_matrix)

# Prelim diagnostics ------------------------------------------------
# summary(pkwater_equiv_1)
# summary(ASO_obs_raw)
# 
summary(MOD10A1_obs_raw)
summary(snowcov_area_1)

summary(MOD10A1_sim_matrix)
summary(MOD10A1_obs_matrix)


# BY HRU (performs operation column-wise)
MOD10A1_NRMSE_byhru_vect0.1new <- hydroGOF::nrmse(sim = MOD10A1_sim_matrix,
                                            obs = MOD10A1_obs_matrix)


summary(MOD10A1_NRMSE_byhru_vect0.1)
summary(MOD10A1_NRMSE_byhru_vect0.1new)
summary(MOD10A1_NRMSE_byhru_vect0.2)
summary(MOD10A1_NRMSE_byhru_vect0.5)
summary(MOD10A1_NRMSE_byhru_vect0.8)
summary(MOD10A1_NRMSE_byhru_vect0.8new)
