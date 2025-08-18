
# Load `here` for path management
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", quietly = TRUE)
}
library(here)

rm(list = ls()) # Clear Environment
source(file.path(here(),"source/run_process/pws.initialize.R")) # Source fn
pws.initialize() # Loads this repository of scripts



# ---------------------------------------------------------------------------
# Read EET Data
# ---------------------------------------------------------------------------

# Can be used in a loop for many projects or trials
test_proj_names <- c("day_BlueRiv", "day_DoloresRiv",
                     "day_EastRiv", "day_TaylorRiv")

test_trial_nums <- c(2,2,2,2)

variables <- c("Q","SWE","ASO","MOD10A1","SMS2", "SMAPsfwt","openet")

for (i in 1:length(test_proj_names)){
  
  pws.read.lists(project_name = test_proj_names[i],
                 trial_number =  test_trial_nums[i],
                 var_names = variables,
                 metrics = "GOF",
                 list_type = "EET")
  
}



# ---------------------------------------------------------------------------
# Read cbh files, compute anomalies
# ---------------------------------------------------------------------------

# function used in a row apply where nhru_area_weights is fixed by name
area.weighted.avg <- function(row){
  weighted_avg <- sum(nhru_area_weights*row)/sum(nhru_area_weights)
  return(weighted_avg)
} # close fn


source(file.path(here(),"source/data_process/cbh_process/pws.read.cbh.R"))

# Loops through project names to extract each file type for each project
for (i in 1:length(test_proj_names)){
  
  name_proj <- test_proj_names[i] #get individual proj name
  
  
  
  # EXTRACT HRU AREAS
  # set trial dynamically
  pws.set.trial(project_name = test_proj_names[i],
                trial_number =  test_trial_nums[i])
  dimensions <- pws.read.dims(paste0(dynamic_input_dir,"/default_input/myparam.param"))
  parameters <- pws.read.params(paste0(dynamic_input_dir,"/default_input/myparam.param"))
  nhru_area_ac <- parameters$hru_area$param_value_matrix #extracts area in acres from default parameter file
  nhru_area_weights <- nhru_area_ac/sum(nhru_area_ac) #storing this in the loop
  
  
  
  # LOAD BASE CBH FILES
  # Precip data
  path_local_prcp <- file.path(here(),"Projects",name_proj,"default_input/prcp.cbh")
  df_name_prcp <- paste0("df_prcp_", name_proj)
  df_prcp <- pws.read.cbh(path_local_prcp) #read in
  df_prcp <- addWaterYear(df_prcp) #add water day
  # assign(df_name_prcp, df_prcp, envir = .GlobalEnv)
  
  # tmax
  path_local_tmax <- file.path(here(),"Projects",name_proj,"default_input/tmax.cbh")
  df_name_tmax <- paste0("df_tmax_", name_proj)
  df_tmax <- pws.read.cbh(path_local_tmax) #read in
  df_tmax <- addWaterYear(df_tmax) #add water day
  # assign(df_name_tmax, df_tmax, envir = .GlobalEnv)
  
  # tmin
  path_local_tmin <- file.path(here(),"Projects",name_proj,"default_input/tmin.cbh")
  df_name_tmin <- paste0("df_tmin_", name_proj)
  df_tmin <- pws.read.cbh(path_local_tmin) #read in
  df_tmin <- addWaterYear(df_tmin) #add water day
  # assign(df_name_tmin, df_tmin, envir = .GlobalEnv)
  
  
  
  # COMPUTE ANNUAL SUMS (PRECIP) OR MEANS (TEMP)
  # Annual average precip
  df_prcp_ann <- df_prcp %>%
    filter(waterYear >= 1982 & waterYear <= 2022) %>%  # Filter relevant years
    group_by(waterYear) %>%  # Group by water year
    summarise(across(starts_with("hru_"), \(x) sum(x, na.rm = TRUE))) # Compute annual total
  hru_cols_prcp <- grep("^hru_", names(df_prcp_ann)) # find columns starting with "hru_"
  df_prcp_ann$A_weighted_avg <- apply(  # Apply row-wise weighted average 
    df_prcp_ann[ , hru_cols_prcp], 1, area.weighted.avg)

  # Annual average tmax
  df_tmax_ann <- df_tmax %>%
    filter(waterYear >= 1982 & waterYear <= 2022) %>%  # Filter relevant water years
    group_by(waterYear) %>%  # Group by water year and season
    summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE)))  # Compute means
  hru_cols_tmax <- grep("^hru_", names(df_tmax_ann)) # find columns starting with "hru_"
  df_tmax_ann$A_weighted_avg <- apply(  # Apply row-wise weighted average 
    df_tmax_ann[ , hru_cols_tmax], 1, area.weighted.avg)
  
  # Annual average tmin
  df_tmin_ann <- df_tmin %>%
    filter(waterYear >= 1982 & waterYear <= 2022) %>%  # Filter relevant water years
    group_by(waterYear) %>%  # Group by water year and season
    summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE)))  # Compute means
  hru_cols_tmin <- grep("^hru_", names(df_tmin_ann)) # find columns starting with "hru_"
  df_tmin_ann$A_weighted_avg <- apply(  # Apply row-wise weighted average 
    df_tmin_ann[ , hru_cols_tmin], 1, area.weighted.avg)
  
  
  
  # Unit Conversions
  # Convert prcp (inches to mm)
  df_prcp_ann[ , hru_cols_prcp] <- df_prcp_ann[ , hru_cols_prcp] * 25.4
  df_prcp_ann$A_weighted_avg <- df_prcp_ann$A_weighted_avg * 25.4
  
  # Convert tmax (F to C)
  df_tmax_ann[ , hru_cols_tmax] <- (df_tmax_ann[ , hru_cols_tmax] - 32) * (5/9)
  df_tmax_ann$A_weighted_avg <- (df_tmax_ann$A_weighted_avg - 32) * (5/9)
  
  # Convert tmin (F to C)
  df_tmin_ann[ , hru_cols_tmin] <- (df_tmin_ann[ , hru_cols_tmin] - 32) * (5/9)
  df_tmin_ann$A_weighted_avg <- (df_tmin_ann$A_weighted_avg - 32) * (5/9)
  
  
  
  # Optional assignment
  assign(paste0("df_prcp_ann_", name_proj), df_prcp_ann, envir = .GlobalEnv)
  assign(paste0("df_tmax_ann_", name_proj), df_tmax_ann, envir = .GlobalEnv) 
  assign(paste0("df_tmin_ann_", name_proj), df_tmin_ann, envir = .GlobalEnv) 
  
  
  
  # Convert to anomalies based on 41 year average
  # Mean values over the full period (area-weighted)
  mean_prcp <- mean(df_prcp_ann$A_weighted_avg, na.rm = TRUE)
  mean_tmax <- mean(df_tmax_ann$A_weighted_avg, na.rm = TRUE)
  mean_tmin <- mean(df_tmin_ann$A_weighted_avg, na.rm = TRUE)
  
  # Anomaly dataframes
  df_prcp_anom <- df_prcp_ann %>%
    mutate(prcp_anomaly = A_weighted_avg - mean_prcp)
  df_tmax_anom <- df_tmax_ann %>%
    mutate(tmax_anomaly = A_weighted_avg - mean_tmax)
  df_tmin_anom <- df_tmin_ann %>%
    mutate(tmin_anomaly = A_weighted_avg - mean_tmin)
  
  # Optional: assign to environment if needed
  assign(paste0("df_prcp_anom_", name_proj), df_prcp_anom, envir = .GlobalEnv)
  assign(paste0("df_tmax_anom_", name_proj), df_tmax_anom, envir = .GlobalEnv)
  assign(paste0("df_tmin_anom_", name_proj), df_tmin_anom, envir = .GlobalEnv)
  
  
}

# get into df
# heatmap
# p t biaxial




# ---------------------------------------------------------------------------
# Pull mu* data
# ---------------------------------------------------------------------------

# INPUTS TO LOOP
# Adapted from sensi_heat_temporal annual diff and sensi_cor_facet

selected_params <-  c("tmax_allsnow", "tmax_cbh_adj",
                      #"snow_cbh_adj",
                      #"dday_intcp", "den_max",
                      "rad_trncf",
                      "jh_coef"#,"soil_moist_max",
                      #"gwflow_coef"
)

measures <- c("lower", "mean", "upper") #sensi metric bounds from bootstrap
names_gages <- c("09046600","09166500","09112500","09107000") #gage ID
Q_years_start <- c(1982, 1982, 1982, 1988) # gage starting year

# Create list for difference dataframes
list_mu_annual <- list()

for (i in 1:length(test_proj_names)){

  name_proj <- test_proj_names[i] #get individual proj name
  name_gage <- names_gages[i] # get gage names
  start_year <- Q_years_start[i] # get starting year

  data_Q <- get(paste0("list_data_",name_proj))[["Q_GOF"]][[name_gage]] #extract GOF from list
  data_params <- get(paste0("params_",name_proj)) # extract params
  names_params <- colnames(data_params) # store param names

  n_years <- length(data_Q$annual$NRMSE$bootstrap$mu_star) # need for extracting annual data

  # print(paste0("i is:",i))

  for (j in seq_along(measures)) {

    df_name <- paste0("df_", measures[j]) # lower, mean, upper from bootstrap

    # Extract the annual data
    # NOTE: Taylor data starts in 1988 rather than 1982 when model starts
    df_mu_annual <- do.call(cbind, lapply(1:n_years, function(k) {
      data_Q$annual$NRMSE$bootstrap$mu_star[[k]][, j]
    })) # annual data


    # df_mu_annual <- df_mu_annual/100 #converting from NRMSE% to absolute
    colnames(df_mu_annual) <- start_year:(start_year + n_years - 1) # colname by yr
    rownames(df_mu_annual) <- names_params # row names by param
    df_mu_annual <- df_mu_annual[selected_params, ]

    # Store in ljst
    list_mu_annual[[df_name]] <- df_mu_annual

  } #close j

  # Assign objects
  assign(paste0("list_mu_ann_", name_proj), list_mu_annual, envir = .GlobalEnv)

} #close i












# # COMPUTES ANOMALIES BASED ON THE TOTAL EE
# # I NOTICED IF I TAKE AN ANNUAL AVERAGE IT IS NOT THE SAME AS THE TOTAL TIMESERIES EE
# # ABANDONING THE IDEA OF MU* ANOMALIE
# 
# list_mu_diffs <- list()
# 
# for (i in 1:length(test_proj_names)){
# 
#   name_proj <- test_proj_names[i] #get individual proj name
#   name_gage <- names_gages[i] # get gage names
#   start_year <- Q_years_start[i] # get starting year
# 
#   data_Q <- get(paste0("list_data_",name_proj))[["Q_GOF"]][[name_gage]] #extract GOF from list
#   data_params <- get(paste0("params_",name_proj)) # extract params
#   names_params <- colnames(data_params) # store param names
# 
#   n_years <- length(data_Q$annual$NRMSE$bootstrap$mu_star) # need for extracting annual data
# 
#   # print(paste0("i is:",i))
# 
#   for (j in seq_along(measures)) {
# 
#     df_name <- paste0("df_", measures[j]) # lower, mean, upper from bootstrap
# 
#     # Extract the total and annual data
#     # NOTE: Taylor data starts in 1988 rather than 1982 when model starts
#     df_mu_total <- data_Q$total$NRMSE$bootstrap$mu_star[, j] #bootstrap over timeseries
#     df_mu_annual <- do.call(cbind, lapply(1:n_years, function(k) {
#       data_Q$annual$NRMSE$bootstrap$mu_star[[k]][, j]
#     })) # annual data
# 
#     df_diff <- df_mu_annual - df_mu_total # Compute the anomalie (ann-tot)
#     df_diff <- df_diff/100 #converting from NRMSE% to absolute
#     # df_diff <- as.data.frame(df_diff) # convert to df
#     colnames(df_diff) <- start_year:(start_year + n_years - 1) # colname by yr
#     rownames(df_diff) <- names_params # row names by param
#     df_diff <- df_diff[selected_params, ]
# 
#     # Store in ljst
#     list_mu_diffs[[df_name]] <- df_diff
# 
#   } #close j
# 
#   # Assign objects
#   assign(paste0("list_mu_anom_", name_proj), list_mu_diffs, envir = .GlobalEnv)
# 
# } #close i




# ---------------------------------------------------------------------------
# Combine into plotting df
# ---------------------------------------------------------------------------



# test <- list_mu_ann_day_BlueRiv$df_mean
# test <- t(test)
# 
# test_P <- data.frame(waterYear = df_prcp_anom_day_BlueRiv$waterYear,
#                      Blue = df_prcp_anom_day_BlueRiv$prcp_anomaly,
#                      Dolores = df_prcp_anom_day_DoloresRiv$prcp_anomaly,
#                      East = df_prcp_anom_day_EastRiv$prcp_anomaly,
#                      Taylor = df_prcp_anom_day_TaylorRiv$prcp_anomaly)
# 
# test_P2 <- test_P %>% 
#   pivot_longer(cols = -c(waterYear),
#                names_to = "Basin",
#                values_to = "prcp_anom")


test_Blue <- data.frame(waterYear = df_prcp_anom_day_BlueRiv$waterYear,
                        prcp_anom = df_prcp_anom_day_BlueRiv$prcp_anomaly,
                        tmax_anom = df_tmax_anom_day_BlueRiv$tmax_anomaly,
                        tmax_allsnow = list_mu_ann_day_BlueRiv$df_mean[1,],
                        tmax_cbh_adj = list_mu_ann_day_BlueRiv$df_mean[2,],
                        rad_trncf = list_mu_ann_day_BlueRiv$df_mean[3,],
                        jh_coef = list_mu_ann_day_BlueRiv$df_mean[4,],
                        Basin = rep("Blue",nrow(df_prcp_anom_day_BlueRiv)))


test_Dolores <- data.frame(waterYear = df_prcp_anom_day_DoloresRiv$waterYear,
                        prcp_anom = df_prcp_anom_day_DoloresRiv$prcp_anomaly,
                        tmax_anom = df_tmax_anom_day_DoloresRiv$tmax_anomaly,
                        tmax_allsnow = list_mu_ann_day_DoloresRiv$df_mean[1,],
                        tmax_cbh_adj = list_mu_ann_day_DoloresRiv$df_mean[2,],
                        rad_trncf = list_mu_ann_day_DoloresRiv$df_mean[3,],
                        jh_coef = list_mu_ann_day_DoloresRiv$df_mean[4,],
                        Basin = rep("Dolores",nrow(df_prcp_anom_day_DoloresRiv)))


test_East <- data.frame(waterYear = df_prcp_anom_day_EastRiv$waterYear,
                        prcp_anom = df_prcp_anom_day_EastRiv$prcp_anomaly,
                        tmax_anom = df_tmax_anom_day_EastRiv$tmax_anomaly,
                        tmax_allsnow = list_mu_ann_day_EastRiv$df_mean[1,],
                        tmax_cbh_adj = list_mu_ann_day_EastRiv$df_mean[2,],
                        rad_trncf = list_mu_ann_day_EastRiv$df_mean[3,],
                        jh_coef = list_mu_ann_day_EastRiv$df_mean[4,],
                        Basin = rep("East",nrow(df_prcp_anom_day_EastRiv)))
# filter the anoms
df_prcp_anom_day_TaylorRiv <- df_prcp_anom_day_TaylorRiv %>%
  filter(waterYear >= 1988)
df_tmax_anom_day_TaylorRiv <- df_tmax_anom_day_TaylorRiv %>%
  filter(waterYear >= 1988)

test_Taylor <- data.frame(waterYear = df_prcp_anom_day_TaylorRiv$waterYear,
                        prcp_anom = df_prcp_anom_day_TaylorRiv$prcp_anomaly,
                        tmax_anom = df_tmax_anom_day_TaylorRiv$tmax_anomaly,
                        tmax_allsnow = list_mu_ann_day_TaylorRiv$df_mean[1,],
                        tmax_cbh_adj = list_mu_ann_day_TaylorRiv$df_mean[2,],
                        rad_trncf = list_mu_ann_day_TaylorRiv$df_mean[3,],
                        jh_coef = list_mu_ann_day_TaylorRiv$df_mean[4,],
                        Basin = rep("Taylor",nrow(df_prcp_anom_day_TaylorRiv)))


df_plot <- rbind(test_Blue,test_Dolores,test_East,test_Taylor)

df_plot <- df_plot %>% 
  pivot_longer(cols = c(tmax_allsnow, tmax_cbh_adj,
                        rad_trncf,jh_coef),
               names_to = "param_name",
               values_to = "mu_star")











# FOR MU* ANOMS

# test_Blue2 <- data.frame(waterYear = df_prcp_anom_day_BlueRiv$waterYear,
#                         prcp_anom = df_prcp_anom_day_BlueRiv$prcp_anomaly,
#                         tmax_anom = df_tmax_anom_day_BlueRiv$tmax_anomaly,
#                         tmax_allsnow = list_mu_anom_day_BlueRiv$df_mean[1,],
#                         tmax_cbh_adj = list_mu_anom_day_BlueRiv$df_mean[2,],
#                         rad_trncf = list_mu_anom_day_BlueRiv$df_mean[3,],
#                         jh_coef = list_mu_anom_day_BlueRiv$df_mean[4,],
#                         Basin = rep("Blue",nrow(df_prcp_anom_day_BlueRiv)))
# 
# 
# test_Dolores2 <- data.frame(waterYear = df_prcp_anom_day_DoloresRiv$waterYear,
#                            prcp_anom = df_prcp_anom_day_DoloresRiv$prcp_anomaly,
#                            tmax_anom = df_tmax_anom_day_DoloresRiv$tmax_anomaly,
#                            tmax_allsnow = list_mu_anom_day_DoloresRiv$df_mean[1,],
#                            tmax_cbh_adj = list_mu_anom_day_DoloresRiv$df_mean[2,],
#                            rad_trncf = list_mu_anom_day_DoloresRiv$df_mean[3,],
#                            jh_coef = list_mu_anom_day_DoloresRiv$df_mean[4,],
#                            Basin = rep("Dolores",nrow(df_prcp_anom_day_DoloresRiv)))
# 
# 
# test_East2 <- data.frame(waterYear = df_prcp_anom_day_EastRiv$waterYear,
#                         prcp_anom = df_prcp_anom_day_EastRiv$prcp_anomaly,
#                         tmax_anom = df_tmax_anom_day_EastRiv$tmax_anomaly,
#                         tmax_allsnow = list_mu_anom_day_EastRiv$df_mean[1,],
#                         tmax_cbh_adj = list_mu_anom_day_EastRiv$df_mean[2,],
#                         rad_trncf = list_mu_anom_day_EastRiv$df_mean[3,],
#                         jh_coef = list_mu_anom_day_EastRiv$df_mean[4,],
#                         Basin = rep("East",nrow(df_prcp_anom_day_EastRiv)))
# # filter the anoms
# df_prcp_anom_day_TaylorRiv <- df_prcp_anom_day_TaylorRiv %>%
#   filter(waterYear >= 1988)
# df_tmax_anom_day_TaylorRiv <- df_tmax_anom_day_TaylorRiv %>%
#   filter(waterYear >= 1988)
# 
# test_Taylor2 <- data.frame(waterYear = df_prcp_anom_day_TaylorRiv$waterYear,
#                           prcp_anom = df_prcp_anom_day_TaylorRiv$prcp_anomaly,
#                           tmax_anom = df_tmax_anom_day_TaylorRiv$tmax_anomaly,
#                           tmax_allsnow = list_mu_anom_day_TaylorRiv$df_mean[1,],
#                           tmax_cbh_adj = list_mu_anom_day_TaylorRiv$df_mean[2,],
#                           rad_trncf = list_mu_anom_day_TaylorRiv$df_mean[3,],
#                           jh_coef = list_mu_anom_day_TaylorRiv$df_mean[4,],
#                           Basin = rep("Taylor",nrow(df_prcp_anom_day_TaylorRiv)))
# 
# 
# df_plot2 <- rbind(test_Blue2,test_Dolores2,test_East2,test_Taylor2)
# 
# df_plot2 <- df_plot2 %>% 
#   pivot_longer(cols = c(tmax_allsnow, tmax_cbh_adj,
#                         rad_trncf,jh_coef),
#                names_to = "param_name",
#                values_to = "mu_star")


# FOR PLOTTING BY ANNUAL FORCING ABS VALUES

test_Blue3 <- data.frame(waterYear = df_prcp_ann_day_BlueRiv$waterYear,
                        prcp_ann = df_prcp_ann_day_BlueRiv$A_weighted_avg,
                        tmax_ann = df_tmax_ann_day_BlueRiv$A_weighted_avg,
                        tmax_allsnow = list_mu_ann_day_BlueRiv$df_mean[1,],
                        tmax_cbh_adj = list_mu_ann_day_BlueRiv$df_mean[2,],
                        rad_trncf = list_mu_ann_day_BlueRiv$df_mean[3,],
                        jh_coef = list_mu_ann_day_BlueRiv$df_mean[4,],
                        Basin = rep("Blue",nrow(df_prcp_anom_day_BlueRiv)))


test_Dolores3 <- data.frame(waterYear = df_prcp_ann_day_DoloresRiv$waterYear,
                         prcp_ann = df_prcp_ann_day_DoloresRiv$A_weighted_avg,
                         tmax_ann = df_tmax_ann_day_DoloresRiv$A_weighted_avg,
                         tmax_allsnow = list_mu_ann_day_DoloresRiv$df_mean[1,],
                         tmax_cbh_adj = list_mu_ann_day_DoloresRiv$df_mean[2,],
                         rad_trncf = list_mu_ann_day_DoloresRiv$df_mean[3,],
                         jh_coef = list_mu_ann_day_DoloresRiv$df_mean[4,],
                         Basin = rep("Dolores",nrow(df_prcp_anom_day_DoloresRiv)))


test_East3 <- data.frame(waterYear = df_prcp_ann_day_EastRiv$waterYear,
                         prcp_ann = df_prcp_ann_day_EastRiv$A_weighted_avg,
                         tmax_ann = df_tmax_ann_day_EastRiv$A_weighted_avg,
                         tmax_allsnow = list_mu_ann_day_EastRiv$df_mean[1,],
                         tmax_cbh_adj = list_mu_ann_day_EastRiv$df_mean[2,],
                         rad_trncf = list_mu_ann_day_EastRiv$df_mean[3,],
                         jh_coef = list_mu_ann_day_EastRiv$df_mean[4,],
                         Basin = rep("East",nrow(df_prcp_anom_day_EastRiv)))
# filter the anoms
df_prcp_ann_day_TaylorRiv <- df_prcp_ann_day_TaylorRiv %>%
  filter(waterYear >= 1988)
df_tmax_ann_day_TaylorRiv <- df_tmax_ann_day_TaylorRiv %>%
  filter(waterYear >= 1988)

test_Taylor3 <- data.frame(waterYear = df_prcp_ann_day_TaylorRiv$waterYear,
                         prcp_ann = df_prcp_ann_day_TaylorRiv$A_weighted_avg,
                         tmax_ann = df_tmax_ann_day_TaylorRiv$A_weighted_avg,
                         tmax_allsnow = list_mu_ann_day_TaylorRiv$df_mean[1,],
                         tmax_cbh_adj = list_mu_ann_day_TaylorRiv$df_mean[2,],
                         rad_trncf = list_mu_ann_day_TaylorRiv$df_mean[3,],
                         jh_coef = list_mu_ann_day_TaylorRiv$df_mean[4,],
                         Basin = rep("Taylor",nrow(df_prcp_anom_day_TaylorRiv)))


df_plot3 <- rbind(test_Blue3,test_Dolores3,test_East3,test_Taylor3)

df_plot3 <- df_plot3 %>% 
  pivot_longer(cols = c(tmax_allsnow, tmax_cbh_adj,
                        rad_trncf,jh_coef),
               names_to = "param_name",
               values_to = "mu_star") %>%
  pivot_longer(cols = c(prcp_ann, tmax_ann),
               names_to = "forcing_name",
               values_to = "forcing_vals")







# ---------------------------------------------------------------------------
# Plotting p & T anomalie scatter
# ---------------------------------------------------------------------------

df_plot$mu_star <- df_plot$mu_star/100 # removing NRMSE as percent
df_plot$log_mu_star <- log10(df_plot$mu_star)

ggplot(df_plot, aes(x = prcp_anom, y = tmax_anom, color = log_mu_star)) +
  geom_point(size = 1) +
  scale_color_viridis_c(option = "plasma") +  # or use "viridis" or other palettes
  facet_grid(rows = vars(param_name), cols = vars(Basin)) +
  theme_minimal(base_size = 9) +
  labs(
    x = "Annual Total Precipitation Anomaly (mm)",
    y = "Annual Average Max. Temperature Anomaly (°C)",
    color = "log(μ*)"#, η*, μ*, log(μ*)
    #title = "Sensitivity log(μ*) across climate anomalies" #μ*
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black")+
  theme_light() +
  theme(
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), # Keeps the plot box
    strip.background = element_rect(fill = "white", color = "white"), # Clean facet strips
    strip.text.x = element_text(color = "black", size = 9, face = "bold"),
    strip.text.y = element_text(color = "black", size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_composite_scatter_logmu2.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)


# ---------------------------------------------------------------------------
# Plotting mu vs absolute forcing  scatter
# ---------------------------------------------------------------------------


forcing_labels <- c(
  "prcp_ann" = "Ann. Tot. Precip. (mm)",
  "tmax_ann" = "Ann. Avg. Tmax. (ºC)"
)

basin_colors <- c(
  "Blue" = "#8da0cb",  # Blue
  "Dolores" = "#fc8d62",  # Orange
  "East" = "#66c2a5",  # Green
  "Taylor" = "#e78ac3"   # Red
)

ggplot(df_plot3, aes(x = forcing_vals, y = mu_star, color = Basin)) +
  geom_point(size = 0.75, alpha = 0.75) +
  scale_color_manual(values = basin_colors) +
  facet_grid(rows = vars(param_name),
             cols = vars(forcing_name),
             scales = "free", #"free_x for logy",
             labeller = labeller(forcing_name = forcing_labels)) +
  #scale_y_log10() +
  theme_minimal(base_size = 9) +
  labs(
    x = "Forcing Values",
    y = "μ* by Parameter",
    color = "Basin"#,
    #title = "Sensitivity (μ*) across forcings" #μ*
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  ) +
  theme_light() +
  theme(
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), # Keeps the plot box
    strip.background = element_rect(fill = "white", color = "white"), # Clean facet strips
    strip.text.x = element_text(color = "black", size = 9, face = "bold"),
    strip.text.y = element_text(color = "black", size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_composite_scatter_byforcing.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)


# ---------------------------------------------------------------------------
# Plotting heatmap mu by forcing var for all params
# ---------------------------------------------------------------------------

# PULL MU* FOR ALL PARAMS
# INPUTS TO LOOP, same as above, omitting param selection
# Adapted from sensi_heat_temporal annual diff and sensi_cor_facet
measures <- c("lower", "mean", "upper") #sensi metric bounds from bootstrap
names_gages <- c("09046600","09166500","09112500","09107000") #gage ID
Q_years_start <- c(1982, 1982, 1982, 1988) # gage starting year

# Create list for difference dataframes
list_mu_annual2 <- list()

for (i in 1:length(test_proj_names)){
  
  name_proj <- test_proj_names[i] #get individual proj name
  name_gage <- names_gages[i] # get gage names
  start_year <- Q_years_start[i] # get starting year
  
  data_Q <- get(paste0("list_data_",name_proj))[["Q_GOF"]][[name_gage]] #extract GOF from list
  data_params <- get(paste0("params_",name_proj)) # extract params
  names_params <- colnames(data_params) # store param names
  
  n_years <- length(data_Q$annual$NRMSE$bootstrap$mu_star) # need for extracting annual data
  
  # print(paste0("i is:",i))
  
  for (j in seq_along(measures)) {
    
    df_name <- paste0("df_", measures[j]) # lower, mean, upper from bootstrap
    
    # Extract the annual data
    # NOTE: Taylor data starts in 1988 rather than 1982 when model starts
    df_mu_annual <- do.call(cbind, lapply(1:n_years, function(k) {
      data_Q$annual$NRMSE$bootstrap$mu_star[[k]][, j]
    })) # annual data
    
    
    # df_mu_annual <- df_mu_annual/100 #converting from NRMSE% to absolute
    colnames(df_mu_annual) <- start_year:(start_year + n_years - 1) # colname by yr
    rownames(df_mu_annual) <- names_params # row names by param
    # df_mu_annual <- df_mu_annual[selected_params, ] # filtering by select
    
    # Store in ljst
    list_mu_annual2[[df_name]] <- df_mu_annual
    
  } #close j
  
  # Assign objects
  assign(paste0("list_mu_annAll_", name_proj), list_mu_annual2, envir = .GlobalEnv)
  
} #close i




# CREATE HELPER FUNCTION TO COMBINE P AND T WITH MU*

# Helper function to generate a plotting dataframe for a basin
create.forcing.long <- function(name_proj, forcing = "prcp", start_year = 1982) {
  
  # Dynamic object names
  df_prcp_ann <- get(paste0("df_prcp_ann_", name_proj))
  df_tmax_ann <- get(paste0("df_tmax_ann_", name_proj))
  df_mu_raw <- get(paste0("list_mu_annAll_", name_proj))$df_mean
  

  if (name_proj == "day_TaylorRiv") {
    df_prcp_ann <- df_prcp_ann %>% filter(waterYear >= 1988)
    df_tmax_ann <- df_tmax_ann %>% filter(waterYear >= 1988)
  }
  
  # Transpose mu_star_df
  df_mu_ann <- as.data.frame(t(df_mu_raw)) %>% 
    mutate(waterYear = as.numeric(rownames(.)))
  
  # Joing mu_star and forcing dataframes
  if (forcing == "prcp") {
    df <- full_join(df_prcp_ann, df_mu_ann, by = "waterYear") %>%
      select(-starts_with("hru_")) %>% #remove hru cols
      rename(forcing_vals = A_weighted_avg) # keep and rename
  } else if (forcing == "tmax") {
    df <- full_join(df_tmax_ann, df_mu_ann, by = "waterYear") %>%
      select(-starts_with("hru_")) %>%
      rename(forcing_vals = A_weighted_avg)
  } else {
    stop("Invalid forcing call. Must be 'prcp' or 'tmax'.")
  }
  
  
  
  param_info <- tibble(
    param_names = rownames(df_mu_raw),
    param_groups = c(
      "Climate","Climate","Climate","Climate","Climate",
      "Climate","Climate",
      "Solar", "Solar", "Solar", "Solar", "Solar",
      "Solar", "Solar",
      "PET", "PET",
      "Intcp.", "Intcp.", "Intcp.","Intcp.", "Intcp.",
      "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
      "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
      "Runoff", "Runoff", "Runoff", "Runoff", "Runoff",
      "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
      "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
      "G"
    )
  )
  
  

  # Pivot longer by params
  df_long <- df %>%
    mutate(Basin = name_proj,
           forcing_type = forcing) %>%
    pivot_longer(cols = c(-waterYear, -forcing_vals, -Basin, -forcing_type),
                 names_to = "param_names",
                 values_to = "mu_star_vals") %>%
    left_join(param_info, by = "param_names") %>%
    mutate(param_names = factor(param_names, levels = rev(param_info$param_names)),
           param_groups = factor(param_groups,
                                 levels = c("Climate", "Solar", "PET", "Snow",
                                            "Intcp.","Runoff", "Soil", "G")))  # Custom order for facets)
  
  return(df_long)
}




# CREATE PLOTTING DFs
# List of basin names (match the object suffixes)
test_proj_names <- c("day_BlueRiv", "day_DoloresRiv",
                     "day_EastRiv", "day_TaylorRiv")

# Build separate dataframes
df_prcp_long <- bind_rows(lapply(test_proj_names, create.forcing.long, forcing = "prcp"))
df_tmax_long <- bind_rows(lapply(test_proj_names, create.forcing.long, forcing = "tmax"))

summary(df_prcp_long)
summary(df_tmax_long)





# TESTING PALETTES
library(pals)
pal.bands(brewer.blues(100))
pals::stepped()

library(RColorBrewer)
display.brewer.all() # Show all palettes

# Stepped color scale FROM OTHER HEATMAP
palette_name <- "YlGnBu"
library(RColorBrewer)
colors_stepped <- brewer.pal(n = 6, name = palette_name)
breaks_stepped <- seq(0, 1, by = 0.2)





# CREATING PLOTS

# df_prcp_long <- df_prcp_long %>%
#   mutate(forcing_bin = cut(forcing_vals, breaks = 30))  # or use pretty breaks
# 
# df_tmax_long <- df_tmax_long %>%
#   mutate(forcing_bin = cut(forcing_vals, breaks = 30))  # or use pretty breaks


# CREATING PRETTY BREAKS
# Reusable bin label formatter — now with decimal support!
format_bin_labels <- function(breaks, digits = 0) {
  lower <- formatC(head(breaks, -1), format = "f", digits = digits)
  upper <- formatC(tail(breaks, -1), format = "f", digits = digits)
  labels <- paste0(lower, "–", upper)
  return(labels)
}

# Breaks
prcp_breaks <- pretty(df_prcp_long$forcing_vals, n = 25)
tmax_breaks <- pretty(df_tmax_long$forcing_vals, n = 12)

# Labels
prcp_labels <- format_bin_labels(prcp_breaks, digits = 0)
tmax_labels <- format_bin_labels(tmax_breaks, digits = 1)

# Apply to dataframes
df_prcp_long <- df_prcp_long %>%
  mutate(
    forcing_bin = cut(forcing_vals, breaks = prcp_breaks, labels = prcp_labels, include.lowest = TRUE)
  )

df_tmax_long <- df_tmax_long %>%
  mutate(
    forcing_bin = cut(forcing_vals, breaks = tmax_breaks, labels = tmax_labels, include.lowest = TRUE)
  )




# PLOTTING
# df_plot3_test$log_mu_star_vals <- log10(df_plot3_test$mu_star_vals)

plot_prcp <- ggplot(df_prcp_long, aes(x = forcing_bin, y = param_names, fill = mu_star_vals)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(rows = vars(param_groups), scales = "free", space = "free") + # Facet by group and observation
  scale_fill_gradientn(
    colors = pals::brewer.blues(9),  # You can use any number between 3–9 for smoothness
    name = "µ*") +
  # scale_fill_viridis_c(option = "plasma") +  # optional scaling
  scale_fill_stepsn(name = "η*", colours = colors_stepped, guide = "coloursteps", breaks = breaks_stepped) +
  theme_minimal(base_size = 9) +
  labs(x = "Ann. Total Precip. for All Basins (mm)",
       y = "Parameter", fill = "µ*") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
        axis.text.y = element_text(vjust = 0.5),# Centered Y-axis text)
        strip.text.y = element_blank(),
        legend.position = "none")  # ✨ Bye, legend!)  


plot_tmax <- ggplot(df_tmax_long, aes(x = forcing_bin, y = param_names, fill = mu_star_vals)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(rows = vars(param_groups), scales = "free", space = "free") + # Facet by group and observation
  scale_fill_gradientn(
    colors = pals::brewer.blues(9),  # You can use any number between 3–9 for smoothness
    name = "µ*") +
  # scale_fill_viridis_c(option = "plasma") +  # optional scaling
  # scale_fill_stepsn(name = "η*", colours = colors_stepped, guide = "coloursteps", breaks = breaks_stepped) +
  theme_minimal(base_size = 9) +
  labs(x = "Ann. Avg. Tmax for All Basins (˚C)",
       y = "Parameter", fill = "µ*") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
        #axis.text.y = element_text(vjust = 0.5),# Centered Y-axis text)
        strip.text.y = element_text(size = 9, face = "bold"),
        axis.title.y = element_blank(),       # no y-axis title
        axis.text.y = element_blank())  


# Combine them
plot_prcp + plot_tmax + plot_layout(ncol = 2)

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_composite_heat_eta2.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)
