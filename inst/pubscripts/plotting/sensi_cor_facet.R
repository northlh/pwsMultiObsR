


# Correlations of sensitvity metrics (eta*) to geographic attributes
# Planned attributes:
# Elevation, slope, aspect, % forested, Annual P, Q/P, P/PET, Temp (DJF,etc)

# Selected parameters: IDK

# Spatial data types: ASO, SCA, SMAP, AET




# ---------------------------------------------------------------------------
# Set trial and read in data and repeat for each basin ----------------------
# EAST ----------------------------------------------------------------------
pws.set.trial(project_name = "day_EastRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_East <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
dims_East <- pws.read.dims("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_EastRiv/default_input/myparam.param")
params_East <- pws.read.params("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_EastRiv/default_input/myparam.param")

elev_East <- params_East$hru_elev$param_value_matrix
aspect_East <- params_East$hru_aspect$param_value_matrix
slope_East <- params_East$hru_slope$param_value_matrix

plot(st_read(file.path(gis_dir,"model_nhru.shp"))[2])



variables <- c("ASO","SMAPsfwt","MOD10A1","openet")
# Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
list_data_East <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
  
  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data_East[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}



# BLUE ----------------------------------------------------------------------
pws.set.trial(project_name = "day_BlueRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Blue <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
dims_Blue <- pws.read.dims("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_BlueRiv/default_input/myparam.param")
params_Blue <- pws.read.params("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_BlueRiv/default_input/myparam.param")

elev_Blue <- params_Blue$hru_elev$param_value_matrix
aspect_Blue <- params_Blue$hru_aspect$param_value_matrix
slope_Blue <- params_Blue$hru_slope$param_value_matrix

plot(st_read(file.path(gis_dir,"model_nhru.shp"))[2])



variables <- c("ASO","SMAPsfwt","MOD10A1","openet")
# Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
list_data_Blue <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
  
  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data_Blue[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}


# DOLO ----------------------------------------------------------------------
pws.set.trial(project_name = "day_DoloresRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Dolores <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
dims_Dolores <- pws.read.dims("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_DoloresRiv/default_input/myparam.param")
params_Dolores <- pws.read.params("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_DoloresRiv/default_input/myparam.param")

elev_Dolores <- params_Dolores$hru_elev$param_value_matrix
aspect_Dolores <- params_Dolores$hru_aspect$param_value_matrix
slope_Dolores <- params_Dolores$hru_slope$param_value_matrix

plot(st_read(file.path(gis_dir,"model_nhru.shp"))[2])
variables <- c("ASO","SMAPsfwt","MOD10A1","openet")
# Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
list_data_Dolores <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
  
  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data_Dolores[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}


# TAYLOR ----------------------------------------------------------------------
pws.set.trial(project_name = "day_TaylorRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Taylor <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
dims_Taylor <- pws.read.dims("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_TaylorRiv/default_input/myparam.param")
params_Taylor <- pws.read.params("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_TaylorRiv/default_input/myparam.param")

elev_Taylor <- params_Taylor$hru_elev$param_value_matrix
aspect_Taylor <- params_Taylor$hru_aspect$param_value_matrix
slope_Taylor <- params_Taylor$hru_slope$param_value_matrix

plot(st_read(file.path(gis_dir,"model_nhru.shp"))[2])
variables <- c("ASO","SMAPsfwt","MOD10A1","openet")
# Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
list_data_Taylor <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
  
  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data_Taylor[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}

# # CONVERT TO FT
# elev_Blue <- elev_Blue*3.281
# elev_Dolores <- elev_Dolores*3.281
# elev_East <- elev_East*3.281
# elev_Taylor <- elev_Taylor*3.281


# # ---------------------------------------------------------------------------
# # EXAMPLE
# # ---------------------------------------------------------------------------
# 
# # Make DFs
# # Facets
# selected_params <- c("adjmix_rain","tmax_allrain_offset","tmax_allsnow","snow_cbh_adj",
#                     "rain_cbh_adj","tmin_cbh_adj","tmax_cbh_adj","dday_intcp")
# attribute_types <- c("Elevation","Slope","Aspect")
# 
# # Categories
# data_names <- c("ASO","MOD10A1","SMAPsfwt","openet")
# basin_names <- c("Blue", "Dolores")
# 
# 
# 
# 
# df_Blue_ASO <- data.frame(
#   hru_1 = list_data_Blue$ASO$MAE_byhru$mu.star[[1]],
#   hru_2 = list_data_Blue$ASO$MAE_byhru$mu.star[[2]],
#   hru_3 = list_data_Blue$ASO$MAE_byhru$mu.star[[3]],
#   hru_4 = list_data_Blue$ASO$MAE_byhru$mu.star[[4]],
#   hru_5 = list_data_Blue$ASO$MAE_byhru$mu.star[[5]],
#   hru_6 = list_data_Blue$ASO$MAE_byhru$mu.star[[6]]
#   )
# 
# rownames(df_Blue_ASO) <- colnames(morris_design_Blue$X)
# 
# # Filter to selected parameter facet
# df_Blue_ASO <- df_Blue_ASO[selected_params,] # filter to selected params
# df_Blue_ASO <- as.data.frame(t(df_Blue_ASO)) # transpose
# # Add attribute facets types
# df_Blue_ASO$Elevation <- elev_Blue
# df_Blue_ASO$Aspect <- aspect_Blue
# df_Blue_ASO$Slope <- slope_Blue
# # Add categorical vars
# df_Blue_ASO$basin_name <- "Blue"
# df_Blue_ASO$data_name <- "ASO" 
# 
# 
# 
# 
# df_Blue_ASO <- data.frame(
# 
#   ASO = list_data_Blue$ASO$MAE_byhru$mu.star,
#   SMAP = list_data$SMAPsfwt$NRMSE_bybasin$mu.star,
#   SCA = list_data$MOD10A1$NRMSE_bybasin$mu.star,
#   AET = list_data$openet$NRMSE_bybasin$mu.star
# )
# colnames(morris_design_East$X)




# ---------------------------------------------------------------------------
# EXTRACT CLIMATE VARS 
# ---------------------------------------------------------------------------


source(file.path(here(),"source/data_process/cbh_process/pws.read.cbh.R"))

# EAST
# prcp
cbh_prcp_East <- pws.read.cbh(file.path(here(),"Projects/day_EastRiv/default_input/prcp.cbh"))
cbh_prcp_East <- addWaterYear(cbh_prcp_East)
prcp_annAvg_East <- cbh_prcp_East %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant years
  group_by(waterYear) %>%  # Group by water year
  summarise(across(starts_with("hru_"), \(x) sum(x, na.rm = TRUE))) %>%  # Compute annual total
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute average of annual totals

# Tmax
cbh_tmax_East <- pws.read.cbh(file.path(here(),"Projects/day_EastRiv/default_input/tmax.cbh"))
cbh_tmax_East <- addWaterYear(cbh_tmax_East)

tmax_seasAvg_East <- cbh_tmax_East %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years



# tmin
cbh_tmin_East <- pws.read.cbh(file.path(here(),"Projects/day_EastRiv/default_input/tmin.cbh"))
cbh_tmin_East <- addWaterYear(cbh_tmin_East)

tmin_seasAvg_East <- cbh_tmin_East %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years





# BLUE
# prcp
cbh_prcp_Blue <- pws.read.cbh(file.path(here(),"Projects/day_BlueRiv/default_input/prcp.cbh"))
cbh_prcp_Blue <- addWaterYear(cbh_prcp_Blue)
prcp_annAvg_Blue <- cbh_prcp_Blue %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant years
  group_by(waterYear) %>%  # Group by water year
  summarise(across(starts_with("hru_"), \(x) sum(x, na.rm = TRUE))) %>%  # Compute annual total
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute average of annual totals

# Tmax
cbh_tmax_Blue <- pws.read.cbh(file.path(here(),"Projects/day_BlueRiv/default_input/tmax.cbh"))
cbh_tmax_Blue <- addWaterYear(cbh_tmax_Blue)

tmax_seasAvg_Blue <- cbh_tmax_Blue %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years



# tmin
cbh_tmin_Blue <- pws.read.cbh(file.path(here(),"Projects/day_BlueRiv/default_input/tmin.cbh"))
cbh_tmin_Blue <- addWaterYear(cbh_tmin_Blue)

tmin_seasAvg_Blue <- cbh_tmin_Blue %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years





# DOLORES
# prcp
cbh_prcp_Dolores <- pws.read.cbh(file.path(here(),"Projects/day_DoloresRiv/default_input/prcp.cbh"))
cbh_prcp_Dolores <- addWaterYear(cbh_prcp_Dolores)
prcp_annAvg_Dolores <- cbh_prcp_Dolores %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant years
  group_by(waterYear) %>%  # Group by water year
  summarise(across(starts_with("hru_"), \(x) sum(x, na.rm = TRUE))) %>%  # Compute annual total
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute average of annual totals

# Tmax
cbh_tmax_Dolores <- pws.read.cbh(file.path(here(),"Projects/day_DoloresRiv/default_input/tmax.cbh"))
cbh_tmax_Dolores <- addWaterYear(cbh_tmax_Dolores)

tmax_seasAvg_Dolores <- cbh_tmax_Dolores %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years





# TAYLOR
# prcp
cbh_prcp_Taylor <- pws.read.cbh(file.path(here(),"Projects/day_TaylorRiv/default_input/prcp.cbh"))
cbh_prcp_Taylor <- addWaterYear(cbh_prcp_Taylor)
prcp_annAvg_Taylor <- cbh_prcp_Taylor %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant years
  group_by(waterYear) %>%  # Group by water year
  summarise(across(starts_with("hru_"), \(x) sum(x, na.rm = TRUE))) %>%  # Compute annual total
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute average of annual totals

# Tmax
cbh_tmax_Taylor <- pws.read.cbh(file.path(here(),"Projects/day_TaylorRiv/default_input/tmax.cbh"))
cbh_tmax_Taylor <- addWaterYear(cbh_tmax_Taylor)

tmax_seasAvg_Taylor <- cbh_tmax_Taylor %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years

# tmin
cbh_tmin_Taylor <- pws.read.cbh(file.path(here(),"Projects/day_TaylorRiv/default_input/tmin.cbh"))
cbh_tmin_Taylor <- addWaterYear(cbh_tmin_Taylor)

tmin_seasAvg_Taylor <- cbh_tmin_Taylor %>%
  filter(waterYear >= 2013 & waterYear <= 2022) %>%  # Filter relevant water years
  mutate(
    month = month(Date),  # Extract month from Date
    season = case_when(
      month %in% c(12, 1, 2)  ~ "DJF",  # Winter (Dec-Jan-Feb)
      month %in% c(3, 4, 5)   ~ "MAM",  # Spring (Mar-Apr-May)
      month %in% c(6, 7, 8)   ~ "JJA",  # Summer (Jun-Jul-Aug)
      month %in% c(9, 10, 11) ~ "SON"   # Fall (Sep-Oct-Nov)
    )
  ) %>%
  group_by(waterYear, season) %>%  # Group by water year and season
  summarise(across(starts_with("hru_"), \(x) mean(x, na.rm = TRUE))) %>%  # Compute seasonal means
  group_by(season) %>%  # Now group by season
  summarise(across(starts_with("hru_"), mean, na.rm = TRUE))  # Compute mean across years




# ---------------------------------------------------------------------------
# PLOTTING
# ---------------------------------------------------------------------------


library(dplyr)
library(tidyr)
library(purrr)

# Facets

selected_params <-  c("tmax_allsnow", "tmax_cbh_adj",
                      #"snow_cbh_adj",
                      #"dday_intcp", "den_max",
                      "rad_trncf",
                      "jh_coef"#,"soil_moist_max",
                      #"gwflow_coef"
                      )
#attribute_types <- c("Elevation", "Slope", "Aspect")

# Categories
data_names <- c("ASO", "MOD10A1",
                "SMAPsfwt", "openet"
                )
basin_names <- c("Blue", "Dolores","East", "Taylor")

# Placeholder for all data
all_data_long <- list()

# Loop through each basin and data name
for (basin in basin_names) {
  for (data_name in data_names) {
    
    # Extract the correct metric based on the data type
    # METHODS NOTE: MAE is used for ASO due to limited timeseries (2 dates)
    metric_type <- ifelse(data_name == "ASO", "MAE_byhru", "NRMSE_byhru")
    #metric_type <- ifelse(data_name == "ASO", "NRMSE_byhru", "NRMSE_byhru")
    
    # Extract the list for the current basin and data type
    basin_data <- get(paste0("list_data_", basin))[[data_name]]
    hru_count <- length(basin_data[[metric_type]]$bootstrap$mu_star)  # Number of HRUs varies
    
    print(hru_count)
    
    # Dynamically construct the dataframe for sensitivity indices
    df <- as.data.frame(
      sapply(1:hru_count, function(i) basin_data[[metric_type]]$bootstrap$mu_star[[i]][,2])
    )
    
    # Set parameter names as rownames
    # rownames(df) <- colnames(get(paste0("morris_design_", basin))$X)
    rownames(df) <- colnames(get(paste0("morris_design_Taylor"))$X)
    
    # Filter to selected parameters
    df <- df[selected_params, ]
    
    # Transpose the dataframe
    df <- as.data.frame(t(df))
    
    # Add attribute columns (Elevation, Slope, Aspect) for each HRU
    df$Elevation <- get(paste0("elev_", basin))[1:hru_count]
   # df$Aspect <- get(paste0("aspect_", basin))[1:hru_count]
   # df$Slope <- get(paste0("slope_", basin))[1:hru_count]
    
    df$AnnPrecip <- t(get(paste0("prcp_annAvg_",basin))[,1:hru_count])
    df$AnnPrecip <- df$AnnPrecip*25.4 # CONVERT TO MM
    
    df$Tmax_DJF <- t(get(paste0("tmax_seasAvg_",basin))[1,2:(hru_count+1)])
    fahrenheit_to_celsius <- function(F) {
      C <- (F - 32) * 5 / 9
      return(C)
    }
    df$Tmax_DJF <- fahrenheit_to_celsius(df$Tmax_DJF)
    # df$Tmax_MAM <- t(get(paste0("tmax_seasAvg_",basin))[2,2:(hru_count+1)])
    # df$Tmax_JJA <- t(get(paste0("tmax_seasAvg_",basin))[3,2:(hru_count+1)])
    # df$Tmax_SON <- t(get(paste0("tmax_seasAvg_",basin))[4,2:(hru_count+1)])
    
    #df$Tmin_DJF <- t(get(paste0("tmin_seasAvg_",basin))[1,2:(hru_count+1)])
    #df$Tmin_MAM <- t(get(paste0("tmin_seasAvg_",basin))[2,2:(hru_count+1)])
    #df$Tmin_JJA <- t(get(paste0("tmin_seasAvg_",basin))[3,2:(hru_count+1)])
    #df$Tmin_SON <- t(get(paste0("tmin_seasAvg_",basin))[4,2:(hru_count+1)])
    
    
    
    # Add categorical columns
    df$basin_name <- basin
    df$data_name <- data_name
    df$metric_used <- metric_type  # Track which metric was used
    df$hru <- 1:hru_count  # Add HRU column to track HRU numbers
    
    # Reshape to long format
    df_long <- df %>%
      pivot_longer(
        cols = -c(Elevation,
                  #Aspect, Slope,
                  AnnPrecip,
                  Tmax_DJF,
                  #Tmax_MAM, Tmax_JJA, Tmax_SON,
                  #Tmin_DJF, Tmin_MAM, Tmin_JJA, Tmin_SON,
                  #Tmin_DJF, Tmin_JJA,
                  basin_name, data_name, metric_used, hru),
        names_to = "parameter_type",
        values_to = "sensitivity_index"
      ) %>%
      pivot_longer(
        cols = c(Elevation,
                 #Aspect, Slope,
                 AnnPrecip,
                 Tmax_DJF
                 #Tmax_MAM, Tmax_JJA, Tmax_SON,
                 #Tmin_DJF, Tmin_MAM, Tmin_JJA, Tmin_SON
                # Tmin_DJF, Tmin_JJA
                 ),
        names_to = "attribute_type",
        values_to = "attribute_value"
      )
    
    # Append to the list
    all_data_long[[paste(basin, data_name, sep = "_")]] <- df_long
  }
}

# Combine all data into one dataframe
final_data <- bind_rows(all_data_long)

# Preview the structure of the final dataframe
head(final_data)

# Calculate Spearman rank correlations for each facet
correlations <- final_data %>%
  group_by(attribute_type, parameter_type) %>%
  summarize(
    spearman_corr = cor(attribute_value, sensitivity_index, method = "spearman"),
    p_value = cor.test(attribute_value, sensitivity_index, method = "spearman",
                       exact = FALSE)$p.value,
    .groups = "drop"
  )

# Merge the correlation back into the data for easy access if needed
final_data_with_corr <- final_data %>%
  left_join(correlations, by = c("attribute_type", "parameter_type"))

# Preview the correlation results
head(correlations)


# # OLD PLOTS
#
# ggplot(final_data_with_corr, aes(x = attribute_value, y = sensitivity_index, 
#                                  color = basin_name, shape = data_name)) +
#   geom_point(size = 3, alpha = 0.7) +
#   facet_grid(rows = vars(parameter_type), cols = vars(attribute_type)) +
#   geom_text(data = correlations, aes(label = round(spearman_corr, 2)),
#             x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, inherit.aes = FALSE) +
#   theme_minimal() +
#   labs(
#     x = "Attribute Value",
#     y = "Sensitivity Index",
#     color = "Basin",
#     shape = "Data Type"
#   )
# 
# 
# ggplot(final_data_with_corr, aes(x = attribute_value, y = sensitivity_index, 
#                                  color = basin_name, shape = data_name)) +
#   geom_point(size = 3, alpha = 0.7) +
#   facet_grid(rows = vars(parameter_type), cols = vars(attribute_type), 
#              scales = "free_x", labeller = label_both) +
#   geom_text(data = correlations, aes(label = round(spearman_corr, 2)),
#             x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, inherit.aes = FALSE) +
#   theme_light() +
#   labs(
#     x = "Attribute Value",
#     y = "Sensitivity Index",
#     color = "Basin",
#     shape = "Data Type",
#     title = "Sensitivity vs. Attribute (Metric: MAE for ASO, NRMSE for Others)"
#   )
# 
# ggplot(final_data_with_corr, aes(x = attribute_value, y = sensitivity_index, 
#                                  color = basin_name, shape = data_name)) +
#   geom_point(size = 3, alpha = 0.7) +
#   facet_grid(rows = vars(parameter_type), cols = vars(attribute_type), 
#              scales = "free", labeller = label_both) +  # Allow both x and y to vary
#   geom_text(data = correlations, aes(label = round(spearman_corr, 2)),
#             x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, inherit.aes = FALSE) +
#   theme_light() +
#   labs(
#     x = "Attribute Value",
#     y = "Sensitivity Index",
#     color = "Basin",
#     shape = "Data Type",
#     title = "Sensitivity vs. Attribute (Metric: MAE for ASO, NRMSE for Others)"
#   )

# final_data_with_corr$parameter_type <- factor(final_data_with_corr$parameter_type,
#                                               levels = c("AnnPrecip", "Elevation", "Tmax_DJF"),
#                                               labels = c("Annual Avg. Precip. (in)", 
#                                                          "Elevation (ft)", 
#                                                          "Tmax DJF (ºF)"))

# Define custom labels
attribute_labels <- c(
  "AnnPrecip" = "Ann. Avg. Precip. (mm)",
  "Elevation" = "Elevation (m)",
  "Tmax_DJF" = "Tmax DJF (ºC)"
)

# # Define custom colors for each basin
# basin_colors <- c(
#   "Blue" = "#0F8299",  # Blue
#   "Dolores" = "#99600F",  # Orange
#   "East" = "#54990F",  # Green
#   "Taylor" = "#3D0F99"   # Red
# )

# This is from one of the brewer palettes...
# https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=4
basin_colors <- c(
  "Blue" = "#8da0cb",  # Blue
  "Dolores" = "#fc8d62",  # Orange
  "East" = "#66c2a5",  # Green
  "Taylor" = "#e78ac3"   # Red
)

# Removing NRMSE as percent (from HydroGOF), so metric in units of Std dev
final_data_with_corr$sensitivity_index <- final_data_with_corr$sensitivity_index/100

# CREATE THE PLOT
 ggplot(final_data_with_corr, aes(x = attribute_value, y = sensitivity_index, 
                                 color = basin_name, shape = data_name)) +
  geom_point(size = 1.5, alpha = 0.75) +
  facet_grid(rows = vars(parameter_type), cols = vars(attribute_type), 
             scales = "free_x",
             labeller = labeller(attribute_type = attribute_labels)) +  # Allow both x and y to vary
  geom_text(data = correlations,
            aes(label = paste0("R = ", round(spearman_corr, 2), 
                               "\np = ", formatC(p_value, format = "e", digits = 1))),
            x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, 
            inherit.aes = FALSE, size = 3) +
  scale_y_log10() +  # Apply log scale to the y-axis
  scale_color_manual(values = basin_colors) +  # Apply custom colors
  theme_bw(base_size = 9) +
  labs(
    x = "Attribute Value",
    y = "μ*",
    color = "Catchment",
    shape = "Observation"#,
    #title = "Sensitivity vs. Attribute (Metric: MAE for ASO, NRMSE for Others)"
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
 # theme(
 #   strip.text.x = element_text(size = 9, face = "bold"),
 #   strip.text.y = element_text(size = 9, face = "bold")
 # )

 ggsave(
   filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_cor_day3.png"),   # Save as PDF
   plot = last_plot(),                       # Replace with your plot object
   width = 6.5, height = 4.5, units = "in", dpi = 300
 )




