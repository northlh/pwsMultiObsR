# ---------------------------------------------------------------------------
# LOAD LIST DATA OF GOF STATS
# ---------------------------------------------------------------------------

# Can be used in a loop for many projects or trials
test_proj_names <- c("day_BlueRiv", "day_DoloresRiv",
                     "day_EastRiv", "day_TaylorRiv")
test_trial_nums <- c(2,2,2,2)

variables <- c("Q","SWE","ASO","MOD10A1","SMS2", "SMAPsfwt","openet")

for (i in 1:length(test_proj_names)){
  
  pws.read.lists(project_name = test_proj_names[i],
                 trial_number = test_trial_nums[i],
                 var_names = variables,
                 metrics = "GOF",
                 list_type = "EET")
  
}






# Set environment ---------------------------------------------
source("source/run_process/pws.set.trial.R")

# Set trial and read in data and repeat for each basin ----------------------

pws.set.trial(project_name = "day_EastRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_East <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))

variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
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


pws.set.trial(project_name = "day_BlueRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Blue <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))

variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
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



pws.set.trial(project_name = "day_DoloresRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Dolores <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))

variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
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



pws.set.trial(project_name = "day_TaylorRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Taylor <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))

variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
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



# Create dataframe with strategically named columns for grouping -------------
list_eta_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])

  list_eta_dfs[[df_name]] <- data.frame <- data.frame(
    Q_East_2200 = list_data_East$Q$`09112200`$total$NRMSE$bootstrap$eta_star[,i],
    Q_East_2500 = list_data_East$Q$`09112500`$total$NRMSE$bootstrap$eta_star[,i],
    Q_Blue_6490 = list_data_Blue$Q$`09046490`$total$NRMSE$bootstrap$eta_star[,i],
    Q_Blue_6600 = list_data_Blue$Q$`09046600`$total$NRMSE$bootstrap$eta_star[,i],
    Q_Dolores_5000 = list_data_Dolores$Q$`09165000`$total$NRMSE$bootstrap$eta_star[,i],
    Q_Dolores_6500 = list_data_Dolores$Q$`09166500`$total$NRMSE$bootstrap$eta_star[,i],
    Q_Taylor_7000 = list_data_Taylor$Q$`09107000`$total$NRMSE$bootstrap$eta_star[,i],
  
    SNOTEL_East_380 = list_data_East$SWE$CO$`380`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_East_737 = list_data_East$SWE$CO$`737`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Blue_531 = list_data_Blue$SWE$CO$`531`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Dolores_465 = list_data_Dolores$SWE$CO$`465`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Dolores_586 = list_data_Dolores$SWE$CO$`586`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Dolores_739 = list_data_Dolores$SWE$CO$`739`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Dolores_1060 = list_data_Dolores$SWE$CO$`1060`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Dolores_1185 = list_data_Dolores$SWE$CO$`1185`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_Taylor_1141 = list_data_Taylor$SWE$CO$`1141`$NRMSE$bootstrap$eta_star[,i],
  
    ASO_East_220421 = list_data_East$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_East_220518 = list_data_East$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    ASO_Blue_210418 = list_data_Blue$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_Blue_210524 = list_data_Blue$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    ASO_Blue_220419 = list_data_Blue$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[3]][,i],
    ASO_Blue_220526 = list_data_Blue$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,i],
    ASO_Dolores_210420 = list_data_Dolores$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_Dolores_210514 = list_data_Dolores$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    ASO_Dolores_220415 = list_data_Dolores$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[3]][,i],
    ASO_Dolores_220510 = list_data_Dolores$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,i],
    ASO_Taylor_220421 = list_data_Taylor$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_Taylor_220525 = list_data_Taylor$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    # ASO_East = list_data_East$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    # ASO_Blue = list_data_Blue$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    # ASO_Dolores = list_data_Dolores$ASO$MAE_bybasin$bootstrap$eta_star[,i],
  
    SCA_East = list_data_East$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA_Blue = list_data_Blue$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA_Dolores = list_data_Dolores$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA_Taylor = list_data_Dolores$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
  
    SMS2_East_380 = list_data_East$SMS2$CO$`380`$NRMSE$bootstrap$eta_star[,i],
    SMS2_East_737 = list_data_East$SMS2$CO$`737`$NRMSE$bootstrap$eta_star[,i],
    SMS2_Blue_531 = list_data_Blue$SMS2$CO$`531`$NRMSE$bootstrap$eta_star[,i],
    #SMS2_Dolores_586 = list_data_Dolores$SMS2$CO$`586`$NRMSE$bootstrap$eta_star[,i],
    SMS2_Dolores_1060 = list_data_Dolores$SMS2$CO$`1060`$NRMSE$bootstrap$eta_star[,i],
    SMS2_Dolores_1185 = list_data_Dolores$SMS2$CO$`1185`$NRMSE$bootstrap$eta_star[,i],
    SMS2_Taylor_1141 = list_data_Taylor$SMS2$CO$`1141`$NRMSE$bootstrap$eta_star[,i],
  
    SMAP_East = list_data_East$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SMAP_Blue = list_data_Blue$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SMAP_Dolores = list_data_Dolores$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SMAP_Taylor = list_data_Taylor$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
  
    AET_East = list_data_East$openet$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET_Blue = list_data_Blue$openet$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET_Dolores = list_data_Dolores$openet$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET_Taylor = list_data_Taylor$openet$NRMSE_bybasin$bootstrap$eta_star[,i]
  )
  
  rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design_Taylor$X) #same
}  


# list_nu_dfs <- list()
# 
# for(i in seq_along(measures)){
# 
#   df_name <- paste0("df_",measures[i])
# 
#   list_nu_dfs[[df_name]] <- data.frame <- data.frame(
#     Q_East_2200 = list_data_East$Q$`09112200`$total$NRMSE$bootstrap$nu_star[,i],
#     Q_East_2500 = list_data_East$Q$`09112500`$total$NRMSE$bootstrap$nu_star[,i],
#     Q_Blue_6490 = list_data_Blue$Q$`09046490`$total$NRMSE$bootstrap$nu_star[,i],
#     Q_Blue_6600 = list_data_Blue$Q$`09046600`$total$NRMSE$bootstrap$nu_star[,i],
#     Q_Dolores_5000 = list_data_Dolores$Q$`09165000`$total$NRMSE$bootstrap$nu_star[,i],
#     Q_Dolores_6500 = list_data_Dolores$Q$`09166500`$total$NRMSE$bootstrap$nu_star[,i],
# 
#     SNOTEL_East_380 = list_data_East$SWE$CO$`380`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_East_737 = list_data_East$SWE$CO$`737`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Blue_531 = list_data_Blue$SWE$CO$`531`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Dolores_465 = list_data_Dolores$SWE$CO$`465`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Dolores_586 = list_data_Dolores$SWE$CO$`586`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Dolores_739 = list_data_Dolores$SWE$CO$`739`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Dolores_1060 = list_data_Dolores$SWE$CO$`1060`$NRMSE$bootstrap$nu_star[,i],
#     SNOTEL_Dolores_1185 = list_data_Dolores$SWE$CO$`1185`$NRMSE$bootstrap$nu_star[,i],
# 
#     # WILL NEED TO SWITCH THIS NRMSE IN THE FUTURE, WILL IT EFFECT THE ID? Yes...
#     ASO_East = list_data_East$ASO$MAE_bybasin$bootstrap$nu_star[,i],
#     ASO_Blue = list_data_Blue$ASO$MAE_bybasin$bootstrap$nu_star[,i],
#     ASO_Dolores = list_data_Dolores$ASO$MAE_bybasin$bootstrap$nu_star[,i],
# 
#     SCA_East = list_data_East$MOD10A1$NRMSE_bybasin$bootstrap$nu_star[,i],
#     SCA_Blue = list_data_Blue$MOD10A1$NRMSE_bybasin$bootstrap$nu_star[,i],
#     SCA_Dolores = list_data_Dolores$MOD10A1$NRMSE_bybasin$bootstrap$nu_star[,i],
# 
#     SMS2_East_380 = list_data_East$SMS2$CO$`380`$NRMSE$bootstrap$nu_star[,i],
#     SMS2_East_737 = list_data_East$SMS2$CO$`737`$NRMSE$bootstrap$nu_star[,i],
#     SMS2_Blue_531 = list_data_Blue$SMS2$CO$`531`$NRMSE$bootstrap$nu_star[,i],
#     SMS2_Dolores_586 = list_data_Dolores$SMS2$CO$`586`$NRMSE$bootstrap$nu_star[,i],
#     SMS2_Dolores_1060 = list_data_Dolores$SMS2$CO$`1060`$NRMSE$bootstrap$nu_star[,i],
#     SMS2_Dolores_1185 = list_data_Dolores$SMS2$CO$`1185`$NRMSE$bootstrap$nu_star[,i],
# 
#     SMAP_East = list_data_East$SMAPsfwt$NRMSE_bybasin$bootstrap$nu_star[,i],
#     SMAP_Blue = list_data_Blue$SMAPsfwt$NRMSE_bybasin$bootstrap$nu_star[,i],
#     SMAP_Dolores = list_data_Dolores$SMAPsfwt$NRMSE_bybasin$bootstrap$nu_star[,i],
# 
#     AET_East = list_data_East$openet$NRMSE_bybasin$bootstrap$nu_star[,i],
#     AET_Blue = list_data_Blue$openet$NRMSE_bybasin$bootstrap$nu_star[,i],
#     AET_Dolores = list_data_Dolores$openet$NRMSE_bybasin$bootstrap$nu_star[,i]
#   )
# }
# 
# nu_df <- list_nu_dfs$df_mean
# summary(nu_df)



# #KGE
# for(i in seq_along(measures)){
#   
#   df_name <- paste0("df_",measures[i])
#   
#   list_eta_dfs[[df_name]] <- data.frame <- data.frame(
#     Q_East_2200 = list_data_East$Q$`09112200`$total$KGE$bootstrap$eta_star[,i],
#     Q_East_2500 = list_data_East$Q$`09112500`$total$KGE$bootstrap$eta_star[,i],
#     Q_Blue_6490 = list_data_Blue$Q$`09046490`$total$KGE$bootstrap$eta_star[,i],
#     Q_Blue_6600 = list_data_Blue$Q$`09046600`$total$KGE$bootstrap$eta_star[,i],
#     Q_Dolores_5000 = list_data_Dolores$Q$`09165000`$total$KGE$bootstrap$eta_star[,i],
#     Q_Dolores_6500 = list_data_Dolores$Q$`09166500`$total$KGE$bootstrap$eta_star[,i],
#     
#     SNOTEL_East_380 = list_data_East$SWE$CO$`380`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_East_737 = list_data_East$SWE$CO$`737`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Blue_531 = list_data_Blue$SWE$CO$`531`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Dolores_465 = list_data_Dolores$SWE$CO$`465`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Dolores_586 = list_data_Dolores$SWE$CO$`586`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Dolores_739 = list_data_Dolores$SWE$CO$`739`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Dolores_1060 = list_data_Dolores$SWE$CO$`1060`$KGE$bootstrap$eta_star[,i],
#     SNOTEL_Dolores_1185 = list_data_Dolores$SWE$CO$`1185`$KGE$bootstrap$eta_star[,i],
#     
#     # WILL NEED TO SWITCH THIS KGE IN THE FUTURE, WILL IT EFFECT THE ID? Yes...
#     ASO_East = list_data_East$ASO$MAE_bybasin$bootstrap$eta_star[,i],
#     ASO_Blue = list_data_Blue$ASO$MAE_bybasin$bootstrap$eta_star[,i],
#     ASO_Dolores = list_data_Dolores$ASO$MAE_bybasin$bootstrap$eta_star[,i],
#     
#     SCA_East = list_data_East$MOD10A1$KGE_bybasin$bootstrap$eta_star[,i],
#     SCA_Blue = list_data_Blue$MOD10A1$KGE_bybasin$bootstrap$eta_star[,i],
#     SCA_Dolores = list_data_Dolores$MOD10A1$KGE_bybasin$bootstrap$eta_star[,i],
#     
#     SMS2_East_380 = list_data_East$SMS2$CO$`380`$KGE$bootstrap$eta_star[,i],
#     SMS2_East_737 = list_data_East$SMS2$CO$`737`$KGE$bootstrap$eta_star[,i],
#     SMS2_Blue_531 = list_data_Blue$SMS2$CO$`531`$KGE$bootstrap$eta_star[,i],
#     SMS2_Dolores_586 = list_data_Dolores$SMS2$CO$`586`$KGE$bootstrap$eta_star[,i],
#     SMS2_Dolores_1060 = list_data_Dolores$SMS2$CO$`1060`$KGE$bootstrap$eta_star[,i],
#     SMS2_Dolores_1185 = list_data_Dolores$SMS2$CO$`1185`$KGE$bootstrap$eta_star[,i],
#     
#     SMAP_East = list_data_East$SMAPsfwt$KGE_bybasin$bootstrap$eta_star[,i],
#     SMAP_Blue = list_data_Blue$SMAPsfwt$KGE_bybasin$bootstrap$eta_star[,i],
#     SMAP_Dolores = list_data_Dolores$SMAPsfwt$KGE_bybasin$bootstrap$eta_star[,i],
#     
#     AET_East = list_data_East$openet$KGE_bybasin$bootstrap$eta_star[,i],
#     AET_Blue = list_data_Blue$openet$KGE_bybasin$bootstrap$eta_star[,i],
#     AET_Dolores = list_data_Dolores$openet$KGE_bybasin$bootstrap$eta_star[,i]
#   )
# }  




# TESTTTTTTT
df_eta <- list_eta_dfs$df_mean


# To view thresholds
pws.sensi.plot.logis(df_mean = list_eta_dfs$df_mean,
                     df_lower = list_eta_dfs$df_lower,
                     df_upper = list_eta_dfs$df_upper,
                     ncol_plot = 8,
                     plot_dir = plot_dir,
                     plot_filename = "plot_sensi_logis_testall_NRMSE.png")

# # Replace with NA if below threshold
# 
# for (i in 1:ncol(df_eta)){
#   
#   threshold <- sensi_thresholds[[i]]
#   
#   # For each column, replace with NA if values below threshold
#   df_eta[,i] <- ifelse(df_eta[, i] < threshold, NA, df_eta[, i])
#   
#   
# }


# The same param_names vector was used to create the SA runs!!!
df_eta$param_names <- colnames(morris_design_Taylor$X) #convert to factor
df_eta <- df_eta %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_eta$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
                        "Climate","Climate",
                        "Solar", "Solar", "Solar", "Solar", "Solar",
                        "Solar", "Solar",
                        "PET", "PET",
                        "Intcp.", "Intcp.", "Intcp.", "Intcp.", "Intcp.",
                        "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
                        "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
                        "Runoff", "Runoff", "Runoff", "Runoff", "Runoff",
                        "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
                        "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
                        "G")

# Pivot the dataframe
df_eta_long <- df_eta %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Column_Name",
    values_to = "eta.star"
  )

# Create labels for errors using output from pws.sensi.plot logis run above
error_data <- df_eta_long %>%
  rowwise() %>%
  mutate(
    error_marker = case_when(
      param_names %in% (type1_param_list[[Column_Name]] %||% character(0)) ~ "■",
      param_names %in% (type2_param_list[[Column_Name]] %||% character(0)) ~ "▲",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  select(Column_Name, param_names, error_marker)  # Keep only relevant columns



# Split the column names into components
df_eta_long <- df_eta_long %>%
  separate(
    Column_Name,
    into = c("Observation", "Basin", "Site"),
    sep = "_",
    fill = "right" # This handles cases where "Site" is missing
  ) %>%
  # Handle missing site numbers (if needed)
  mutate(Site = ifelse(is.na(Site), "", Site)) %>%
  # Create a new column for display-friendly x-axis labels
  mutate(Basin_Site = ifelse(Site == "", Basin, paste(Basin, Site, sep = "_")))

# Replace underscores with spaces in the x-axis labels
df_eta_long <- df_eta_long %>%
  mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "



# ERROR DATA - REPEAT THE SAME STEPS SO THE ERRORS ARE ALIGNED
# Split the column names into components
error_data <- error_data %>%
  separate(
    Column_Name,
    into = c("Observation", "Basin", "Site"),
    sep = "_",
    fill = "right" # This handles cases where "Site" is missing
  ) %>%
  # Handle missing site numbers (if needed)
  mutate(Site = ifelse(is.na(Site), "", Site)) %>%
  # Create a new column for display-friendly x-axis labels
  mutate(Basin_Site = ifelse(Site == "", Basin, paste(Basin, Site, sep = "_")))

# Replace underscores with spaces in the x-axis labels
error_data <- error_data %>%
  mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "



# JOIN THE ERROR COLUMN
# Merge error markers back into the transformed dataframe
df_eta_long <- df_eta_long %>%
  left_join(error_data, by = c("param_names", "Observation", "Basin", "Site","Basin_Site"))

#df_eta_long$error_marker <- error_data$error_marker # WORKS JUST THE SAME :/



# Define custom order for facets
df_eta_long <- df_eta_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                    "Snow","Intcp.",
                                                     "Runoff","Soil", "G")),  # Custom order for vertical facets
    Observation = factor(Observation, levels = c("Q", "SNOTEL",
                                                 "ASO", "SCA",
                                                 "SMS2","SMAP",
                                                 "AET")) # Custom order for horizontal facets
  )


# PRIOR TO ADDING ERROR MARKERS

# # Pivot the dataframe
# df_eta_long <- df_eta %>%
#   pivot_longer(
#     cols = -c(param_names, param_groups),
#     names_to = "Column_Name",
#     values_to = "eta.star"
#   ) %>%
#   # Split the column names into components
#   separate(
#     Column_Name,
#     into = c("Observation", "Basin", "Site"),
#     sep = "_",
#     fill = "right" # This handles cases where "Site" is missing
#   ) %>%
#   # Handle missing site numbers (if needed)
#   mutate(Site = ifelse(is.na(Site), "", Site)) %>%
#   # Create a new column for display-friendly x-axis labels
#   mutate(Basin_Site = ifelse(Site == "", Basin, paste(Basin, Site, sep = "_")))
# 
# # Replace underscores with spaces in the x-axis labels
# df_eta_long <- df_eta_long %>%
#   mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "
# 
# 
# # Define custom order for facets
# df_eta_long <- df_eta_long %>%
#   mutate(
#     param_groups = factor(param_groups, levels = c("Climate", "Solar",
#                                             "Snow", "PET",
#                                             "Soil", "Intcpt.",
#                                             "Runoff", "G")),  # Custom order for vertical facets
#     Observation = factor(Observation, levels = c("Q", "SNOTEL",
#                                                  "ASO", "SCA",
#                                                  "SMS2","SMAP",
#                                                  "AET")) # Custom order for horizontal facets
#   )



palette_name <- "YlGnBu"
library(RColorBrewer)
colors4plot <- brewer.pal(n = 6, name = palette_name)
breaks <- seq(0, 1, by = 0.2)

# Stepped color scale

# Plot
library(ggplot2)

ggplot(df_eta_long, aes(x = Basin_Site, y = param_names, fill = eta.star)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_stepsn(name = "η*", colours = colors4plot, guide = "coloursteps", breaks = breaks) +
  facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
  labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
       x = "Observation by Basin", y = "Parameters") +
  geom_text(aes(label = error_marker), vjust = 0.5, size = 2, na.rm = TRUE) +
  #scale_y_discrete(limits = rev(levels(df_eta_long$param_names))) +  # Invert y-axis order
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
    axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_day.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)



# continuous color scale

# ggplot(df_eta_long, aes(x = Basin_Site, y = param_names, fill = eta.star)) +
#   geom_tile(color = "white") +
#   scale_color_continuous(name = "η*") +
#   facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
#   labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
#     x = "Observation by Basin and Site", y = "Parameters") +
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1),
#     strip.text.x = element_text(size = 10, face = "bold"),
#     strip.text.y = element_text(size = 10, face = "bold")
#   )

# Alternatively, specify the plot explicitly




# Virdis color scale

# # Plot
# library(ggplot2)
#
# ggplot(df_eta_long, aes(x = Basin_Site, y = param_names, fill = eta.star)) +
#   geom_tile(color = "white") +
#   scale_fill_viridis_c(option = "C", name = "η*") +
#   facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
#   labs(title = "Faceted Heatmap by Parameter Group and Observation Type",
#        x = "Observation by Basin and Site", y = "Parameters") +
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1),
#     strip.text.x = element_text(size = 10, face = "bold"),
#     strip.text.y = element_text(size = 10, face = "bold")
#   )







# # Set environment ---------------------------------------------
# source("source/run_process/pws.set.trial.R")
# 
# # Set trial and read in data and repeat for each basin ----------------------
# 
# pws.set.trial(project_name = "final_EastRiv",
#               trial_number = 2) #if things went okay this should be 3
# 
# morris_design_East <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
# 
# variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
# # Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
# list_data_East <- list()
# # Loop through each variable and metric type to dynamically load the list_EET files
# for (var in variables) {
#   file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
#   
#   # Check if the file exists (to handle optional metrics)
#   if (file.exists(file_path)) {
#     list_data_East[[var]] <- fromJSON(file_path)
#   } else {
#     warning(paste("File not found:", file_path))
#   }
# }
# 
# 
# pws.set.trial(project_name = "final_BlueRiv",
#               trial_number = 2) #if things went okay this should be 3
# 
# morris_design_Blue <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
# 
# variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
# # Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
# list_data_Blue <- list()
# # Loop through each variable and metric type to dynamically load the list_EET files
# for (var in variables) {
#   file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
#   
#   # Check if the file exists (to handle optional metrics)
#   if (file.exists(file_path)) {
#     list_data_Blue[[var]] <- fromJSON(file_path)
#   } else {
#     warning(paste("File not found:", file_path))
#   }
# }
# 
# 
# 
# pws.set.trial(project_name = "final_DoloresRiv",
#               trial_number = 2) #if things went okay this should be 3
# 
# morris_design_Dolores <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))
# 
# variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")
# # Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
# list_data_Dolores <- list()
# # Loop through each variable and metric type to dynamically load the list_EET files
# for (var in variables) {
#   file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")
#   
#   # Check if the file exists (to handle optional metrics)
#   if (file.exists(file_path)) {
#     list_data_Dolores[[var]] <- fromJSON(file_path)
#   } else {
#     warning(paste("File not found:", file_path))
#   }
# }
# 
# 
# 
# 
# # Create dataframe with strategically named columns for grouping -------------
# 
# df_mu <- data.frame(
#   Q_East_2200 = list_data_East$Q$`09112200`$total$NRMSE$mu.star,
#   Q_East_2500 = list_data_East$Q$`09112500`$total$NRMSE$mu.star,
#   Q_Blue_6490 = list_data_Blue$Q$`09046490`$total$NRMSE$mu.star,
#   Q_Blue_6600 = list_data_Blue$Q$`09046600`$total$NRMSE$mu.star,
#   Q_Dolores_5000 = list_data_Dolores$Q$`09165000`$total$NRMSE$mu.star,
#   Q_Dolores_6500 = list_data_Dolores$Q$`09166500`$total$NRMSE$mu.star,
#   
#   SNOTEL_East_380 = list_data_East$SWE$CO$`380`$NRMSE$mu.star,
#   SNOTEL_East_737 = list_data_East$SWE$CO$`737`$NRMSE$mu.star,
#   SNOTEL_Blue_531 = list_data_Blue$SWE$CO$`531`$NRMSE$mu.star,
#   SNOTEL_Dolores_465 = list_data_Dolores$SWE$CO$`465`$NRMSE$mu.star,
#   SNOTEL_Dolores_586 = list_data_Dolores$SWE$CO$`586`$NRMSE$mu.star,
#   SNOTEL_Dolores_739 = list_data_Dolores$SWE$CO$`739`$NRMSE$mu.star,
#   SNOTEL_Dolores_1060 = list_data_Dolores$SWE$CO$`1060`$NRMSE$mu.star,
#   SNOTEL_Dolores_1185 = list_data_Dolores$SWE$CO$`1185`$NRMSE$mu.star,
#   
#   # WILL NEED TO SWITCH THIS NRMSE IN THE FUTURE, WILL IT EFFECT THE ID? Yes...
#   ASO_East = list_data_East$ASO$MAE_bybasin$mu.star,
#   ASO_Blue = list_data_Blue$ASO$MAE_bybasin$mu.star,
#   ASO_Dolores = list_data_Dolores$ASO$MAE_bybasin$mu.star,
#   
#   SCA_East = list_data_East$MOD10A1$NRMSE_bybasin$mu.star,
#   SCA_Blue = list_data_Blue$MOD10A1$NRMSE_bybasin$mu.star,
#   SCA_Dolores = list_data_Dolores$MOD10A1$NRMSE_bybasin$mu.star,
#   
#   SMS2_East_380 = list_data_East$SMS2$CO$`380`$NRMSE$mu.star,
#   SMS2_East_737 = list_data_East$SMS2$CO$`737`$NRMSE$mu.star,
#   SMS2_Blue_531 = list_data_Blue$SMS2$CO$`531`$NRMSE$mu.star,
#   SMS2_Dolores_586 = list_data_Dolores$SMS2$CO$`586`$NRMSE$mu.star,
#   SMS2_Dolores_1060 = list_data_Dolores$SMS2$CO$`1060`$NRMSE$mu.star,
#   SMS2_Dolores_1185 = list_data_Dolores$SMS2$CO$`1185`$NRMSE$mu.star,
#   
#   SMAP_East = list_data_East$SMAPsfwt$NRMSE_bybasin$mu.star,
#   SMAP_Blue = list_data_Blue$SMAPsfwt$NRMSE_bybasin$mu.star,
#   SMAP_Dolores = list_data_Dolores$SMAPsfwt$NRMSE_bybasin$mu.star,
#   
#   AET_East = list_data_East$openet$NRMSE_bybasin$mu.star,
#   AET_Blue = list_data_Blue$openet$NRMSE_bybasin$mu.star,
#   AET_Dolores = list_data_Dolores$openet$NRMSE_bybasin$mu.star
# )
# 
# 
# # Get mu.star into eta.star
# df_eta <- matrix(ncol = ncol(df_mu),
#                  nrow = nrow(df_mu)) 
# 
# # Normalize by the max in each column to get nu*
# for (i in 1:ncol(df_mu)){
#   df_eta[,i] <- df_mu[,i] / max(df_mu[,i])
# } # close loop
# 
# # Get back into df
# 
# df_eta <- as.data.frame(df_eta)
# colnames(df_eta) <- colnames(df_mu)
# 
# 
# # The same param_names vector was used to create the SA runs!!!
# df_eta$param_names <- colnames(morris_design_East$X)
# df_eta$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
#                         "Climate","Climate",
#                         "Solar", "Solar", "Solar", "Solar", "Solar",
#                         "Solar", "Solar",
#                         "PET", "PET",
#                         "Intcpt.", "Intcpt.", "Intcpt.", 
#                         "Intcpt.", "Intcpt.", 
#                         "Snow", "Snow", "Snow", "Snow", "Snow", "Snow", 
#                         "Snow", "Snow", "Snow", "Snow", "Snow", "Snow", 
#                         "Runoff", "Runoff", "Runoff", "Runoff", "Runoff", 
#                         "Soil", "Soil", "Soil", "Soil", "Soil", "Soil", 
#                         "Soil", "Soil", "Soil", "Soil", "Soil", "Soil", 
#                         "G")
# 
# 
# 
# 
# df_eta_long <- df_eta %>%
#   pivot_longer(
#     cols = -c(param_names, param_groups),
#     names_to = "Column_Name",
#     values_to = "eta.star"
#   ) %>%
#   # Split the column names into components
#   separate(
#     Column_Name,
#     into = c("Observation", "Basin", "Site"),
#     sep = "_",
#     fill = "right" # This handles cases where "Site" is missing
#   ) %>%
#   # Handle missing site numbers (if needed)
#   mutate(Site = ifelse(is.na(Site), "", Site)) %>%
#   # Create a new column for display-friendly x-axis labels
#   mutate(Basin_Site = ifelse(Site == "", Basin, paste(Basin, Site, sep = "_")))
# 
# # Replace underscores with spaces in the x-axis labels
# df_eta_long <- df_eta_long %>%
#   mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "
# 
# 
# # Define custom order for facets
# df_eta_long <- df_eta_long %>%
#   mutate(
#     param_groups = factor(param_groups, levels = c("Climate", "Solar",
#                                             "Snow", "PET",
#                                             "Soil", "Intcpt.",
#                                             "Runoff", "G")),  # Custom order for vertical facets
#     Observation = factor(Observation, levels = c("Q", "SNOTEL",
#                                                  "ASO", "SCA",
#                                                  "SMS2","SMAP",
#                                                  "AET")) # Custom order for horizontal facets
#   )
# 
# 
# palette_name <- "YlGnBu"
# library(RColorBrewer)
# colors <- brewer.pal(n = 6, name = palette_name)
# breaks <- seq(0, 1, by = 0.2)
# 
# 
# # Plot
# library(ggplot2)
# 
# ggplot(df_eta_long, aes(x = Basin_Site, y = param_names, fill = eta.star)) +
#   geom_tile(color = "white") +
#   scale_fill_stepsn(name = "η*", colours = colors, guide = "coloursteps", breaks = breaks) +
#   facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
#   labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
#        x = "Observation by Basin and Site", y = "Parameters") +
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1),
#     strip.text.x = element_text(size = 10, face = "bold"),
#     strip.text.y = element_text(size = 10, face = "bold")
#   )
#   
# 
# # Alternatively, specify the plot explicitly
# ggsave(
#   filename = file.path(plot_dir,"sensi_heat_facet_Dol.png"),   # Save as PDF
#   plot = last_plot(),                       # Replace with your plot object
#   width = 6.5, height = 9, units = "in"
# )
# 
#   
# # # Plot
# # library(ggplot2)
# # 
# # ggplot(df_eta_long, aes(x = Basin_Site, y = param_names, fill = eta.star)) +
# #   geom_tile(color = "white") +
# #   scale_fill_viridis_c(option = "C", name = "η*") +
# #   facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
# #   labs(title = "Faceted Heatmap by Parameter Group and Observation Type",
# #        x = "Observation by Basin and Site", y = "Parameters") +
# #   theme_minimal(base_size = 12) +
# #   theme(
# #     axis.text.x = element_text(angle = 90, hjust = 1),
# #     strip.text.x = element_text(size = 10, face = "bold"),
# #     strip.text.y = element_text(size = 10, face = "bold")
# #   )



