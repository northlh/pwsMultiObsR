# Set Trial
source("source/run_process/pws.set.trial.R")
pws.set.trial(project_name = "day_TaylorRiv",
              trial_number = 2) #if things went okay this should be 3

# Read in morris object
morris_design <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))

# Read in EET files

variables <- c("Q","SWE","ASO","SMS2","SMAPsfwt","MOD10A1","openet")

# Dynamically load the sensitivity analysis results based on the input variables (e.g., SWE, Q)
list_data <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_EET_", var,"_GOF.json")

  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}


# # Custom dataframe of mu*
# df_mu <- data.frame(
#   Q = list_data$Q$`09112500`$total$KGE$mu.star,
#   SWE = list_data$SWE$CO$`380`$KGE$mu.star,
#   ASO = list_data$ASO$KGE$mu.star,
#   SMS2 = list_data$SMS2$CO$`380`$KGE$mu.star,
#   SMAP = list_data$SMAPsfwt$KGE_bybasin$mu.star,
#   SCA = list_data$MOD10A1$KGE_bybasin$mu.star,
#   AET = list_data$openet$KGE_bybasin$mu.star
# )




# BLUE RIVER
list_eta_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])
  
  list_eta_dfs[[df_name]] <- data.frame(
    Q_09046490 = list_data$Q$`09046490`$total$NRMSE$bootstrap$eta_star[,i],
    Q_09046600 = list_data$Q$`09046600`$total$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_531 = list_data$SWE$CO$`531`$NRMSE$bootstrap$eta_star[,i],
    ASO_MAE_basin = list_data$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    ASO_NRMSE_210418 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_NRMSE_210524 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    ASO_NRMSE_220419 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[3]][,i],
    ASO_NRMSE_220526 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,i],
    SMS2_531 = list_data$SMS2$CO$`531`$NRMSE$bootstrap$eta_star[,i],
    SMAP = list_data$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA = list_data$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET = list_data$openet$NRMSE_bybasin$bootstrap$eta_star[,i]
  )
  rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design$X)
}





# DOLORES
list_eta_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])
  
  list_eta_dfs[[df_name]] <- data.frame(
    Q_09165000 = list_data$Q$`09165000`$total$NRMSE$bootstrap$eta_star[,i],
    Q_09166500 = list_data$Q$`09166500`$total$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_465 = list_data$SWE$CO$`465`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_586 = list_data$SWE$CO$`586`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_739 = list_data$SWE$CO$`739`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_1060 = list_data$SWE$CO$`1060`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_1185 = list_data$SWE$CO$`1185`$NRMSE$bootstrap$eta_star[,i],
    ASO_MAE_basin = list_data$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    ASO_NRMSE_210420 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_NRMSE_210514 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    ASO_NRMSE_220415 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[3]][,i],
    ASO_NRMSE_220510 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,i],
    #SMS2_586 = list_data$SMS2$CO$`586`$NRMSE$bootstrap$eta_star[,i],
    SMS2_1060 = list_data$SMS2$CO$`1060`$NRMSE$bootstrap$eta_star[,i],
    SMS2_1185 = list_data$SMS2$CO$`1185`$NRMSE$bootstrap$eta_star[,i],
    SMAP = list_data$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA = list_data$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET = list_data$openet$NRMSE_bybasin$bootstrap$eta_star[,i]
  )
  rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design$X)
}





# EAST RIVER
list_eta_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])
  
  list_eta_dfs[[df_name]] <- data.frame(
    Q_09112500 = list_data$Q$`09112500`$total$NRMSE$bootstrap$eta_star[,i],
    Q_09112200 = list_data$Q$`09112200`$total$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_380 = list_data$SWE$CO$`380`$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_737 = list_data$SWE$CO$`737`$NRMSE$bootstrap$eta_star[,i],
    ASO_MAE_basin = list_data$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    ASO_NRMSE_220421 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_NRMSE_220510 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    SMS2_380 = list_data$SMS2$CO$`380`$NRMSE$bootstrap$eta_star[,i],
    SMS2_737 = list_data$SMS2$CO$`737`$NRMSE$bootstrap$eta_star[,i],
    SMAP = list_data$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA = list_data$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET = list_data$openet$NRMSE_bybasin$bootstrap$eta_star[,i]
  )
  rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design$X)
}





# TAYLOR RIVER
list_eta_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])
  
  list_eta_dfs[[df_name]] <- data.frame(
    Q_09107000 = list_data$Q$`09107000`$total$NRMSE$bootstrap$eta_star[,i],
    SNOTEL_1141 = list_data$SWE$CO$`1141`$NRMSE$bootstrap$eta_star[,i],
    ASO_MAE_basin = list_data$ASO$MAE_bybasin$bootstrap$eta_star[,i],
    ASO_NRMSE_220421 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[1]][,i],
    ASO_NRMSE_220425 = list_data$ASO$NRMSE_byflight_AW$bootstrap$eta_star[[2]][,i],
    SMS2_1141 = list_data$SMS2$CO$`1141`$NRMSE$bootstrap$eta_star[,i],
    SMAP = list_data$SMAPsfwt$NRMSE_bybasin$bootstrap$eta_star[,i],
    SCA = list_data$MOD10A1$NRMSE_bybasin$bootstrap$eta_star[,i],
    AET = list_data$openet$NRMSE_bybasin$bootstrap$eta_star[,i]
  )
  rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design$X)
}



# df_mu <- data.frame(
#   Jan = list_data$Q$`09112500`$monthly$KGE$mu.star[[1]],
#   Feb = list_data$Q$`09112500`$monthly$KGE$mu.star[[2]],
#   Mar = list_data$Q$`09112500`$monthly$KGE$mu.star[[3]],
#   Apr = list_data$Q$`09112500`$monthly$KGE$mu.star[[4]],
#   May = list_data$Q$`09112500`$monthly$KGE$mu.star[[5]],
#   Jun = list_data$Q$`09112500`$monthly$KGE$mu.star[[6]],
#   Jul = list_data$Q$`09112500`$monthly$KGE$mu.star[[7]],
#   Aug = list_data$Q$`09112500`$monthly$KGE$mu.star[[8]],
#   Sep = list_data$Q$`09112500`$monthly$KGE$mu.star[[9]],
#   Oct = list_data$Q$`09112500`$monthly$KGE$mu.star[[10]],
#   Nov = list_data$Q$`09112500`$monthly$KGE$mu.star[[11]],
#   Dec = list_data$Q$`09112500`$monthly$KGE$mu.star[[12]]
# )


# PLOTS
source("source/output_process/pws.sensi.plot.heatmap.R")
pws.sensi.plot.heatmap(df = list_eta_dfs$df_mean,
                       plot_dir = plot_dir,
                       plot_filename = "sensi_heatmap_fullPeriod2.png")

source("source/output_process/pws.sensi.plot.logis.R")
pws.sensi.plot.logis2(df_mean = list_eta_dfs$df_mean,
                     df_lower = list_eta_dfs$df_lower,
                     df_upper = list_eta_dfs$df_upper,
                     ncol_plot = 5, # 6, 6, 6, 5
                     plot_dir = plot_dir,
                     plot_filename = "sensi_logis_final2_test2.png")

print(sensi_param_list)
print(type2_param_list)

write_json(x = sensi_param_list, path = file.path(dynamic_output_dir,"!list_sensi_param.json"))
write_json(x = type1_param_list, path = file.path(dynamic_output_dir,"!list_sensi_param_type1.json"))
write_json(x = type2_param_list, path = file.path(dynamic_output_dir,"!list_sensi_param_type2.json"))

