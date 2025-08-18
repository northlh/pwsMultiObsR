

# Set trial
source("source/run_process/pws.set.trial.R")

# Set trial and read in data and repeat for each basin ----------------------

pws.set.trial(project_name = "final_BlueRiv",
              trial_number = 3) #if things went okay this should be 3


# Load outputs
mmLHS_params_Blue <- read.csv(paste0(dynamic_input_dir,"/!mmLHS_params.csv"))

variables <- c("Q","SWE","ASO","MOD10A1","SMS2", "SMAPsfwt","openet")

# Dynamically load the GOF analysis results based on the input variables (e.g., SWE, Q)
list_data_Blue <- list()
# Loop through each variable and metric type to dynamically load the list_EET files
for (var in variables) {
  file_path <- paste0(dynamic_output_dir, "/!list_GOF_", var,".json")
  
  # Check if the file exists (to handle optional metrics)
  if (file.exists(file_path)) {
    list_data_Blue[[var]] <- fromJSON(file_path)
  } else {
    warning(paste("File not found:", file_path))
  }
}


# Define dataframe of desired objective functions

df_gof <- data.frame(
  Q_Blue_09046490 = list_data_Blue$Q$`09046490`$total$NRMSE,
  Q_Blue_09046600 = list_data_Blue$Q$`09046600`$total$NRMSE,
  
  SNOTEL_Blue_531 = list_data_Blue$SWE$CO$`531`$NRMSE,
  
  ASO_Blue_20220419 = list_data_Blue$ASO$NRMSE_byflight_AW[,3],
  ASO_Blue_20220526 = list_data_Blue$ASO$NRMSE_byflight_AW[,4],
  
  SCA_Blue = list_data_Blue$MOD10A1$NRMSE_bybasin,
  
  SMS2_Blue_531 = list_data_Blue$SMS2$CO$`531`$NRMSE,
  
  SMAP_Blue = list_data_Blue$SMAPsfwt$NRMSE_bybasin,
  
  AET_Blue = list_data_Blue$openet$NRMSE_bybasin
)



# Dignostics 
for (i in 1:length(df_gof)){
  xnames <- colnames(df_gof)
  hist(df_gof[,i], main = paste0("Histogram of ",xnames[i]))
}

summary(df_gof)

# Determine behavioral indices

NRMSE_thresh <- 100
idx_behav <- list()
idx_behav_test <- list()
idx_best <- list()
NRMSE_best <- list()
# thresholds <- c(0.833, 0.833, 
#                 0.5, 0.5,
#                 0.5, 0.5,
#                 1,
#                 1.2, 1.2,
#                 1.5,
#                 0.5)
# #thresholds <- thresholds*100




# Define the desired percentile (e.g., 25th percentile)
quant_threshold <- 0.05


quant_values <- apply(df_gof, 2, quantile, probs = quant_threshold)
# Compute the NRMSE value at the desired percentile

# # Find indices where NRMSE is within the percentile
# idx_behav_test2 <- which(df$NRMSE <= quant_values)
# 
# # View the result
# print(indices_within_percentile)
# 
# # Optionally, retrieve the rows of the dataframe that meet the condition
# filtered_df <- df[indices_within_percentile, ]
# print(filtered_df)


for (i in 1:length(df_gof)){
  
  obs_name <- names(df_gof)[i]
  
  idx_behav_test[[obs_name]] <- which(df_gof[[obs_name]] <= quant_values[[i]])
  idx_behav[[obs_name]] <- which(df_gof[[obs_name]] <= NRMSE_thresh)
  idx_best[[obs_name]] <- which.min(df_gof[[obs_name]])
  NRMSE_best[[obs_name]] <- min(df_gof[[obs_name]])
  
}



criteria_names <- list(Q = colnames(df_gof[c(1,2)]), 
                       Q_SNOTEL_531 = colnames(df_gof[c(1,2,3)]), 
                       Q_ASO_a = colnames(df_gof[c(1,2,4)]), 
                       Q_ASO_b = colnames(df_gof[c(1,2,5)]), 
                       Q_ASO_both = colnames(df_gof[c(1,2,4,5)]),
                       Q_SCA = colnames(df_gof[c(1,2,6)]),
                       Q_AET = colnames(df_gof[c(1,2,9)])#,
                       # Q_ASO_AET = colnames(df_gof[c(1,2,5,6,11)])
) 

pws.uncert.multiobj(criteria_names = criteria_names,
                    df_gof = df_gof,
                    behav_indices = idx_behav)

# pws.uncert.plot.params(dynamic_input_dir = dynamic_input_dir,
#                        plot_dir = plot_dir,
#                        behav_indices_joint = behav_indices_joint)


pws.uncert.plot.pbox(dynamic_input_dir = dynamic_input_dir,
                     plot_dir = plot_dir,
                     behav_indices_joint = behav_indices_joint,
                     plot_param_names = c("tmax_allsnow","snow_cbh_adj",
                                          "dday_intcp", "den_max",
                                          "jh_coef","soil_moist_max",
                                          "covden_sum","gwflow_coef"))

# pws.uncert.plot.pvio(dynamic_input_dir = dynamic_input_dir,
#                      plot_dir = plot_dir,
#                      behav_indices_joint = behav_indices_joint)

