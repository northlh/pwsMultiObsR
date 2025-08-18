# ---------------------------------------------------------------------------
# LOAD LIST DATA OF GOF STATS
# ---------------------------------------------------------------------------

# Can be used in a loop for many projects or trials
test_proj_names <- c("day_BlueRiv", "day_DoloresRiv",
                     "day_EastRiv", "day_TaylorRiv")

test_trial_nums <- c(3,3,3,3)

variables <- c("Q","SWE","ASO","MOD10A1","SMS2", "SMAPsfwt","openet")

for (i in 1:length(test_proj_names)){

  pws.read.lists(project_name = test_proj_names[i],
                trial_number = test_trial_nums[i],
                var_names = variables,
                metrics = "GOF",
                list_type = "GOF")

}



# ---------------------------------------------------------------------------
# GET PERTINENT DATA (IE RMSE STATS) INTO A DF
# ---------------------------------------------------------------------------


# df_gof <- data.frame(
#   Q_East_09112200 = list_data_day_EastRiv$Q$`09112200`$total$NRMSE,
#   Q_East_09112500 = list_data_day_EastRiv$Q$`09112500`$total$NRMSE,
# 
#   SNOTEL_East_380 = list_data_day_EastRiv$SWE$CO$`380`$NRMSE,
#   SNOTEL_East_737 = list_data_day_EastRiv$SWE$CO$`737`$NRMSE,
# 
#   ASO_East_20220421 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,1],
#   ASO_East_20220523 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,2],
# 
#   SCA_East = list_data_day_EastRiv$MOD10A1$NRMSE_bybasin,
# 
#   SMS2_East_380 = list_data_day_EastRiv$SMS2$CO$`380`$NRMSE,
#   SMS2_East_737 = list_data_day_EastRiv$SMS2$CO$`737`$NRMSE,
# 
#   SMAP_East = list_data_day_EastRiv$SMAPsfwt$NRMSE_bybasin,
# 
#   AET_East = list_data_day_EastRiv$openet$NRMSE_bybasin
# )


df_gof_east <- data.frame(
  Q_East_2200 = list_data_day_EastRiv$Q$`09112200`$total$NRMSE,
  Q_East_2500 = list_data_day_EastRiv$Q$`09112500`$total$NRMSE,
  
  SNOTEL_East_380 = list_data_day_EastRiv$SWE$CO$`380`$NRMSE,
  SNOTEL_East_737 = list_data_day_EastRiv$SWE$CO$`737`$NRMSE,
  
  ASO_East_220421 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,1],
  ASO_East_220518 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,2],
  
  SCA_East = list_data_day_EastRiv$MOD10A1$NRMSE_bybasin,
  
  SMS2_East_380 = list_data_day_EastRiv$SMS2$CO$`380`$NRMSE,
  SMS2_East_737 = list_data_day_EastRiv$SMS2$CO$`737`$NRMSE,
  
  SMAP_East = list_data_day_EastRiv$SMAPsfwt$NRMSE_bybasin,
  
  AET_East = list_data_day_EastRiv$openet$NRMSE_bybasin
)

df_gof_blue <- data.frame(
  Q_Blue_6490 = list_data_day_BlueRiv$Q$`09046490`$total$NRMSE,
  Q_Blue_6600 = list_data_day_BlueRiv$Q$`09046600`$total$NRMSE,
  
  SNOTEL_Blue_531 = list_data_day_BlueRiv$SWE$CO$`531`$NRMSE,
  
  ASO_Blue_210418 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,1],
  ASO_Blue_210524 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,2],
  ASO_Blue_220419 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,3],
  ASO_Blue_220526 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,4],
  
  SCA_Blue = list_data_day_BlueRiv$MOD10A1$NRMSE_bybasin,
  
  SMS2_Blue_531 = list_data_day_BlueRiv$SMS2$CO$`531`$NRMSE,
  
  SMAP_Blue = list_data_day_BlueRiv$SMAPsfwt$NRMSE_bybasin,
  
  AET_Blue = list_data_day_BlueRiv$openet$NRMSE_bybasin
)


df_gof_dolores <- data.frame(
  
  Q_Dolores_5000 = list_data_day_DoloresRiv$Q$`09165000`$total$NRMSE,
  Q_Dolores_6500 = list_data_day_DoloresRiv$Q$`09166500`$total$NRMSE,
  
  SNOTEL_Dolores_465 = list_data_day_DoloresRiv$SWE$CO$`465`$NRMSE,
  SNOTEL_Dolores_586 = list_data_day_DoloresRiv$SWE$CO$`586`$NRMSE,
  SNOTEL_Dolores_739 = list_data_day_DoloresRiv$SWE$CO$`739`$NRMSE,
  SNOTEL_Dolores_1060 = list_data_day_DoloresRiv$SWE$CO$`1060`$NRMSE,
  SNOTEL_Dolores_1185 = list_data_day_DoloresRiv$SWE$CO$`1185`$NRMSE,
  
  ASO_Dolores_210420 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,1],
  ASO_Dolores_210514 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,2],
  ASO_Dolores_220415 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,3],
  ASO_Dolores_220510 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,4],
  
  SCA_Dolores = list_data_day_DoloresRiv$MOD10A1$NRMSE_bybasin,
  
  #SMS2_Dolores_586 = list_data_day_DoloresRiv$SMS2$CO$`586`$NRMSE,
  SMS2_Dolores_1060 = list_data_day_DoloresRiv$SMS2$CO$`1060`$NRMSE,
  SMS2_Dolores_1185 = list_data_day_DoloresRiv$SMS2$CO$`1185`$NRMSE,
  
  SMAP_Dolores = list_data_day_DoloresRiv$SMAPsfwt$NRMSE_bybasin,
  
  AET_Dolores = list_data_day_DoloresRiv$openet$NRMSE_bybasin
)


df_gof_taylor <- data.frame(
  
  Q_Taylor_7000 = list_data_day_TaylorRiv$Q$`09107000`$total$NRMSE,
  
  SNOTEL_Taylor_1141 = list_data_day_TaylorRiv$SWE$CO$`1141`$NRMSE,
  
  ASO_Taylor_220421 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW[,1],
  ASO_Taylor_220525 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW[,2],
  
  SCA_Taylor = list_data_day_TaylorRiv$MOD10A1$NRMSE_bybasin,
  
  SMS2_Taylor_1141 = list_data_day_TaylorRiv$SMS2$CO$`1141`$NRMSE,
  
  SMAP_Taylor = list_data_day_TaylorRiv$SMAPsfwt$NRMSE_bybasin,
  
  AET_Taylor = list_data_day_TaylorRiv$openet$NRMSE_bybasin
)

# # NOT USED HERE --- from heatmap  
# df_gof_total <- list(
#     Q_East_2200 = list_data_day_EastRiv$Q$`09112200`$total$NRMSE,
#     Q_East_2500 = list_data_day_EastRiv$Q$`09112500`$total$NRMSE,
#     Q_Blue_6490 = list_data_day_BlueRiv$Q$`09046490`$total$NRMSE,
#     Q_Blue_6600 = list_data_day_BlueRiv$Q$`09046600`$total$NRMSE,
#     Q_Dolores_5000 = list_data_day_DoloresRiv$Q$`09165000`$total$NRMSE,
#     Q_Dolores_6500 = list_data_day_DoloresRiv$Q$`09166500`$total$NRMSE,
#     Q_Taylor_7000 = list_data_day_TaylorRiv$Q$`09107000`$total$NRMSE,
# 
#     SNOTEL_East_380 = list_data_day_EastRiv$SWE$CO$`380`$NRMSE,
#     SNOTEL_East_737 = list_data_day_EastRiv$SWE$CO$`737`$NRMSE,
#     SNOTEL_Blue_531 = list_data_day_BlueRiv$SWE$CO$`531`$NRMSE,
#     SNOTEL_Dolores_465 = list_data_day_DoloresRiv$SWE$CO$`465`$NRMSE,
#     SNOTEL_Dolores_586 = list_data_day_DoloresRiv$SWE$CO$`586`$NRMSE,
#     SNOTEL_Dolores_739 = list_data_day_DoloresRiv$SWE$CO$`739`$NRMSE,
#     SNOTEL_Dolores_1060 = list_data_day_DoloresRiv$SWE$CO$`1060`$NRMSE,
#     SNOTEL_Dolores_1185 = list_data_day_DoloresRiv$SWE$CO$`1185`$NRMSE,
#     SNOTEL_Taylor_1141 = list_data_day_TaylorRiv$SWE$CO$`1141`$NRMSE,
# 
#     ASO_East_220421 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,1],
#     ASO_East_220518 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW[,2],
#     ASO_Blue_210418 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,1],
#     ASO_Blue_210524 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,2],
#     ASO_Blue_220419 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,3],
#     ASO_Blue_220526 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW[,4],
#     ASO_Dolores_210420 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,1],
#     ASO_Dolores_210514 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,2],
#     ASO_Dolores_220415 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,3],
#     ASO_Dolores_220510 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW[,4],
#     ASO_Taylor_220421 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW[,1],
#     ASO_Taylor_220525 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW[,2],
# 
#     SCA_East = list_data_day_EastRiv$MOD10A1$NRMSE_bybasin,
#     SCA_Blue = list_data_day_BlueRiv$MOD10A1$NRMSE_bybasin,
#     SCA_Dolores = list_data_day_DoloresRiv$MOD10A1$NRMSE_bybasin,
#     SCA_Taylor = list_data_day_TaylorRiv$MOD10A1$NRMSE_bybasin,
# 
#     SMS2_East_380 = list_data_day_EastRiv$SMS2$CO$`380`$NRMSE,
#     SMS2_East_737 = list_data_day_EastRiv$SMS2$CO$`737`$NRMSE,
#     SMS2_Blue_531 = list_data_day_BlueRiv$SMS2$CO$`531`$NRMSE,
#     ###SMS2_Dolores_586 = list_data_day_DoloresRiv$SMS2$CO$`586`$NRMSE,
#     SMS2_Dolores_1060 = list_data_day_DoloresRiv$SMS2$CO$`1060`$NRMSE,
#     SMS2_Dolores_1185 = list_data_day_DoloresRiv$SMS2$CO$`1185`$NRMSE,
#     SMS2_Taylor_1141 = list_data_day_TaylorRiv$SMS2$CO$`1141`$NRMSE,
# 
#     SMAP_East = list_data_day_EastRiv$SMAPsfwt$NRMSE_bybasin,
#     SMAP_Blue = list_data_day_BlueRiv$SMAPsfwt$NRMSE_bybasin,
#     SMAP_Dolores = list_data_day_DoloresRiv$SMAPsfwt$NRMSE_bybasin,
#     SMAP_Taylor = list_data_day_TaylorRiv$SMAPsfwt$NRMSE_bybasin,
# 
#     AET_East = list_data_day_EastRiv$openet$NRMSE_bybasin,
#     AET_Blue = list_data_day_BlueRiv$openet$NRMSE_bybasin,
#     AET_Dolores = list_data_day_DoloresRiv$openet$NRMSE_bybasin,
#     AET_Taylor = list_data_day_TaylorRiv$openet$NRMSE_bybasin
#   )
# 
# #rownames(list_eta_dfs[[df_name]]) <- colnames(morris_design_Taylor$X) #same
 






# ---------------------------------------------------------------------------
# ID BEHAVIORAL 
# ---------------------------------------------------------------------------

# FUNCTION DEFINITION
pws.uncert.NRMSE <- function(df_gof,
                             NRMSE_thresh,
                             alt_thresh,
                             alt_cols = c("SCA", "SMS2",
                                          "SMAP")){
  
  # Diagnostics
  for (i in 1:length(df_gof)){
    xnames <- colnames(df_gof)
    hist(df_gof[,i], main = paste0("Histogram of ",xnames[i]))
  }
  summary(df_gof)
  
  # Intialize
  idx_behav <- list()
  idx_best <- list()
  NRMSE_best <- list()
  NRMSE_behav <- list()
  
  # For NRMSE, less is better, thus using <= thresh and min functions
  for (i in 1:length(df_gof)){
    
    obs_name <- names(df_gof)[i]
    
    # Check if the column name contains any of the alternative keywords
    use_alt_thresh <- any(sapply(alt_cols, function(x) grepl(x, obs_name)))
    current_thresh <- ifelse(use_alt_thresh, alt_thresh, NRMSE_thresh)
    #print(current_thresh)

    idx_behav[[obs_name]] <- which(df_gof[[obs_name]] <= current_thresh)
    NRMSE_behav[[obs_name]] <- df_gof[[obs_name]][idx_behav[[obs_name]]]
    
    idx_best[[obs_name]] <- which.min(df_gof[[obs_name]])
    NRMSE_best[[obs_name]] <- min(df_gof[[obs_name]])
  }
  
  return(list("idx_behav" = idx_behav,
              "NRMSE_behav" = NRMSE_behav,
              "idx_best" = idx_best,
              "NRMSE_best" = NRMSE_best))
} # close fn


# test <- pws.uncert.NRMSE(df_gof = df_gof,
#                  NRMSE_thresh = 100)

# Find behavioral models in each basin
results_behav <- list()
list_gofdfs <- list(Blue = df_gof_blue,
                          Dolores = df_gof_dolores,
                          East = df_gof_east,
                          Taylor = df_gof_taylor
                     )
for (i in 1:length(list_gofdfs)){
  list_name <- names(list_gofdfs)[[i]]
  #print(list_name)
  results_behav[[list_name]] <- pws.uncert.NRMSE(df_gof = list_gofdfs[[i]],
                                         NRMSE_thresh = 100, 
                                         alt_thresh = 150)
}


# ---------------------------------------------------------------------------
# #Criteria for all sites ----
criteria_east <- list(Q = colnames(df_gof_east[c(1,2)]),
                      Q_SNOTEL_380 = colnames(df_gof_east[c(1,2,3)]),
                      Q_SNOTEL_737 = colnames(df_gof_east[c(1,2,4)]),
                      Q_ASO_220421 = colnames(df_gof_east[c(1,2,5)]),
                      Q_ASO_220518 = colnames(df_gof_east[c(1,2,6)]),
                      Q_SCA = colnames(df_gof_east[c(1,2,7)]),
                      Q_SMS2_380 = colnames(df_gof_east[c(1,2,8)]),
                      Q_SMS2_737 = colnames(df_gof_east[c(1,2,9)]),
                      Q_SMAP = colnames(df_gof_east[c(1,2,10)]),
                      Q_AET = colnames(df_gof_east[c(1,2,11)])#,
)


criteria_blue <- list(Q = colnames(df_gof_blue[c(1,2)]),
                      Q_SNOTEL_581 = colnames(df_gof_blue[c(1,2,3)]),
                      Q_ASO_210418 = colnames(df_gof_blue[c(1,2,4)]),### less than 30 flag
                      Q_ASO_210524 = colnames(df_gof_blue[c(1,2,5)]),
                      Q_ASO_220419 = colnames(df_gof_blue[c(1,2,6)]),
                      Q_ASO_220526 = colnames(df_gof_blue[c(1,2,7)]),
                      Q_SCA = colnames(df_gof_blue[c(1,2,8)]),###
                      Q_SMS2_581 = colnames(df_gof_blue[c(1,2,9)]),
                      Q_SMAP = colnames(df_gof_blue[c(1,2,10)]),###
                      Q_AET = colnames(df_gof_blue[c(1,2,11)])#,
)


criteria_dolores <- list(Q = colnames(df_gof_dolores[c(1,2)]),
                         Q_SNOTEL_465 = colnames(df_gof_dolores[c(1,2,3)]),
                         Q_SNOTEL_586 = colnames(df_gof_dolores[c(1,2,4)]),
                         Q_SNOTEL_739 = colnames(df_gof_dolores[c(1,2,5)]),
                         Q_SNOTEL_1060 = colnames(df_gof_dolores[c(1,2,6)]),
                         #Q_SNOTEL_1185 = colnames(df_gof_dolores[c(1,2,7)]),### remove for box
                         Q_ASO_210420 = colnames(df_gof_dolores[c(1,2,8)]),
                         Q_ASO_210514 = colnames(df_gof_dolores[c(1,2,9)]),
                         Q_ASO_220415 = colnames(df_gof_dolores[c(1,2,10)]),
                         Q_ASO_220510 = colnames(df_gof_dolores[c(1,2,11)]),
                         Q_SCA = colnames(df_gof_dolores[c(1,2,12)]),
                         ###Q_SMS2_586 = colnames(df_gof_dolores[c(1,2,13)]),
                         # Q_SMS2_1060 = colnames(df_gof_dolores[c(1,2,13)]),###
                         # Q_SMS2_1185 = colnames(df_gof_dolores[c(1,2,14)]), ###
                         # Q_SMAP = colnames(df_gof_dolores[c(1,2,15)]), ### 
                         Q_AET = colnames(df_gof_dolores[c(1,2,16)])#,
)


criteria_taylor <- list(Q = colnames(df_gof_taylor[c(1)]),
                        Q_SNOTEL_1141 = colnames(df_gof_taylor[c(1,2)]),
                        Q_ASO_220421 = colnames(df_gof_taylor[c(1,3)]),
                        Q_ASO_220525 = colnames(df_gof_taylor[c(1,4)]),
                        Q_SCA = colnames(df_gof_taylor[c(1,5)]),
                        Q_SMS2_1141 = colnames(df_gof_taylor[c(1,6)]),
                        Q_SMAP = colnames(df_gof_taylor[c(1,7)]),
                        Q_AET = colnames(df_gof_taylor[c(1,8)])#,
)




# Criteria for select ----
criteria_east <- list(Q = colnames(df_gof_east[c(1,2)]),
                       # Q_SNOTEL_380 = colnames(df_gof_east[c(1,2,3)]),
                       # Q_SNOTEL_737 = colnames(df_gof_east[c(1,2,4)]),
                      #Q_SNOTEL_worst = colnames(df_gof_east[c(1,2,3)]),
                      Q_SNOTEL = colnames(df_gof_east[c(1,2,4)]),
                       # Q_ASO_220421 = colnames(df_gof_east[c(1,2,5)]),
                       # Q_ASO_220518 = colnames(df_gof_east[c(1,2,6)]),
                      Q_ASO = colnames(df_gof_east[c(1,2,5)]),
                      #Q_ASO_worst = colnames(df_gof_east[c(1,2,6)]),
                       Q_SCA = colnames(df_gof_east[c(1,2,7)]),
                       # Q_SMS2_380 = colnames(df_gof_east[c(1,2,8)]),
                       # Q_SMS2_737 = colnames(df_gof_east[c(1,2,9)]),
                      #Q_SMS2_worst = colnames(df_gof_east[c(1,2,8)]),
                      Q_SMS2 = colnames(df_gof_east[c(1,2,9)]),
                       Q_SMAP = colnames(df_gof_east[c(1,2,10)]),
                       Q_AET = colnames(df_gof_east[c(1,2,11)])#,
)


criteria_blue <- list(Q = colnames(df_gof_blue[c(1,2)]),
                      # Q_SNOTEL_581 = colnames(df_gof_blue[c(1,2,3)]),
                      Q_SNOTEL = colnames(df_gof_blue[c(1,2,3)]),
                      #Q_ASO_2104 = colnames(df_gof_blue[c(1,2,4)]),
                      # Q_ASO_210524 = colnames(df_gof_blue[c(1,2,5)]),
                      # Q_ASO_220419 = colnames(df_gof_blue[c(1,2,6)]),
                      Q_ASO = colnames(df_gof_blue[c(1,2,7)]),
                      #Q_ASO_220526 = colnames(df_gof_blue[c(1,2,7)]),
                      #Q_SCA = colnames(df_gof_blue[c(1,2,8)]),
                      # Q_SMS2_581 = colnames(df_gof_blue[c(1,2,9)]),
                      Q_SMS2 = colnames(df_gof_blue[c(1,2,9)]),
                      #Q_SMAP = colnames(df_gof_blue[c(1,2,10)]),
                      Q_AET = colnames(df_gof_blue[c(1,2,11)])#,
)


criteria_dolores <- list(Q = colnames(df_gof_dolores[c(1,2)]),
                      #Q_SNOTEL_465 = colnames(df_gof_dolores[c(1,2,3)]),
                      #Q_SNOTEL_586 = colnames(df_gof_dolores[c(1,2,4)]),
                      #Q_SNOTEL_739 = colnames(df_gof_dolores[c(1,2,5)]),
                      #Q_SNOTEL_1060 = colnames(df_gof_dolores[c(1,2,6)]),
                      Q_SNOTEL = colnames(df_gof_dolores[c(1,2,6)]),
                      #Q_SNOTEL_1185 = colnames(df_gof_dolores[c(1,2,7)]),
                      #Q_ASO_210420 = colnames(df_gof_dolores[c(1,2,8)]),
                      # Q_ASO_210514 = colnames(df_gof_dolores[c(1,2,9)]),
                      Q_ASO = colnames(df_gof_dolores[c(1,2,10)]),
                      #Q_ASO_220415 = colnames(df_gof_dolores[c(1,2,10)]),
                      #Q_ASO_220510 = colnames(df_gof_dolores[c(1,2,11)]),
                      Q_SCA = colnames(df_gof_dolores[c(1,2,12)]),
                      ####Q_SMS2_586 = colnames(df_gof_dolores[c(1,2,13)]),
                      #Q_SMS2_1060 = colnames(df_gof_dolores[c(1,2,13)]),
                      #Q_SMS2_1185 = colnames(df_gof_dolores[c(1,2,14)]),
                      #Q_SMAP = colnames(df_gof_dolores[c(1,2,15)]),
                      Q_AET = colnames(df_gof_dolores[c(1,2,16)])#,
)


criteria_taylor <- list(Q = colnames(df_gof_taylor[c(1)]),
                      #Q_SNOTEL_1141 = colnames(df_gof_taylor[c(1,2)]),
                      Q_SNOTEL = colnames(df_gof_taylor[c(1,2)]),
                      #Q_ASO_220421 = colnames(df_gof_taylor[c(1,3)]),
                      #Q_ASO_220525 = colnames(df_gof_taylor[c(1,4)]),
                      Q_ASO = colnames(df_gof_taylor[c(1,4)]),
                      Q_SCA = colnames(df_gof_taylor[c(1,5)]),
                      # Q_SMS2_1141 = colnames(df_gof_taylor[c(1,6)]),
                      Q_SMS2 = colnames(df_gof_taylor[c(1,6)]),
                      Q_SMAP = colnames(df_gof_taylor[c(1,7)]),
                      Q_AET = colnames(df_gof_taylor[c(1,8)])#,
)



# ---------------------------------------------------------------------------
# FIND INTERSECTION
# ---------------------------------------------------------------------------

# FUNCTION DEFINITION
pws.uncert.NRMSEintersect <- function(df_gof,
                                      criteria_names,
                                      idx_behav,
                                      Q_col = 1){
  
  # Initialize
  idx_behav_joint <- list()
  NRMSEQ_behav_joint <- list()
  
  # Loop through each criterion in criteria_names
  for (criterion in names(criteria_names)) {
    # Extract the vectors based on criteria names
    vectors_to_compare <- idx_behav[criteria_names[[criterion]]]
    
    # Find the common values among the selected vectors
    # IMPORTANT METHODOLOGICAL CHOICE OF A HARD INTERSECTION
    behav_indices_criteria <- Reduce(intersect, vectors_to_compare)
    
    # Only process further if there are common indices
    if (length(behav_indices_criteria) > 0) {
      
      # Store the common values in the results list
      idx_behav_joint[[criterion]] <- behav_indices_criteria
      # Pulls Q column of df, which is furthest downstream
      NRMSEQ_behav_joint[[criterion]] <- df_gof[[Q_col]][idx_behav_joint[[criterion]]]
    }
  }
  
  return(list("idx_behav_joint" = idx_behav_joint,
              "NRMSEQ_behav_joint" = NRMSEQ_behav_joint))
} # close fn



# Find intersection of behavioral models in each basin
results_intersection <- list()
list_criteria <- list(Blue = criteria_blue,
                      Dolores = criteria_dolores,
                      East = criteria_east,
                      Taylor = criteria_taylor
)

# downstream discharge columns
Q_col_numbers <- c(2,2,2,1) # second is furthest downstream, only 1 in taylor

for (i in 1:length(list_gofdfs)){
  list_name <- names(list_gofdfs)[[i]]
  results_intersection[[list_name]] <- pws.uncert.NRMSEintersect(df_gof = list_gofdfs[[i]],
                                                                 criteria_names = list_criteria[[i]],
                                                                 idx_behav = results_behav[[i]]$idx_behav,
                                                                 Q_col = Q_col_numbers[[i]])
}

# ---------------------------------------------------------------------------
# PLOT ECDF
# ---------------------------------------------------------------------------

library(tidyverse)

# Convert the nested list into a long dataframe
df_ecdf <- map_dfr(names(results_intersection), function(basin) {
  inner_list <- results_intersection[[basin]]$NRMSEQ_behav_joint
  map_dfr(names(inner_list), function(name) {
    tibble(
      Basin = basin,
      Observation = name,
      NRMSE = inner_list[[name]]
    )
  })
})

head(df_ecdf)

# Get it into 0 to 1
df_ecdf<- df_ecdf %>%
  mutate(NRMSE = NRMSE/100)

# # Get it into NSE
# df_6<- df_ecdf %>%
#   mutate(NSE = 1 - (NRMSE)^2)
#
# # Preview result
# print(df_nrmse)


# # TESTING PALETTES
# library(pals)
# pal.bands(stepped())
# pals::stepped()
# 
# library(RColorBrewer)
#display.brewer.all() # Show all palettes
# 

# LABELING WITH ONLY THE BEST FOR EACH OBS
df_ecdf$Observation <- df_ecdf$Observation |>
  str_replace_all("_", " ") |>             # Replace underscores with spaces
  str_replace("Q SNOTEL", "Q ∩ SNOTEL") |>
  str_replace("Q ASO", "Q ∩ ASO") |>
  str_replace("Q SCA", "Q ∩ SCA") |>
  str_replace("Q SMS2", "Q ∩ SMS2") |>
  str_replace("Q SMAP", "Q ∩ SMAP") |>
  str_replace("Q AET", "Q ∩ AET")

# # Stepped alternative palette
# custom_colors_ecdf <- c(
#   "Q" = "#333333",
#   "Q ∩ SNOTEL" = "#7ABECC",
#   "Q ∩ ASO" = "#967ACC",
#   "Q ∩ SCA" = "#B8DEE6",
#   "Q ∩ SMS2" = "#CCAA7A",
#   "Q ∩ SMAP" = "#990F26",
#   "Q ∩ AET" = "#54990F"
#   # Add all other Observation names as needed
# )

# Stepped3
custom_colors_ecdf <- c(
  #"Q" = "#636363", # for box
  "Q" = "black", # for ecdf
  "Q ∩ SNOTEL" = "#9ECAE1",
  "Q ∩ ASO" = "#6BAED6",
  "Q ∩ SCA" = "#756BB1",
  "Q ∩ SMS2" = "#FDAE6B",
  "Q ∩ SMAP" = "#FD8D3C",
  "Q ∩ AET" = "#74C476"
  # Add all other Observation names as needed
)

# Force levels to match
df_ecdf$Observation <- factor(df_ecdf$Observation, levels = names(custom_colors_ecdf))



# PLOTTING
ggplot(df_ecdf, aes(x = NRMSE, color = Observation)) +
  stat_ecdf(geom = "step", size = 0.6) +
  facet_wrap(~Basin, scales = "fixed") +
  labs(
    #title = "Cumulative Distribution of NRMSE by Basin",
    x = "NRMSE(Q)",
    y = "Empirical Cumulative Distribution",
    color = "Observation"
  ) +
  scale_color_manual(values = custom_colors_ecdf) +
  theme_light(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), # Keeps the plot box
    strip.background = element_rect(fill = "white", color = "white"), # Keeps facet box but clean
    strip.text = element_text(color = "black", size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/uncert_cdf_final3.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)




# # FOR PLOTTING ONE AT A TIME

# LABELING WITH SITES, CREATES MANY ITEMS IN LEGEND :/
df_ecdf$Observation <- df_ecdf$Observation |>
  str_replace("^Q_", "Q ∩ ") |>
  str_replace("^SNOTEL_", "SNOTEL ") |>
  str_replace("^ASO_", "ASO ") |>
  str_replace("^SCA_", "SCA") |>
  str_replace("^SMS2_", "SMS2 ") |>
  str_replace("^SMAP_", "SMAP") |>
  str_replace("^AET_", "AET") |>
  str_replace_all("_", " ") |>
  str_replace("Q SNOTEL", "Q ∩ SNOTEL") |>
  str_replace("Q ASO", "Q ∩ ASO") |>
  str_replace("Q SCA", "Q ∩ SCA") |>
  str_replace("Q SMS2", "Q ∩ SMS2") |>
  str_replace("Q SMAP", "Q ∩ SMAP") |>
  str_replace("Q AET", "Q ∩ AET")

unique_observations <- unique(df_ecdf$Observation)
unique_observations

custom_colors_by_site <- c(
  "Q" = "#636363", # for box
  #"Q" = "black", 
  
  "Q ∩ SNOTEL 581" = "#1f77b4", # Blue
  "Q ∩ SNOTEL 1060" = "#1f77b4",# Dolores - 1185
  "Q ∩ SNOTEL 739" = "#6BAED6",
  "Q ∩ SNOTEL 586" = "#9ECAE1",
  "Q ∩ SNOTEL 465" = "#C6DBEF",
  "Q ∩ SNOTEL 737" = "#1f77b4",#East
  "Q ∩ SNOTEL 380" = "#9ECAE1",
  "Q ∩ SNOTEL 1141" = "#1f77b4",#Tay
  
  "Q ∩ ASO 210418" = "#DADAEB", #Blue
  "Q ∩ ASO 210524" = "#BCBDDC",
  "Q ∩ ASO 220419" = "#9E9AC8",
  "Q ∩ ASO 220526" = "#756BB1",
  
  "Q ∩ ASO 210420" = "#DADAEB", #Dolores
  "Q ∩ ASO 210514" = "#BCBDDC",
  "Q ∩ ASO 220415" = "#9E9AC8",
  "Q ∩ ASO 220510" = "#756BB1",
  
  "Q ∩ ASO 220421" = "#9E9AC8", # East and Taylor
  "Q ∩ ASO 220518" = "#756BB1",
  #"Q ∩ ASO 220421" = "#3D0F99",
  "Q ∩ ASO 220525" = "#756BB1",
  
  "Q ∩ SCA" = "#C7E9C0",
  
  "Q ∩ SMS2 581" = "#E6550D",
  #"Q ∩ SMS2 1060" = "#E6550D",
  "Q ∩ SMS2 737" = "#E6550D",
  "Q ∩ SMS2 380" = "#FD8D3C",
  "Q ∩ SMS2 1141" = "#E6550D",
  
  "Q ∩ SMAP" = "#FDD0A2",
  "Q ∩ AET" = "#31A354"
  # Add all other site-specific or basin-specific colors as needed
)
df_ecdf$Observation <- factor(df_ecdf$Observation,
                              levels = names(custom_colors_by_site))




df_plot_filtered <- df_ecdf %>%
  filter(Basin == "Taylor")

ggplot(df_plot_filtered, aes(x = NRMSE, color = Observation)) +
  stat_ecdf(geom = "step", size = 0.6) +
  facet_wrap(~ Basin, scales = "fixed") + # Optional, but you can still facet for 1 basin
  scale_color_manual(values = custom_colors_by_site) +
  #scale_linetype_manual(values = custom_colors_by_site) +
  labs(x = "NRMSE(Q)", y = "Empirical Cumulative Distribution", color = "Observation", linetype = "Observation") +
  theme_light(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(color = "black", size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/uncert_cdf_day_Taylor.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 3.5, units = "in", dpi = 300
)



# ---------------------------------------------------------------------------
# PLOT BOXPLOT
# ---------------------------------------------------------------------------


plot_param_names = c("jh_coef",
                     #"rad_trncf",
                     "tmax_allsnow"#,
                     #"tmax_cbh_adj"
                     )

# plot_param_labels <- c(
#   "snow_cbh_adj" = "snow_cbh_adj",
#   "jh_coef" = "jh_coef (/ºF)"
# )
# 
# plot_param_labels <- c(#"tmax_allsnow (ºF)",
#                         "snow_cbh_adj",
#                        #"dday_intcp", "den_max",
#                        "jh_coef (/ºF)" #,
#                       # "soil_moist_max (in)",
#                       # "covden_sum",
#                       # "gwflow_coef (/d)"
#                        )

list_params <- list(Blue = params_day_BlueRiv,
                    Dolores = params_day_DoloresRiv,
                    East = params_day_EastRiv,
                    Taylor = params_day_TaylorRiv)

# Initialize an empty list to store dataframes
plotting_list <- list()

for (basin in names(list_params)) {
  for (i in seq_along(plot_param_names)) {
    plot_param <- plot_param_names[i]
    
    for (criterion in names(results_intersection[[basin]]$idx_behav_joint)) {
      behav_param_vals <- list_params[[basin]][results_intersection[[basin]]$idx_behav_joint[[criterion]], plot_param]
      
      # print(paste0(basin,plot_param,criterion))
      # print(length(behav_param_vals))
      
      # Create dataframe for this combination
      temp_df <- data.frame(
        Basin = basin,
        Parameter = plot_param,
        Criterion = criterion,
        Value = behav_param_vals
      )
      
      # Append to the plotting list
      plotting_list[[length(plotting_list) + 1]] <- temp_df
    }
  }
}

# Combine all dataframes into one
df_box <- do.call(rbind, plotting_list)


unique_observations_box <- unique(df_box$Criterion)
unique_observations_box

# LABELING WITH ONLY THE BEST FOR EACH OBS
df_box$Criterion <- df_box$Criterion |>
  str_replace_all("_", " ") |>             # Replace underscores with spaces
  str_replace("Q SNOTEL", "Q ∩ SNOTEL") |>
  str_replace("Q ASO", "Q ∩ ASO") |>
  str_replace("Q SCA", "Q ∩ SCA") |>
  str_replace("Q SMS2", "Q ∩ SMS2") |>
  str_replace("Q SMAP", "Q ∩ SMAP") |>
  str_replace("Q AET", "Q ∩ AET")


# Force levels to match for coloring by data criterion
df_box$Criterion <- factor(df_box$Criterion, levels = names(custom_colors_by_site)) # change palette
# Add units to the Param labels
df_box$Parameter <- factor(df_box$Parameter)
plot_param_labels <- c(
  "jh_coef" = "jh_coef (/ºF)",
  #"rad_trncf" = "rad_trncf",
  "tmax_allsnow" = "tmax_allsnow (ºF)"#,
  #"tmax_cbh_adj" = "tmax_cbh_adj (ºF)"
)

# Plot using ggplot2
library(ggplot2)
ggplot(df_box, aes(x = Criterion, y = Value, fill = Criterion)) +
  geom_boxplot(
    outlier.size = 0.8,
    outlier.shape = 3
  ) +
  facet_grid(Parameter ~ Basin,
             space = "free_x", scales = "free",
             labeller = labeller(Parameter = as_labeller(plot_param_labels))) +
  scale_fill_manual(values = custom_colors_by_site) + #change palette HERE +
  labs(
    #title = "Cumulative Distribution of NRMSE by Basin",
    x = "Observation Criteria",
    y = "Parameter Value"
  ) +
  theme_light() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), # Keeps the plot box
    strip.background = element_rect(fill = "white", color = "white"), # Clean facet strips
    strip.text = element_text(color = "black", size = 9, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 7, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/uncert_box_finalAll3.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 3.5, units = "in", dpi = 300
)


for (i in 1:length(results_intersection$Taylor$NRMSEQ_behav_joint)) {
  print((length(results_intersection$Taylor$NRMSEQ_behav_joint[[i]])/22000)*100)
}

length(results_intersection$Blue$NRMSEQ_behav_joint)










  # 
  # # DEFINE COLOR MAPPING FOR FINAL EAST FIG ONLY
  # 
  # # pals::pal.bands(stepped(n = 24))
  # # print(stepped(n=24))
  # color_mapping <- c(
  #   "Q" = "#333333", 
  #   "Q ∩ SNOTEL a" = "#C7B8E6",
  #   "Q ∩ SNOTEL b" = "#967ACC",
  #   "Q ∩ ASO a" = "#B8DEE6",
  #   "Q ∩ ASO b" = "#7ABECC",
  #   "Q ∩ ASO ab" = "#3E9FB3",
  #   "Q ∩ SCA" = "#78B33E",
  #   "Q ∩ SMS2 a" = "#B3823E",
  #   "Q ∩ AET" = "#B33E52"
  # )
  # 
  # # AGAIN SPECIFIC TO FINAL FIG, COLOR MAP NAMES LINE UP WITH INDICIES
  # # Reorder the levels of Criterion to match color_mapping
  # df_plot$Criterion <- factor(df_plot$Criterion, levels = names(behav_indices_joint), labels = names(color_mapping))
  # 
  # 
  # 
  # # Create the plot using ggplot2 with boxplot
  # p <- ggplot(df_plot, aes(x = Criterion, y = Value, color = Criterion)) +
  #   geom_boxplot() +  # Replace geom_density() with geom_boxplot()
  #   scale_color_manual(values = color_mapping) +  # Custom color mapping
  #   labs(
  #     x = "Criteria",
  #     y = plot_param_label,
  #     #title = paste(plot_param)
  #   ) +
  #   theme_classic() +
  #   theme(legend.position = "none",
  #         axis.text.x = element_text(angle = 90, hjust = 1))
  