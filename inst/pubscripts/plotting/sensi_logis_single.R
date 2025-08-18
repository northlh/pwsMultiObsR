



# ---------------------------------------------------------------------------
# LOAD LIST DATA OF GOF STATS
# ---------------------------------------------------------------------------

variables <- c("Q","SWE","ASO","MOD10A1","SMS2", "SMAPsfwt","openet")

pws.read.lists(project_name = "day_BlueRiv",
               trial_number = 2,
               var_names = variables,
               metrics = "GOF",
               list_type = "EET")



# ---------------------------------------------------------------------------
# LOAD LIST DATA OF GOF STATS
# ---------------------------------------------------------------------------

pws.sensi.plot.logis(df_mean = df_mean2,
                     df_lower = df_lower2,
                     df_upper = df_upper2,
                     plot_dir = plot_dir,
                     ncol_plot = 1,
                     plot_filename = "plot_sensi_logis_BlueASO.png")


df_mean1 = as.data.frame(list_data_day_BlueRiv$Q_GOF$`09046490`$total$NRMSE$bootstrap$eta_star[,2])
colnames(df_mean1) <- "Q_09046490"
df_lower1 = as.data.frame(list_data_day_BlueRiv$Q_GOF$`09046490`$total$NRMSE$bootstrap$eta_star[,1])
colnames(df_lower1) <- "Q_09046490"
df_upper1 = as.data.frame(list_data_day_BlueRiv$Q_GOF$`09046490`$total$NRMSE$bootstrap$eta_star[,3])
colnames(df_upper1) <- "Q_09046490"



df_mean2 = as.data.frame(list_data_day_BlueRiv$ASO_GOF$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,2])
colnames(df_mean2) <- "ASO"
df_lower2 = as.data.frame(list_data_day_BlueRiv$ASO_GOF$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,1])
colnames(df_lower2) <- "ASO"
df_upper2 = as.data.frame(list_data_day_BlueRiv$ASO_GOF$NRMSE_byflight_AW$bootstrap$eta_star[[4]][,3])
colnames(df_upper2) <- "ASO"



ncol_plot = 1
plot_dir = plot_dir


pws.set.trial("day_EastRiv",1)

nhru_shp <- file.path(gis_dir, "model_nhru.shp")

plot(st_read(nhru_shp)[,1])
                      
                      
                
