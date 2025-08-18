# ---------------------------------------------------------------------------
# INITIALIZE
# ---------------------------------------------------------------------------


# Load `here` for path management
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", quietly = TRUE)
}
library(here)

rm(list = ls()) # Clear Environment
source(file.path(here(),"source/run_process/pws.initialize.R")) # Source fn
pws.initialize() # Loads this repository of scripts



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


# Grab parameter names, assumes all projects used the same parameter vector
pws.set.trial(project_name = "day_TaylorRiv",
              trial_number = 2) #if things went okay this should be 3

morris_design_Taylor <- readRDS(paste0(dynamic_input_dir, "/!morris_design.rds"))


# ---------------------------------------------------------------------------
# CREATE DATA FRAME FROM OUTPUTS
# ---------------------------------------------------------------------------

# Create dataframe with strategically named columns for grouping -------------
list_sigma_dfs <- list()
measures <- c("lower","mean","upper")

for(i in seq_along(measures)){
  
  df_name <- paste0("df_",measures[i])
  
  list_sigma_dfs[[df_name]] <- data.frame <- data.frame(
    # Q_East_2200 = list_data_day_EastRiv$Q$`09112200`$total$NRMSE$bootstrap$sigma[,i],
    # Q_East_2500 = list_data_day_EastRiv$Q$`09112500`$total$NRMSE$bootstrap$sigma[,i],
    # Q_Blue_6490 = list_data_day_BlueRiv$Q$`09046490`$total$NRMSE$bootstrap$sigma[,i],
    # Q_Blue_6600 = list_data_day_BlueRiv$Q$`09046600`$total$NRMSE$bootstrap$sigma[,i],
    # Q_Dolores_5000 = list_data_day_DoloresRiv$Q$`09165000`$total$NRMSE$bootstrap$sigma[,i],
    # Q_Dolores_6500 = list_data_day_DoloresRiv$Q$`09166500`$total$NRMSE$bootstrap$sigma[,i],
    # Q_Taylor_7000 = list_data_day_TaylorRiv$Q$`09107000`$total$NRMSE$bootstrap$sigma[,i],
    
    SNOTEL_East_380 = list_data_day_EastRiv$SWE$CO$`380`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_East_737 = list_data_day_EastRiv$SWE$CO$`737`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Blue_531 = list_data_day_BlueRiv$SWE$CO$`531`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Dolores_465 = list_data_day_DoloresRiv$SWE$CO$`465`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Dolores_586 = list_data_day_DoloresRiv$SWE$CO$`586`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Dolores_739 = list_data_day_DoloresRiv$SWE$CO$`739`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Dolores_1060 = list_data_day_DoloresRiv$SWE$CO$`1060`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Dolores_1185 = list_data_day_DoloresRiv$SWE$CO$`1185`$NRMSE$bootstrap$sigma[,i],
    SNOTEL_Taylor_1141 = list_data_day_TaylorRiv$SWE$CO$`1141`$NRMSE$bootstrap$sigma[,i],

    ASO_East_220421 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[1]][,i],
    ASO_East_220518 = list_data_day_EastRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[2]][,i],
    ASO_Blue_210418 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[1]][,i],
    ASO_Blue_210524 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[2]][,i],
    ASO_Blue_220419 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[3]][,i],
    ASO_Blue_220526 = list_data_day_BlueRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[4]][,i],
    ASO_Dolores_210420 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[1]][,i],
    ASO_Dolores_210514 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[2]][,i],
    ASO_Dolores_220415 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[3]][,i],
    ASO_Dolores_220510 = list_data_day_DoloresRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[4]][,i],
    ASO_Taylor_220421 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[1]][,i],
    ASO_Taylor_220525 = list_data_day_TaylorRiv$ASO$NRMSE_byflight_AW$bootstrap$sigma[[2]][,i],
    # ASO_East = list_data_day_EastRiv$ASO$MAE_bybasin$bootstrap$sigma[,i],
    # ASO_Blue = list_data_day_BlueRiv$ASO$MAE_bybasin$bootstrap$sigma[,i],
    # ASO_Dolores = list_data_day_DoloresRiv$ASO$MAE_bybasin$bootstrap$sigma[,i],

    SCA_East = list_data_day_EastRiv$MOD10A1$NRMSE_bybasin$bootstrap$sigma[,i],
    SCA_Blue = list_data_day_BlueRiv$MOD10A1$NRMSE_bybasin$bootstrap$sigma[,i],
    SCA_Dolores = list_data_day_DoloresRiv$MOD10A1$NRMSE_bybasin$bootstrap$sigma[,i],
    SCA_Taylor = list_data_day_DoloresRiv$MOD10A1$NRMSE_bybasin$bootstrap$sigma[,i]#,
    
    # SMS2_East_380 = list_data_day_EastRiv$SMS2$CO$`380`$NRMSE$bootstrap$sigma[,i],
    # SMS2_East_737 = list_data_day_EastRiv$SMS2$CO$`737`$NRMSE$bootstrap$sigma[,i],
    # SMS2_Blue_531 = list_data_day_BlueRiv$SMS2$CO$`531`$NRMSE$bootstrap$sigma[,i],
    # #SMS2_Dolores_586 = list_data_day_DoloresRiv$SMS2$CO$`586`$NRMSE$bootstrap$sigma[,i],
    # SMS2_Dolores_1060 = list_data_day_DoloresRiv$SMS2$CO$`1060`$NRMSE$bootstrap$sigma[,i],
    # SMS2_Dolores_1185 = list_data_day_DoloresRiv$SMS2$CO$`1185`$NRMSE$bootstrap$sigma[,i],
    # SMS2_Taylor_1141 = list_data_day_TaylorRiv$SMS2$CO$`1141`$NRMSE$bootstrap$sigma[,i],
    # 
    # SMAP_East = list_data_day_EastRiv$SMAPsfwt$NRMSE_bybasin$bootstrap$sigma[,i],
    # SMAP_Blue = list_data_day_BlueRiv$SMAPsfwt$NRMSE_bybasin$bootstrap$sigma[,i],
    # SMAP_Dolores = list_data_day_DoloresRiv$SMAPsfwt$NRMSE_bybasin$bootstrap$sigma[,i],
    # SMAP_Taylor = list_data_day_TaylorRiv$SMAPsfwt$NRMSE_bybasin$bootstrap$sigma[,i],
    # 
    # AET_East = list_data_day_EastRiv$openet$NRMSE_bybasin$bootstrap$sigma[,i],
    # AET_Blue = list_data_day_BlueRiv$openet$NRMSE_bybasin$bootstrap$sigma[,i],
    # AET_Dolores = list_data_day_DoloresRiv$openet$NRMSE_bybasin$bootstrap$sigma[,i],
    # AET_Taylor = list_data_day_TaylorRiv$openet$NRMSE_bybasin$bootstrap$sigma[,i]
  )
  
  rownames(list_sigma_dfs[[df_name]]) <- colnames(morris_design_Taylor$X) #same
}  

# ---------------------------------------------------------------------------
# PIVOT DF
# ---------------------------------------------------------------------------


# TESTTTTTTT
df_sigma <- list_sigma_dfs$df_mean


# The same param_names vector was used to create the SA runs!!!
df_sigma$param_names <- colnames(morris_design_Taylor$X) #convert to factor
df_sigma <- df_sigma %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_sigma$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
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
df_sigma_long <- df_sigma %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Column_Name",
    values_to = "sigma"
  ) %>%
  # Split the column names into components
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
df_sigma_long <- df_sigma_long %>%
  mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "


# Define custom order for facets
df_sigma_long <- df_sigma_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                   "Snow","Intcp.",
                                                   "Runoff","Soil", "G")),
    # Custom order for vertical facets
    # Observation = factor(Observation, levels = c("Q", "SNOTEL",
    #                                              "ASO", "SCA",
    #                                              "SMS2","SMAP",
    #                                              "AET")) # Custom order for horizontal facets
    # Observation = factor(Observation, levels = c("Q",
    #                                              "SMS2","SMAP",
    #                                              "AET")) # Custom order for horizontal facets
     Observation = factor(Observation, levels = c("SNOTEL",
                                                  "ASO", "SCA")) # Custom order for horizontal facets
  )



# ---------------------------------------------------------------------------
# PLOT
# ---------------------------------------------------------------------------


library(pals)
# pal.bands(brewer.blues(100))

# Choose a darker subset, e.g., positions 3 to 9
custom_blues <- brewer.blues(9)[2:9]

# pal.csf(parula, main="parula")
# pal.csf(cubehelix, main="cubehelix")
# pal.csf(plasma, main="plasma")


# Plot
library(ggplot2)

# Note on mac: control+command+space for character viewer

df_sigma_long$log_sigma <- log10(df_sigma_long$sigma)

df_sigma_long$norm_sigma <- df_sigma_long$sigma/100


ggplot(df_sigma_long, aes(x = Basin_Site, y = param_names, fill = norm_sigma)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # scale_fill_stepsn(name = "η*", colours = colors4plot, guide = "coloursteps", breaks = breaks) +
  scale_fill_gradientn(
    colors = custom_blues,  # You can use any number between 3–9 for smoothness
    name = "σ") +
  facet_grid(param_groups ~ Observation, scales = "free", space = "free") + # Facet by group and observation
  labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
    x = "Observation by Basin", y = "Parameters") +
  #geom_text(aes(label = error_marker), vjust = 0.5, size = 2, na.rm = TRUE) +
  #scale_y_discrete(limits = rev(levels(df_eta_long$param_names))) +  # Invert y-axis order
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
    axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold")
  )

 ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_sigma_snow.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)




