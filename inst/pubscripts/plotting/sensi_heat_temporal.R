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




# STANDARD SENSI PLOT
# ---------------------------------------------------------------------------
# EAST RIVER
# list_ann <- list()
# measures <- c("lower","mean","upper")
# 
# for(i in seq_along(measures)){
#   
#   df_name <- paste0("df_",measures[i])
#   
#   list_ann[[df_name]] <- data.frame(
#     Q_09112500 = list_data_East$Q$`09112500`$total$NRMSE$bootstrap$eta_star[,i],
#     `1982-1991` = list_data_East$Q$`09112500`$rolling$`1982-1991`$NRMSE$bootstrap$eta_star[,i],
#     `1983-1992` = list_data_East$Q$`09112500`$rolling$`1983-1992`$NRMSE$bootstrap$eta_star[,i],
#     `1984-1993` = list_data_East$Q$`09112500`$rolling$`1984-1993`$NRMSE$bootstrap$eta_star[,i]
#   )
#   rownames(list_ann[[df_name]]) <- colnames(morris_design_East$X)
# }
# 


# EAST RIVER
list_ann <- list()
measures <- c("lower", "mean", "upper")

for (i in seq_along(measures)) {
  
  df_name <- paste0("df_", measures[i])
  
  # Generate the sequence of years dynamically
  years <- 1982:2022
  
  # Create the dataframe dynamically
  list_ann[[df_name]] <- data.frame(
    #`1982-2022` = list_data_East$Q$`09112500`$total$NRMSE$bootstrap$eta_star[, i],
    setNames(
      lapply(seq_along(years), function(j) {
        list_data_East$Q$`09112500`$annual$NRMSE$bootstrap$eta_star[[j]][, i]
      }),
      years
    )
  )
  
  # Remove "X" from column names
  colnames(list_ann[[df_name]]) <- sub("^X", "", colnames(list_ann[[df_name]]))
  
  rownames(list_ann[[df_name]]) <- colnames(morris_design_East$X)
}





df_ann <- list_ann$df_mean

# The same param_names vector was used to create the SA runs!!!
df_ann$param_names <- colnames(morris_design_East$X)
df_ann <- df_ann %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_ann$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
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
                         "G")


df_ann_long <- df_ann %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Column_Name",
    values_to = "eta.star")


# Define custom order for facets
df_ann_long <- df_ann_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                   "Snow","Intcp.",
                                                   "Runoff","Soil", "G")))

# Stepped color scale
palette_name <- "YlGnBu"
library(RColorBrewer)
colors <- brewer.pal(n = 6, name = palette_name)
breaks <- seq(0, 1, by = 0.2)

# Stepped color scale plot
ggplot(df_ann_long, aes(x = Column_Name, y = param_names, fill = eta.star)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_stepsn(name = "η*", colours = colors, guide = "coloursteps", breaks = breaks) +
  facet_grid(rows = df_ann_long$param_groups, scales = "free", space = "free") + # Facet by group and observation
  labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
    x = "Water Year", y = "Parameters") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
    axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
    strip.text.y = element_text(size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_day_Qann.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)



# ANNUAL DIFFERENCE
# ---------------------------------------------------------------------------
# Create list for difference dataframes
# CHANGED TO MU* on 05/06/2025, variables not renamed

list_eta_diffs <- list()

for (i in seq_along(measures)) {
  
  df_name <- paste0("df_", measures[i])
  
  # Extract the total and annual data
  df_total <- list_data_East$Q$`09112500`$total$NRMSE$bootstrap$eta_star[, i]
  df_annual <- do.call(cbind, lapply(1:41, function(j) {
    list_data_East$Q$`09112500`$annual$NRMSE$bootstrap$eta_star[[j]][, i]
  }))
  
  # Compute the difference (Annual - Total)
  df_diff <- df_annual - df_total
  
  # Convert to dataframe and rename columns
  df_diff <- as.data.frame(df_diff)
  colnames(df_diff) <- 1982:2022
  rownames(df_diff) <- colnames(morris_design_East$X)
  
  # Store in list
  list_eta_diffs[[df_name]] <- df_diff
}

# Select one measure to plot (e.g., "mean")
df_diff <- list_eta_diffs[["df_mean"]]

# Add param names
df_diff$param_names <- colnames(morris_design_East$X)
df_diff <- df_diff %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_diff$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
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
                          "G")

# Pivot
df_diff_long <- df_diff %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Year",
    values_to = "etaAnnualDiff")


# Define custom order for facets
df_diff_long <- df_diff_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                   "Snow","Intcp.",
                                                   "Runoff","Soil", "G"))
  )

# # FOR MU* ONLY
# # Removing NRMSE as percent (from HydroGOF), so metric in units of Std dev
# df_diff_long$etaAnnualDiff <- df_diff_long$etaAnnualDiff/100


# Get the max absolute difference to set symmetric color limits
max_diff <- max(abs(df_diff_long$etaAnnualDiff))


# Plot the heatmap
ggplot(df_diff_long, aes(x = Year, y = param_names, fill = etaAnnualDiff)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#ca0020", mid = "#f7f7f7", high = "#0571b0", midpoint = 0, limits = c(-max_diff, max_diff)) +
  #scale_fill_gradientn(colors = pals::coolwarm(100), limits = c(-max_diff, max_diff)) +
  facet_grid(rows = df_diff_long$param_groups, scales = "free", space = "free") +
  labs(#title = "Annual Difference in η* of Q from 1982-2022 Average, East River 09112500",
       x = "Water Year",
       y = "Parameters",
       fill = "η* Difference") + # μ*, η*
  theme_minimal(base_size = 9) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
         axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
         strip.text.y = element_text(size = 9, face = "bold"))

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_day_QannDiff_mu.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)




# ROLLING
# ---------------------------------------------------------------------------
# Create list for difference dataframes
list_roll <- list()
list_eta_diffs_roll <- list()
rolling_periods <- 1982:2013  # Start years for 10-year periods

for (i in seq_along(measures)) {
  
  df_name <- paste0("df_", measures[i])
  
  # Extract the total and rolling data
  df_total <- list_data_East$Q$`09112500`$total$NRMSE$bootstrap$eta_star[, i]
  
  df_rolling <- do.call(cbind, lapply(rolling_periods, function(start_year) {
    end_year <- start_year + 9
    period_name <- paste0(start_year, "-", end_year)
    list_data_East$Q$`09112500`$rolling[[period_name]]$NRMSE$bootstrap$eta_star[, i]
  }))
  

  # Compute the difference (Rolling - Total)
  df_diff <- df_rolling - df_total
  
  # Convert to dataframe and rename columns
  df_rolling <- as.data.frame(df_rolling)
  df_diff <- as.data.frame(df_diff)
  
  colnames(df_rolling) <- paste0(rolling_periods, "-", rolling_periods + 9)
  rownames(df_rolling) <- colnames(morris_design_East$X)
  
  colnames(df_diff) <- paste0(rolling_periods, "-", rolling_periods + 9)
  rownames(df_diff) <- colnames(morris_design_East$X)
  
  # Store in list
  list_roll[[df_name]] <- df_rolling
  list_eta_diffs_roll[[df_name]] <- df_diff
  
}

# Select one measure to plot (e.g., "mean")
df_roll <- list_roll[["df_mean"]]

# The same param_names vector was used to create the SA runs!!!
df_roll$param_names <- colnames(morris_design_East$X)
df_roll <- df_roll %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_roll$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
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
                         "G")


df_roll_long <- df_roll %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Column_Name",
    values_to = "eta.star")


# Define custom order for facets
df_roll_long <- df_roll_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                   "Snow","Intcp.",
                                                   "Runoff","Soil", "G")))

# Stepped color scale
palette_name <- "YlGnBu"
library(RColorBrewer)
colors <- brewer.pal(n = 6, name = palette_name)
breaks <- seq(0, 1, by = 0.2)

# Stepped color scale plot
ggplot(df_roll_long, aes(x = Column_Name, y = param_names, fill = eta.star)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_stepsn(name = "η*", colours = colors, guide = "coloursteps", breaks = breaks) +
  facet_grid(rows = df_roll_long$param_groups, scales = "free", space = "free") + # Facet by group and observation
  labs(#title = "Faceted Heatmap by Parameter Group and Observation Type",
    x = "10 Year Rolling Period", y = "Parameters") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
    axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
    strip.text.y = element_text(size = 9, face = "bold")
  )

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_day_Qroll.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)

# ROLLING DIFFERENCE
# ---------------------------------------------------------------------------
# Select one measure to plot (e.g., "mean")
df_diff_roll <- list_eta_diffs_roll[["df_mean"]]

# Add parameter names and groups
df_diff_roll$param_names <- colnames(morris_design_East$X)
df_diff_roll <- df_diff_roll %>%
  mutate(param_names = factor(param_names, levels = rev(sort(unique(param_names)))))
df_diff_roll$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
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
                               "G")

# Pivot to long format
df_diff_roll_long <- df_diff_roll %>%
  pivot_longer(
    cols = -c(param_names, param_groups),
    names_to = "Rolling_Period",
    values_to = "etaRollingDiff"
  )

# Define custom order for facets
df_diff_roll_long <- df_diff_roll_long %>%
  mutate(
    param_groups = factor(param_groups, levels = c("Climate", "Solar", "PET",
                                                   "Snow","Intcp.",
                                                   "Runoff","Soil", "G"))
  ) # Custom order for vertical facet

# # FOR MU* ONLY
# # Removing NRMSE as percent (from HydroGOF), so metric in units of Std dev
# df_diff_roll_long$etaRollingDiff <- df_diff_roll_long$etaRollingDiff/100
# 
# 
# # # Commented out so the scale is the same as annual
# # # Get the max absolute difference to set symmetric color limits
# max_diff_roll <- max(abs(df_diff_roll_long$etaRollingDiff))


# Plot the heatmap
ggplot(df_diff_roll_long, aes(x = Rolling_Period, y = param_names, fill = etaRollingDiff)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#ca0020", mid = "#f7f7f7", high = "#0571b0", midpoint = 0, limits = c(-max_diff, max_diff)) +
  facet_grid(rows = vars(param_groups), scales = "free", space = "free") +
  labs(#title = "Rolling Difference in η* of Q from 1982-2022 Average, East River 09112500",
       x = "10 Year Rolling Period",
       y = "Parameters",
       fill = "μ* Difference") +
  theme_minimal(base_size = 9) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Centered X-axis text
         axis.text.y = element_text(vjust = 0.5),  # Centered Y-axis text
         strip.text.y = element_text(size = 9, face = "bold"))

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_heat_day_QrollDiff_mu.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 6.5, units = "in", dpi = 300
)
