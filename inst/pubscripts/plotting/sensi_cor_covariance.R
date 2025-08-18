


# Nasty script to examine attribute covariance,
# run this after running sensi_cor_facet script


basin_names <- c("Blue", "Dolores","East", "Taylor")

# Placeholder for all data
all_data_long2 <- list()

# Loop through each basin and data name
for (basin in basin_names) {
  for (data_name in data_names) {
    
    # Extract the correct metric based on the data type
    # METHODS NOTE: MAE is used for ASO due to limited timeseries (2 dates)
    metric_type <- ifelse(data_name == "ASO", "MAE_byhru", "NRMSE_byhru")
    #metric_type <- ifelse(data_name == "ASO", "NRMSE_byhru", "NRMSE_byhru")
    
    # Extract the list for the current basin and data type
    # for this, don't need basin data but hru_count is derived from it
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
    
    # Filter to one parameter
    df <- df[1, ]
    
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
    #df$data_name <- data_name
    #df$metric_used <- metric_type  # Track which metric was used
    df$hru <- 1:hru_count  # Add HRU column to track HRU numbers
    
    # Reshape to long format
    df_long <- df %>%
      # pivot_longer(
      #   cols = -c(Elevation,
      #             #Aspect, Slope,
      #             AnnPrecip,
      #             Tmax_DJF,
      #             #Tmax_MAM, Tmax_JJA, Tmax_SON,
      #             #Tmin_DJF, Tmin_MAM, Tmin_JJA, Tmin_SON,
      #             #Tmin_DJF, Tmin_JJA,
      #             basin_name, hru, Elevation),
      #   names_to = "forcing_type",
      #   values_to = "forcing_value"
      # ) %>%
      pivot_longer(
        cols = c(#Elevation,
                 #Aspect, Slope,
                 AnnPrecip,
                 Tmax_DJF
                 #Tmax_MAM, Tmax_JJA, Tmax_SON,
                 #Tmin_DJF, Tmin_MAM, Tmin_JJA, Tmin_SON
                 # Tmin_DJF, Tmin_JJA
        ),
        names_to = "forcing_type",
        values_to = "forcing_value"
      )

    # Append to the list
    all_data_long2[[paste(basin, data_name, sep = "_")]] <- df_long
  }
}

# Bind data
final_data2 <- bind_rows(all_data_long2)



# Calculate Spearman rank correlations for each facet
correlations2 <- final_data2 %>%
  group_by(forcing_type) %>%
  summarize(
    spearman_corr = cor(forcing_value, Elevation, method = "spearman"),
    p_value = cor.test(forcing_value, Elevation, method = "spearman",
                       exact = FALSE)$p.value,
    .groups = "drop"
  )

# Merge the correlation back into the data for easy access if needed
final_data_with_corr2 <- final_data2 %>%
  left_join(correlations2, by = c("forcing_type"))


# Define custom labels
attribute_labels2 <- c(
  "AnnPrecip" = "Ann. Avg. Precip. (mm)",
  "Tmax_DJF" = "Tmax DJF (ºC)"
)



ggplot(final_data_with_corr2, aes(x = forcing_value, y = Elevation, 
                                 color = basin_name)) +
  geom_point(size = 1.5, alpha = 0.75) +
  facet_grid(cols = vars(forcing_type),
             scales = "free_x",
             labeller = labeller(forcing_type = attribute_labels2)
             ) +  # Allow both x and y to vary
  geom_text(data = correlations2,
            aes(label = paste0("R = ", round(spearman_corr, 2),
                               "\np = ", formatC(p_value, format = "e", digits = 1))),
            x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
            inherit.aes = FALSE, size = 3) +
  # scale_y_log10() +  # Apply log scale to the y-axis
  scale_color_manual(values = basin_colors) +  # Apply custom colors
  theme_bw(base_size = 9) +
  labs(
    x = "Attribute Value",
    y = "Elevation (m)",
    color = "Catchment"#,
    #title = "Sensitivity vs. Attribute (Metric: MAE for ASO, NRMSE for Others)"
  ) +
  theme_light() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), # Keeps the plot box
    strip.background = element_rect(fill = "white", color = "white"), # Clean facet strips
    strip.text.x = element_text(color = "black", size = 9, face = "bold"),
    strip.text.y = element_text(color = "black", size = 9, face = "bold")
  )


ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/sensi_cor_day_covaraince.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)

