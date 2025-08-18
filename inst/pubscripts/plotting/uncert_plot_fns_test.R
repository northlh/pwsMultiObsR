

pws.uncert.plot.pbox <- function(dynamic_input_dir,
                                   plot_dir,
                                   behav_indices_joint,
                                   plot_param_names = c("tmax_allsnow",
                                                        "tmax_allrain_offset",
                                                        "snow_cbh_adj")) {
  
  # # Set up color palette
  # colors <- pals::brewer.set2(length(behav_indices_joint))  #colors for criteria
  
  # Read in MMLHS parameters
  mmLHS_params <- read.csv(paste0(dynamic_input_dir,"/!mmLHS_params.csv"))
  
  
  
  
  
  # Assemble plotting df per param  -----------------------------------------
  
  list_plots <- list()
  
  plot_param_labels <- c("tmax_allsnow (ºF)", "snow_cbh_adj",
                         "dday_intcp", "den_max",
                         "jh_coef (/ºF)","soil_moist_max (in)",
                         "covden_sum","gwflow_coef (/d)")
  
  # Number of columns in the grid
  ncol_plot <- 2
  
  # # Calculate total number of plots
  # total_plots <- length(plot_param_names)
  
  # Identify plots that belong to the bottom row
  #bottom_row_indices <- ((total_plots - 1) %% ncol_plot + 1):(total_plots)
  bottom_row_indices <- c(7,8) #HARCODED FOR FINAL
  #print(bottom_row_indices)
  
  
  
  
  
  for (i in seq_along(plot_param_names)) {
    plot_param <- plot_param_names[i]
    plot_param_label <- plot_param_labels[i]
    # Intialize the df per param
    plotting_list <- list()
    
    for (criterion in names(behav_indices_joint)){
      
      # Extract behavioral params for criteria and parameter combo
      behav_param_vals <- mmLHS_params[behav_indices_joint[[criterion]], plot_param]
      
      # Append to list
      plotting_list[[criterion]] <- data.frame(
        Value = behav_param_vals,
        Criterion = criterion
      )
      
    } # close for criterion
    
    # Combine all criteria into a long, singular data frame for the parameter
    df_plot <- do.call(rbind, plotting_list)
    
    
    # DEFINE COLOR MAPPING FOR FINAL EAST FIG ONLY
    
    # pals::pal.bands(stepped(n = 24))
    # print(stepped(n=24))
    color_mapping <- c(
      "Q" = "#333333", 
      "Q ∩ SNOTEL a" = "#C7B8E6",
      "Q ∩ SNOTEL b" = "#967ACC",
      "Q ∩ ASO a" = "#B8DEE6",
      "Q ∩ ASO b" = "#7ABECC",
      "Q ∩ ASO ab" = "#3E9FB3",
      "Q ∩ SCA" = "#78B33E",
      "Q ∩ SMS2 a" = "#B3823E",
      "Q ∩ AET" = "#B33E52"
    )
    
    # AGAIN SPECIFIC TO FINAL FIG, COLOR MAP NAMES LINE UP WITH INDICIES
    # Reorder the levels of Criterion to match color_mapping
    df_plot$Criterion <- factor(df_plot$Criterion, levels = names(behav_indices_joint), labels = names(color_mapping))

    # # Define the number of criteria for color mapping
    # num_criteria <- length(unique(df_plot$Criterion))
    # 
    # # If fewer than 3 criteria, default to a simple palette or let ggplot pick colors
    # if (num_criteria >= 3) {
    #   
    #   color_palette <- pals::brewer.set2(num_criteria)
    # } else {
    #   color_palette <- pals::brewer.set1(num_criteria)  # Fall back to a simpler palette
    # }
    # 
    # # Create the plot using ggplot2
    # p <- ggplot(df_plot, aes(x = Value, color = Criterion)) +
    #   geom_density(size = 1) +  # Draw density lines, size controls line thickness
    #   scale_color_manual(values = color_palette) +  # Use custom color palette
    #   labs(
    #     x = plot_param,  # Use the parameter name for the x-axis label
    #     y = "Density",
    #     title = paste("Density Plot for", plot_param)
    #   ) +
    #   theme_classic() +
    #   theme(legend.title = element_blank())  # Hide legend title
    # 
    # # Display the plot
    # print(p)
    
    
    # # Create the plot using ggplot2, letting ggplot assign colors automatically
    # p <- ggplot(df_plot, aes(x = Value, color = Criterion)) +
    #   geom_density(size = 1) +  # Draw density lines, size controls line thickness
    #   labs(
    #     x = plot_param,  # Use the parameter name for the x-axis label
    #     y = "Density",
    #     title = paste("Density Plot for", plot_param)
    #   ) +
    #   theme_classic() +
    #   theme(legend.title = element_blank())  # Hide legend title
    # 
    # # Display the plot
    # print(p)
    
    
    
    
    # # Create the plot using ggplot2 with boxplot
    # p <- ggplot(df_plot, aes(x = Criterion, y = Value, color = Criterion)) +
    #   geom_boxplot() +  # Replace geom_density() with geom_boxplot()
    #   labs(
    #     x = "Criterion",
    #     y = plot_param,
    #     title = paste("Boxplot for", plot_param)
    #   ) +
    #   theme_classic() +
    #   theme(legend.position = "none")  # Hide legend for boxplot
    
    # Create the plot using ggplot2 with boxplot
    p <- ggplot(df_plot, aes(x = Criterion, y = Value, color = Criterion)) +
      geom_boxplot() +  # Replace geom_density() with geom_boxplot()
      scale_color_manual(values = color_mapping) +  # Custom color mapping
      labs(
        x = "Criteria",
        y = plot_param_label,
        #title = paste(plot_param)
      ) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Suppress x-axis labels for non-bottom-row plots
    if (!i %in% bottom_row_indices) {
      p <- p + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
    }
    
    
    # Display the plot
    print(p)
    
    list_plots[[plot_param]] <- p
    
    
    
    
    # # Save the plot as PNG
    # plot_filename <- paste0("plot_uncert_pbox_", plot_param, ".png")
    # ggsave(filename = file.path(plot_dir, plot_filename),
    #        plot = p, width = 10, height = 8, dpi = 300)
    # cat("Parameter distributions have been saved as", file.path(plot_dir, plot_filename), " \n")
    
  } # close for plot_param
  
  
  # Combine all the plots in the specified number of columns
  combined_plot <- wrap_plots(list_plots, ncol = ncol_plot)
  
  print(combined_plot)
  
  plot_filename <- paste0("uncert_pbox_combined.png")
  ggsave(filename = file.path(plot_dir, plot_filename),
         plot = combined_plot, width = 6, height = 7, dpi = 300)
  cat("Parameter distributions have been saved as", file.path(plot_dir, plot_filename), " \n")

  
}#close fn 









# CDS PLOTS OF THE OBJECTIVE FUNCTION

pws.uncert.plot.objCDF <- function(mean_likelihoods){ # a list output by 
  
  
  for (i in 1:length(mean_likelihoods)){
    xnames <- names(mean_likelihoods)
    hist(mean_likelihoods[[i]], main = paste0("Histogram of ",xnames[i]))
  }
  
  
  # Convert mean_likelihoods to a long-format data frame, handling empty lists
  df_behav <- bind_rows(lapply(names(mean_likelihoods), function(name) {
    if (length(mean_likelihoods[[name]]) > 0) {
      return(data.frame(NRMSE = mean_likelihoods[[name]], Observation = name))
    } else {
      return(NULL)  # Skip empty entries
    }
  }), .id = "ID")
  
  # Ensure there is data before plotting
  if (nrow(df_behav) > 0) {
    # Plot the CDF
    ggplot(df_behav, aes(x = NRMSE, color = Observation)) +
      stat_ecdf(geom = "step", size = 1) +
      labs(title = "Cumulative Distribution of NRMSE for Behavioral Models, East River",
           x = "NRMSE (%)",
           y = "Cumulative Probability",
           color = "Criteria") +
      theme_minimal()
  } else {
    print("No behavioral models met the threshold; nothing to plot.")
  }
  
  
  
  
  # # JUST NRMSE of Q
  # 
  # NRMSE_behav_Q <- list()
  # 
  # for (i in 1:length(idx_behav_joint)){
  #   
  #   criteria_name <- names(idx_behav_joint)[i]
  #   
  #   NRMSE_behav_Q[[criteria_name]] <- df_gof[[1]][idx_behav_joint[[i]]]
  #   
  # }
  # 
  # # Convert mean_likelihoods to a long-format data frame, handling empty lists
  # df_behav_Q <- bind_rows(lapply(names(NRMSE_behav_Q), function(name) {
  #   if (length(NRMSE_behav_Q[[name]]) > 0) {
  #     return(data.frame(NRMSE = NRMSE_behav_Q[[name]], Observation = name))
  #   } else {
  #     return(NULL)  # Skip empty entries
  #   }
  # }), .id = "ID")
  # 
  # # Ensure there is data before plotting
  # if (nrow(df_behav_Q) > 0) {
  #   # Plot the CDF
  #   ggplot(df_behav_Q, aes(x = NRMSE, color = Observation)) +
  #     stat_ecdf(geom = "step", size = 1) +
  #     labs(title = "Cumulative Distribution of NRMSE for Behavioral Models, East River",
  #          x = "NRMSE (%)",
  #          y = "Cumulative Probability",
  #          color = "Criteria") +
  #     theme_minimal()
  # } else {
  #   print("No behavioral models met the threshold; nothing to plot.")
  # }
  
  
  
}









# # VIOLIN PLOT OF PARAMETERS
#
# pws.uncert.plot.pvio <- function(dynamic_input_dir,
#                                  plot_dir,
#                                  behav_indices_joint,
#                                  plot_param_names = c("tmax_allsnow",
#                                                       "tmax_allrain_offset",
#                                                       "snow_cbh_adj")) {
#   
#   # # Set up color palette
#   # colors <- pals::brewer.set2(length(behav_indices_joint))  #colors for criteria
#   
#   # Read in MMLHS parameters
#   mmLHS_params <- read.csv(paste0(dynamic_input_dir,"/!mmLHS_params.csv"))
#   
#   
#   
#   
#   
#   # Assemble plotting df per param  -----------------------------------------
#   
#   for (plot_param in plot_param_names){
#     
#     # Intialize the df per param
#     plotting_list <- list()
#     
#     for (criterion in names(behav_indices_joint)){
#       
#       # Extract behavioral params for criteria and parameter combo
#       behav_param_vals <- mmLHS_params[behav_indices_joint[[criterion]], plot_param]
#       
#       # Append to list
#       plotting_list[[criterion]] <- data.frame(
#         Value = behav_param_vals,
#         Criterion = criterion
#       )
#       
#     } # close for criterion
#     
#     # Combine all criteria into a long, singular data frame for the parameter
#     df_plot <- do.call(rbind, plotting_list)
#     
#     # # Define the number of criteria for color mapping
#     # num_criteria <- length(unique(df_plot$Criterion))
#     # 
#     # # If fewer than 3 criteria, default to a simple palette or let ggplot pick colors
#     # if (num_criteria >= 3) {
#     #   
#     #   color_palette <- pals::brewer.set2(num_criteria)
#     # } else {
#     #   color_palette <- pals::brewer.set1(num_criteria)  # Fall back to a simpler palette
#     # }
#     # 
#     # # Create the plot using ggplot2
#     # p <- ggplot(df_plot, aes(x = Value, color = Criterion)) +
#     #   geom_density(size = 1) +  # Draw density lines, size controls line thickness
#     #   scale_color_manual(values = color_palette) +  # Use custom color palette
#     #   labs(
#     #     x = plot_param,  # Use the parameter name for the x-axis label
#     #     y = "Density",
#     #     title = paste("Density Plot for", plot_param)
#     #   ) +
#     #   theme_classic() +
#     #   theme(legend.title = element_blank())  # Hide legend title
#     # 
#     # # Display the plot
#     # print(p)
#     
#     
#     # # letting ggplot assign colors automatically
#     # p <- ggplot(df_plot, aes(x = Value, color = Criterion)) +
#     #   geom_density(size = 1) +  # Draw density lines, size controls line thickness
#     #   labs(
#     #     x = plot_param,  # Use the parameter name for the x-axis label
#     #     y = "Density",
#     #     title = paste("Density Plot for", plot_param)
#     #   ) +
#     #   theme_classic() +
#     #   theme(legend.title = element_blank())  # Hide legend title
#     # 
#     # # Display the plot
#     # print(p)
#     
#     
#     
#     
#     # Create the plot using ggplot with violin
#     p <- ggplot(df_plot, aes(x = Criterion, y = Value, color = Criterion)) +
#       geom_violin(trim = FALSE) +  # Replace geom_density() with geom_boxplot()
#       labs(
#         x = "Criteria",
#         y = plot_param,
#         title = paste("Boxplot for", plot_param)
#       ) +
#       theme_classic() +
#       theme(legend.position = "none")  # Hide legend for boxplot
#     
#     # Display the plot
#     print(p)
#     
#     
#     
#     
#     # Save the plot as PNG
#     plot_filename <- paste0("plot_uncert_pvio_", plot_param, ".png")
#     ggsave(filename = file.path(plot_dir, plot_filename),
#            plot = p, width = 10, height = 8, dpi = 300)
#     cat("Parameter distributions have been saved as", file.path(plot_dir, plot_filename), " \n")
#     
#   } # close for plot_param
#   
#   
# }#close fn 
