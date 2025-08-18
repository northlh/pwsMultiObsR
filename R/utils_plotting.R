# The module hosts all plotting related functions



#' Plot SNOTEL stations in your catchment
#'
#' Relies on an internal STOTEL station metadata file to find SNOTEL stations
#' given user specifeid boundaries in a shapefile.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param hru_filename character: file name of the hru .shp file
#' @param gages_filename character: file name of the gages .shp file
#' @param buffer_dist integer: distance in m to flag stations near boundaries
#' @param map_extent_frac numeric: fraction of the overall bounding box to
#' extend the map view. Useful for seeing stations outside of the catchment.
#' @param plot_filename: character: name of the plot with .png extension
#' @return ggplot: plot object
#' @examples
#'
#' plot_snotel("/Users/me/mywork/",
#'             "east_river")
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import sf
plot_snotel<- function(dir_root,
                       project_name,
                       hru_filename = "model_nhru.shp",
                       gages_filename = "model_npoigages.shp",
                       buffer_dist = 1000,
                       map_extent_fac = 0.1,
                       plot_filename = "plot_snotel_stations.png",
                       option_save = TRUE,
                       option_overwrite = FALSE){

  directories <- fm_trial_set(dir_root, project_name, 1)
  gis_dir <- directories["dir_gis"]
  plot_dir <- directories["dir_plot"]

  # READ METADATA -----------------------------------------------------------

  # Read metadata from source data folder
  station_data <- read.csv(system.file("example_obs",
                                       "nrcs_metadata_clean.csv",
                                       package = "pwsMultiObsR"))

  # Filter the station data to only include relevant columns
  station_data <- station_data %>%
    dplyr::select(site_num, site_name, lat, lon)

  # Convert it to an sf object with lat/long
  stations_sf <- station_data %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Assuming (EPSG:4326)

  # READ .SHP -----------------------------------------------------

  hru_sf <- sf::st_read(file.path(gis_dir,hru_filename))
  hru_sf <- sf::st_transform(hru_sf, sf::st_crs(stations_sf))

  gages_sf <- sf::st_read(file.path(gis_dir, gages_filename))
  gages_sf <- sf::st_transform(gages_sf, sf::st_crs(stations_sf))

  # CLASSIFY STATIONS --------------------------------------------------------
  # as within, near, or outside

  # Check if the shapefile has valid geometry and fix it if necessary
  if (sum(sf::st_is_valid(hru_sf)) < nrow(hru_sf)) {
    cat("HRU shapefile has invalid geometry. Attempting to fix...\n")
    hru_sf <- sf::st_make_valid(hru_sf)
  }

  # Create a buffer around the polygons to detect near points
  shape_buffered <- sf::st_buffer(hru_sf, dist = buffer_dist)

  # Classify stations as "within" (if they intersect the polygon)
  intersects <- sf::st_intersects(stations_sf, hru_sf, sparse = FALSE)
  stations_sf$category <- ifelse(rowSums(intersects) > 0, "within", "outside")

  # Classify stations as "near"
  near_stations <- sf::st_intersects(
    stations_sf, shape_buffered, sparse = FALSE)

  # Update the category to "near" if a station is within the buffers
  stations_sf$category[stations_sf$category == "outside" &
                         rowSums(near_stations) > 0] <- "near-outside"
  stations_sf$category[stations_sf$category == "within" &
                         rowSums(near_stations) > 0] <- "near-within"

  # PLOT RESULTS --------------------------------------------------------

  # Factorize the category column to ensure all categories are always present
  stations_sf$category <- factor(
    stations_sf$category, levels = c(
      "outside", "near-outside","near-within", "within"))

  # Retrieve the bounding box (extent) of the shapefile
  bbox <- sf::st_bbox(shape_buffered)  # Use the buffered shapefile

  # Optionally extend the bounding box for better visualization
  bbox_ext <- bbox
  bbox_ext[1] <- bbox[1] - (bbox[3] - bbox[1]) * map_extent_fac  # xmin
  bbox_ext[2] <- bbox[2] - (bbox[4] - bbox[2]) * map_extent_fac  # ymin
  bbox_ext[3] <- bbox[3] + (bbox[3] - bbox[1]) * map_extent_fac  # xmax
  bbox_ext[4] <- bbox[4] + (bbox[4] - bbox[2]) * map_extent_fac  # ymax

  # Create the plot
  plot <- ggplot() +
    geom_sf(data = hru_sf, fill = "transparent", color = "darkgray") +  # Plot shapefile
    geom_sf_text(data = hru_sf, aes(label = model_hru_),
                 size = 3, color = "black")+
    geom_sf(data = stations_sf, aes(color = category), size = 3) +
    geom_sf_text(data = stations_sf, aes(label = site_num),
                 size = 3, vjust = -0.75, hjust = 0, color = "black")+
    geom_sf(data = gages_sf, size = 3, color = "black") +
    geom_sf_text(data = gages_sf, aes(label = gage_id),
                 size = 3, vjust = -0.75, hjust = 0, color = "black")+
    scale_color_manual(values = c("outside" = "red",
                                  "near-outside" = "goldenrod1",
                                  "near-within" = "palegreen2",
                                  "within" = "green4"),
                       name = "SNOTEL Category",
                       drop = FALSE, # missing categoreies are still included
                       labels = c("Outside",
                                  "Near Outside Boundary",
                                  "Near Inside HRU Boundary",
                                  "Within")) +  # Add custom legend labels
    theme_minimal() +
    labs(title = "Station Locations Relative to Shapefile Boundaries",
         x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(bbox_ext[1], bbox_ext[3]),
             ylim = c(bbox_ext[2], bbox_ext[4])) +  # Limit to extended bbox
    theme(legend.position = "right")  # Position the legend on the right

  # Print the plot
  print(plot)

  # Save the plot as PNG
  plot_path <- file.path(plot_dir, plot_filename)
  file_exists_flag <- file.exists(plot_path)

  if (option_save & !file_exists_flag){
    ggsave(filename = plot_path,
           plot = plot, width = 6.5, height = 6.5, dpi = 300)
    cat("Stations plot has been saved as",
        file.path(plot_dir, plot_filename), " \n")
  }
  else if (option_save & file_exists_flag & option_overwrite){
    ggsave(filename = plot_path,
           plot = plot, width = 6.5, height = 6.5, dpi = 300)
    cat("Plot overwritten")
    cat("Stations plot has been saved as",
        file.path(plot_dir, plot_filename), " \n")
  }

  return(plot)

}



#' Plot USGS stream gages in your catchment
#'
#' Maps USGS stations given user specified PRMS legacy shapefiles.
#'
#' @param dir_root character: parent working directory path
#' @param project_name character: project name
#' @param hru_filename character: file name of the hru .shp file
#' @param gages_filename character: file name of the gages .shp file
#' @param map_extent_frac numeric: fraction of the overall bounding box to
#' extend the map view. Useful for seeing stations outside of the catchment.
#' @param plot_filename: character: name of the plot with .png extension
#' @return ggplot: plot object
#' @examples
#'
#' plot_usgs("/Users/me/mywork/",
#'             "east_river")
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import sf
plot_usgs <- function(dir_root,
                      project_name,
                      hru_filename = "model_nhru.shp",
                      gages_filename = "model_npoigages.shp",
                      seg_filename = "model_nsegment.shp",
                      map_extent_fac = 0.1,
                      plot_filename = "plot_usgs_stations.png",
                      option_save = TRUE,
                      option_overwrite = FALSE){

  directories <- fm_trial_set(dir_root, project_name, 1)
  gis_dir <- directories["dir_gis"]
  plot_dir <- directories["dir_plot"]

  # READ .SHP -----------------------------------------------------

  wgs84_crs = "+proj=longlat +datum=WGS84 +no_def"

  # Read the shapefile using sf (ensure it's in the same CRS)
  hru_sf <- sf::st_read(file.path(gis_dir,hru_filename))%>%
    st_transform(sf::st_crs(wgs84_crs))
  gages_sf <- sf::st_read(file.path(gis_dir, gages_filename))%>%
    st_transform(sf::st_crs(wgs84_crs))
  seg_sf <- sf::st_read(file.path(gis_dir, seg_filename))%>%
    st_transform(sf::st_crs(wgs84_crs))

  # PLOT RESULTS --------------------------------------------------------

  bbox <- sf::st_bbox(hru_sf)

  # Optionally extend the bounding box for better visualization
  bbox_ext <- bbox
  bbox_ext[1] <- bbox[1] - (bbox[3] - bbox[1]) * map_extent_fac  # xmin
  bbox_ext[2] <- bbox[2] - (bbox[4] - bbox[2]) * map_extent_fac  # ymin
  bbox_ext[3] <- bbox[3] + (bbox[3] - bbox[1]) * map_extent_fac  # xmax
  bbox_ext[4] <- bbox[4] + (bbox[4] - bbox[2]) * map_extent_fac  # ymax

  # Create the plot
  plot <- ggplot() +
    # HRUS
    geom_sf(data = hru_sf, fill = "transparent", color = "darkgray") +
    # GAGES
    geom_sf(data = gages_sf, size = 3, color = "black") +
    geom_sf_text(data = gages_sf, aes(label = gage_id),
                 size = 3, vjust = -0.75, hjust = 0, color = "black")+
    # SEGMENTS
    geom_sf(data = seg_sf, size = 3, color = "cyan3") +
    geom_sf_text(data = seg_sf, aes(label = model_seg_),
                 size = 3, vjust = -0.75, hjust = 0, color = "black")+
    theme_minimal() +
    labs(title = "USGS Stream Gage Locations",
         x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(bbox_ext[1], bbox_ext[3]),
             ylim = c(bbox_ext[2], bbox_ext[4])) +  # Limit to extended bbox
    theme(legend.position = "right")  # Position the legend on the right

  # Print the plot
  print(plot)

  # Save the plot as PNG
  plot_path <- file.path(plot_dir, plot_filename)
  file_exists_flag <- file.exists(plot_path)

  if (option_save & !file_exists_flag){
    ggsave(filename = plot_path,
           plot = plot, width = 6.5, height = 6.5, dpi = 300)
    cat("Stations plot has been saved as",
        file.path(plot_dir, plot_filename), " \n")
  }
  else if (option_save & file_exists_flag & option_overwrite){
    ggsave(filename = plot_path,
           plot = plot, width = 6.5, height = 6.5, dpi = 300)
    cat("Plot overwritten")
    cat("Stations plot has been saved as",
        file.path(plot_dir, plot_filename), " \n")
  }

  return(plot)

}



#' Plot eta* sensitivity heatmap
#'
#' Uses pre-set dataframe to plot a rough heatmap
#'
#' @param df_eta data.frame: From the sensi_eet function output, construct
#' a dataframe of the bootstrap mean eta.star.
#' @param type1_params list: (optional) From the fit_logis output, input the
#' list element named 'type1_params'
#' @param type2_params list: (optional) From the fit_logis output, input the
#' list element named 'type2_params'
#' @param param_attributes data.frame: From the fm_set_trial or fm_create_trial
#' output, use the object named 'param_attributes'
#' @param dir_plot character: From the fm_set_trial or fm_create_trial
#' output, use the object named 'dir_plot'
#' @param plot_filename: character: name of the plot with .png extension
#' @return ggplot: plot object
#' @export
#' @import ggplot2
plot_heatmap_eta <- function(
    df_eta,
    type1_params = NULL,
    type2_params = NULL,
    param_attributes,
    dir_plot,
    plot_filename = "plot_sensi_heatmap.png") {
  # Load necessary libraries
  library(ggplot2)
  library(RColorBrewer)
  library(tidyr)
  library(dplyr)


  # CHECKS ------------------------------------------------------------------

  # Check if the output plot file already exists
  if (file.exists(file.path(dir_plot, plot_filename))) {
    stop(paste("The file:", plot_filename,
               "already exists. Aborting to avoid overwriting."))
  }

  if (!is.null(type1_params) || !is.null(type2_params)){
    warning("Both error types must be input for them to show.")
  }



  # GGPLOT PLOTTING --------------------------------------------------------

  # # Set color palette and breaks for the heatmap
  # palette_name <- "YlGnBu"
  # colors <- RcolorBrewer::brewer.pal(n = 6, name = palette_name)
  # breaks <- seq(0, 1, by = 0.2)
  #
  # # Replace underscores with spaces in column names (for better readability in the plot)
  # colnames(df) <- gsub("_", " ", colnames(df))
  #
  # # Convert the dataframe to a long format suitable for ggplot2
  # df_long <- df %>%
  #   rownames_to_column(var = "Params") %>%
  #   pivot_longer(cols = -Params, names_to = "Metrics", values_to = "η*")
  #
  # # # Create bins for the η* values to be used in the plot
  # # df_long$bin <- cut(df_long$`η*`, breaks = breaks, include.lowest = TRUE, right = FALSE)
  #
  # # Generate the heatmap using ggplot2
  # p <- ggplot(df_long, aes(x = Metrics, y = Params, fill = `η*`)) +
  #   geom_tile(color = "white", linewidth = 0.5, width = 0.667) +  # Adjust tile width
  #   scale_fill_stepsn(name = "η*", colours = colors, guide = "coloursteps", breaks = breaks) +
  #   theme_minimal() +
  #   labs(x = "Output Metrics", y = "Parameter Name") +
  #   theme(
  #     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
  #     axis.text.y = element_text(size = 12),
  #     axis.title.x = element_text(size = 14),
  #     axis.title.y = element_text(size = 14),
  #     legend.title = element_text(size = 14),
  #     legend.text = element_text(size = 12)
  #   )
  #
  # # Save the plot as PNG
  # ggsave(filename = file.path(dir_plot, plot_filename),
  #        plot = p, width = 10, height = 8, dpi = 300)
  # cat("Heatmap has been saved as", file.path(dir_plot, plot_filename), " \n")


  # The same param_names vector was used to create the SA runs!!!

  # df_eta$param_names <- colnames(morris_design_Taylor$X) #convert to factor

  df_eta$param_names <- param_attributes$names

  df_eta <- df_eta %>%
    dplyr::mutate(
      param_names = factor(
        param_names, levels = rev(sort(unique(param_names)))))
  # df_eta$param_groups <- c("Climate","Climate","Climate","Climate","Climate",
  #                          "Climate","Climate",
  #                          "Solar", "Solar", "Solar", "Solar", "Solar",
  #                          "Solar", "Solar",
  #                          "PET", "PET",
  #                          "Intcp.", "Intcp.", "Intcp.", "Intcp.", "Intcp.",
  #                          "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
  #                          "Snow", "Snow", "Snow", "Snow", "Snow", "Snow",
  #                          "Runoff", "Runoff", "Runoff", "Runoff", "Runoff",
  #                          "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
  #                          "Soil", "Soil", "Soil", "Soil", "Soil", "Soil",
  #                          "G")
  df_eta$param_groups <- param_attributes$modules

  # Pivot the dataframe
  df_eta_long <- df_eta %>%
    dplyr::pivot_longer(
      cols = -c(param_names, param_groups),
      names_to = "col_name",
      values_to = "eta.star"
    )

  if (!is.null(type1_params) & !is.null(type2_params)){
    error_data <- df_eta_long %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        error_marker = case_when(
          param_names %in% (type1_params[[col_name]] %||% character(0)) ~ "■",
          param_names %in% (type2_params[[col_name]] %||% character(0)) ~ "▲",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(col_name, param_names, error_marker)  # Keep only relevant columns

    df_eta_long <- df_eta_long %>%
      dplyr::left_join(error_data, by = c("param_names", "col_name"))
  }


  # # Split the column names into components
  # df_eta_long <- df_eta_long %>%
  #   separate(
  #     col_name,
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
  #
  # # ERROR DATA - REPEAT THE SAME STEPS SO THE ERRORS ARE ALIGNED
  # # Split the column names into components
  # error_data <- error_data %>%
  #   separate(
  #     col_name,
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
  # error_data <- error_data %>%
  #   mutate(Basin_Site = gsub("_", " ", Basin_Site)) # Replace "_" with " "

  # Define custom order for facets
  df_eta_long <- df_eta_long %>%
    dplyr::mutate(
      param_groups = factor(
        param_groups, levels = c(
          "Climate", "Solar", "PET","Snow","Intception", "Runoff",
          "Soil", "Groundwater"))
    )

  palette_name <- "YlGnBu"
  colors4plot <- RColorBrewer::brewer.pal(n = 6, name = palette_name)
  breaks <- seq(0, 1, by = 0.2)

  p <- ggplot(
    df_eta_long, aes(x = col_name, y = param_names, fill = eta.star)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_stepsn(
      name = "η*", colours = colors4plot,
      guide = "coloursteps", breaks = breaks) +
    facet_grid(
      rows = vars(param_groups), scales = "free", space = "free") +
    labs(x = "Observations", y = "Parameters") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(vjust = 0.5),
      strip.text.x = element_text(size = 9, face = "bold"),
      strip.text.y = element_text(size = 9, face = "bold")
    )

  if (!is.null(type1_params) & !is.null(type2_params)){
    p <- p + geom_text(
      aes(label = error_marker),
      vjust = 0.5, size = 2, na.rm = TRUE
    )
  }

  ggsave(
    filename = file.path(dir_plot, plot_filename),
    plot = p,
    width = 6.5, height = 6.5, units = "in", dpi = 300
  )
  cat("Heatmap has been saved as", file.path(dir_plot, plot_filename), " \n")

  return(p)
}
