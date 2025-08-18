
pws.plot.basin <- function(basin_name,
                           gis_dir,
                           obs_dir,
                           USGS_station_ids = NULL, 
                           NRCS_station_ids = NULL,
                           hru_filename = "model_nhru.shp",
                           gages_filename = "model_npoigages.shp",
                           seg_filename = "model_nsegment.shp",
                           NRCS_data_path = file.path(here(),"/source/data_process/NRCS_process/StationMetadata_clean.csv"),
                           utm_crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs",
                           wgs84_crs = "+proj=longlat +datum=WGS84 +no_def",
                           elev_units = "imperial",
                           elev_min = NULL,
                           elev_max = NULL,
                           flag_legend = 1,
                           scale_pos = "tr"){
  
  # READ METADATA -----------------------------------------------------------
  
  NRCS_data <- read.csv(NRCS_data_path) %>%
    select(site_num, site_name, lat, lon) #%>%
  #st_as_sf(coords = c("lon", "lat"), crs = wgs84_crs)
  
  
  
  # READ HRU .SHP -----------------------------------------------------
  
  hru_sf <- st_read(file.path(gis_dir, hru_filename)) %>%
    st_transform(st_crs(wgs84_crs))
  
  # Check for invalid geometry in the sf
  invalid_geometries <- hru_sf[!st_is_valid(hru_sf),]
  if (nrow(invalid_geometries) > 0) {
    hru_sf <- st_make_valid(hru_sf)
  }
  
  # Get basin extent in UTM for cropping DEM
  basin_extent_utm <- st_union(hru_sf) %>%
    st_transform(crs = utm_crs) #in utm for extracting 
  
  basin_extent_terra <- terra::vect(basin_extent_utm)
  
  
  
  # READ GAGES .SHP -----------------------------------------------------
  
  # HARDCODED TO CO
  if (is.null(USGS_station_ids)){USGS_station_ids <- fromJSON(
    file.path(obs_dir,"USGS/USGS_stations.json"))[["site_ids"]]}
  
  gages_sf <- st_read(file.path(gis_dir, gages_filename)) %>%
    st_transform(st_crs(wgs84_crs)) %>%
    mutate(station_id = USGS_station_ids)
  
  
  
  # READ SEG .SHP -----------------------------------------------------
  
  seg_sf <- st_read(file.path(gis_dir, seg_filename)) %>%
    st_transform(st_crs(wgs84_crs))
  
  
  
  # Define NRCS stations to plot ---------------------------------------------
  
  # HARDCODED TO CO
  if (is.null(NRCS_station_ids)){NRCS_station_ids <- fromJSON(
    file.path(obs_dir,"SNOTEL/NRCS_stations.json"))[["CO"]][["site_ids"]]}
  
  NRCS_coords <- NRCS_data[NRCS_data$site_num %in% NRCS_station_ids,c(3,4)]
  NRCS_ids_actual <- NRCS_data[NRCS_data$site_num %in% NRCS_station_ids,1]
  
  NRCS_coords_sf <- NRCS_coords %>%
    st_as_sf(coords = c("lon", "lat"), crs = wgs84_crs) %>%
    mutate(station_id = NRCS_ids_actual)
  
  
  
  
  # LOAD DEM ------------------------------------------------------------------
  # Function to calculate ground resolution for a given latitude and zoom level
  calc.ground.res <- function(lat, z) {
    (cos(lat * (pi / 180)) * 2 * pi * 6378137) / (256 * (2 ^ (z + 1)))
  }
  
  # Function to find the minimum zoom level with ground resolution below 50 meters
  # z ranges from 1 to 14
  find.min.zoom <- function(lat, target_res = 50) {
    z <- 1
    while (z <= 14) {
      if (calc.ground.res(lat, z) < target_res) {
        return(z)
      }
      z <- z + 1
    }
    return(14) # Return max zoom if none found below target resolution
  }
  
  # Get DEM
  avg_lat <- mean(NRCS_coords$lat)
  min_zoom <- find.min.zoom(avg_lat)  # Assuming find.min.zoom is defined
  
  # Download DEM using UTM-projected basin extent
  rast_dem <- elevatr::get_elev_raster(basin_extent_utm, z = min_zoom)
  rast_dem <- methods::as(rast_dem, "SpatRaster")  # Convert to SpatRaster
  
  # Mask DEM to basin extent
  rast_dem_masked <- terra::mask(rast_dem, basin_extent_terra)
  
  # Transform DEM to WGS84 for plotting
  rast_dem_masked_wgs84 <- terra::project(rast_dem_masked, terra::crs(wgs84_crs))
  
  # Convert masked DEM to a data frame for ggplot
  dem_df <- as.data.frame(rast_dem_masked_wgs84, xy = TRUE, na.rm = TRUE)
  colnames(dem_df) <- c("x", "y", "elevation")
  #dem_df$elevation <- dem_df$elevation*3.28084 #convert to ft
  
  assign("dem_df", dem_df, envir = .GlobalEnv) # bad behavior, just to get
  
  # Compute elevation range if not provided
  if (is.null(elev_min)){ elev_min <- min(dem_df$elevation, na.rm = TRUE)}
  if (is.null(elev_max)){ elev_max <- max(dem_df$elevation, na.rm = TRUE)}
  
  
  
  # PLOTTING WITH GGPLOT ------------------------------------------------------
  
  print(nrow(gages_sf))  # Should be >0
  print(nrow(NRCS_coords_sf))  # Should be >0
  print(nrow(seg_sf))  # Should be >0
  
  library(ggspatial)
  library(ggrepel)
  
  # Define dynamic labeling improvements
  bbox <- st_bbox(hru_sf)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  
  x_breaks <- pretty(c(bbox$xmin, bbox$xmax), n = 2)
  y_breaks <- pretty(c(bbox$ymin, bbox$ymax), n = 2)
  
   p <- ggplot() +
    # Plot DEM using cubehelix color palette
    geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
    scale_fill_gradientn(colors = pals::cubehelix(n = 100),
                         limits = c(elev_min, elev_max)) +
    
    # Overlay HRU boundaries
    geom_sf(data = hru_sf, fill = NA, color = "white", size = 0.75) +
    
    # Overlay stream segments
    geom_sf(data = seg_sf, color = "cyan", size = 1) +
    
    # Overlay NRCS station points and labels
    geom_sf(data = NRCS_coords_sf, color = "black", shape = 24, size = 2,
            fill = "black") +
    
    geom_text_repel(data = NRCS_coords_sf,
                    aes(x = st_coordinates(NRCS_coords_sf)[,1],
                        y = st_coordinates(NRCS_coords_sf)[,2],
                        label = station_id),
                    size = 2.5,
                    color = "black",
                    max.overlaps = Inf,
                    box.padding = 0.25,
                    point.padding = 0.3,
                    segment.color = "black",
                    segment.size = 0.3) +
    
    # Overlay USGS station points and label by station ID
    geom_sf(data = gages_sf, color = "black", shape = 21, size = 2,
            fill = "black") +
    
    geom_text_repel(data = gages_sf,
                    aes(x = st_coordinates(gages_sf)[,1],
                        y = st_coordinates(gages_sf)[,2],
                        label = station_id),
                    size = 2.5,
                    color = "black",
                    max.overlaps = Inf,
                    box.padding = 0.25,
                    point.padding = 0.3,
                    segment.color = "black",
                    segment.size = 0.3) +
    
    # scale_x_continuous(limits = c(bbox$xmin - 0.01, bbox$xmax + 0.01),
    #                    n.breaks = 3,
    #                    minor_breaks = NULL) +
    # scale_y_continuous(limits = c(bbox$ymin - 0.01, bbox$ymax + 0.01),
    #                    n.breaks = 3,
    #                    minor_breaks = NULL) +
     
    scale_x_continuous(limits = c(bbox$xmin - 0.01, bbox$xmax + 0.01),
                       breaks = x_breaks) +
    scale_y_continuous(limits = c(bbox$ymin - 0.01, bbox$ymax + 0.01),
                       breaks = y_breaks) +
    
    # Set coordinate system to WGS84
    coord_sf(crs = st_crs(wgs84_crs), expand = FALSE) +
    
    # Add scale bar and north arrow
    # Location # bl east, br blue, tl dolo, tr tay
    annotation_scale(location = scale_pos,
                     width_hint = 0.25,
                     height = unit(0.15, "cm"),
                     text_cex = 0.5,
                     unit_category = "metric") +   # Scale bar in bottom left (bl)
    # annotation_north_arrow(location = "tr", which_north = "true",
    #                        style = north_arrow_fancy_orienteering) +  # North arrow in top left
    
    # Add labels and title
    labs(x = "Longitude", y = "Latitude", fill = "Elevation (m)"#,
         #title = paste("Basin Map of", basin_name)
         , title = basin_name
    ) +
    
    # Apply a minimal theme for clean visualization
    theme_linedraw(base_size = 9) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = ifelse(flag_legend, "right", "none"))
  
   return(p)
} # close fn


# ---------------------------------------------------------------------------
# FOR PATCHWORK
# ---------------------------------------------------------------------------

# Can be used in a loop for many projects or trials
test_proj_names <- c("day_BlueRiv", "day_DoloresRiv",
                     "day_EastRiv", "day_TaylorRiv")

test_trial_nums <- c(1,1,1,1)

names_basin <- c("Blue", "Dolores", "East", "Taylor")

list_USGS <- list(day_BlueRiv = NULL,
                  day_DoloresRiv = NULL,
                  day_EastRiv = c(09112200,09112500), # originally wrong order
                  day_TaylorRiv = NULL)

flag_legend <- c(0,1,0,0)

scale_postion <- c("tr","tl","tr","tl")

plot_list <- list()

list_dem <- list()

for (i in 1:length(test_proj_names)){
 
  pws.set.trial(project_name = test_proj_names[i],
                trial_number =  test_trial_nums[i])
  
  p <- pws.plot.basin(
    basin_name = names_basin[i],
    USGS_station_ids = list_USGS[[i]],
    gis_dir = gis_dir,
    obs_dir = obs_dir,
    elev_min = 1800,
    elev_max = 4450,
    flag_legend = flag_legend[i],
    scale_pos = scale_postion[i]
    # any other parameters here
  )
  
  #print(p) 
  list_dem[[i]] <- dem_df # assigned in function above
  
  plot_list[[i]] <- p
}  

df_dem_combined <- do.call(rbind, list_dem) # tried to plot this :/


# Create the gridded plot
library(patchwork)
wrap_plots(plot_list, ncol = 2)

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/basin_patch2.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.75, units = "in", dpi = 300
)





# ATTEMPT TO PATCH THE KEYMAP IN

# Create empty plots to fill blank slots (like position 5)
empty_plot <- ggplot() + theme_void()

# Arrange manually in 3x2 grid
# Layout:
# [1] [2] [3]
# [4] [5] [6]
#          ^ keymap goes here

final_layout <- (
  plot_list[[1]] | plot_list[[2]] | empty_plot) /
  (plot_list[[3]] | plot_list[[4]]   | keymap)

final_layout

ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/basin_patch_key.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)








# ---------------------------------------------------------------------------
# Test floating
# ---------------------------------------------------------------------------



hru_east_path <- file.path(here(),"example_inputs/EastRiv/GIS/model_nhru.shp")
hru_blue_path <- file.path(here(),"example_inputs/BlueRiv/GIS/model_nhru.shp")
hru_dolores_path <- file.path(here(),"example_inputs/DoloresRiv/GIS/model_nhru.shp")
hru_taylor_path <- file.path(here(),"example_inputs/TaylorRiv/GIS/model_nhru.shp")
ucrb_path <- "/Users/lnorth/Desktop/RA/RECIEVED/UCRB_shp_scibase/bdy_ucrb_plus_LittleColoradoRiver_utm83/bdy_ucrb_plus_LittleColoradoRiver_utm83.shp"

# Read in, transform all to same crs
hru_east <- st_transform(st_make_valid(st_read(hru_east_path)), crs = 4326)
hru_blue <- st_transform(st_make_valid(st_read(hru_blue_path)), crs = 4326)
hru_dolores <- st_transform(st_make_valid(st_read(hru_dolores_path)), crs = 4326)
hru_taylor <- st_transform(st_make_valid(st_read(hru_taylor_path)), crs = 4326)
ucrb_raw <- st_transform(st_make_valid(st_read(ucrb_path)), crs = 4326)
# Get extents
hru_east_ext <- st_union(hru_east[1])
hru_blue_ext <- st_union(hru_blue[1])
hru_dolores_ext <- st_union(hru_dolores[1])
hru_taylor_ext <- st_union(hru_taylor[1])


# Get basic data
library(rnaturalearth)
# Get US and Colorado boundaries
us_boundaries <- ne_countries(scale = "large", returnclass = "sf")
co_boundaries <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Colorado")
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Tranform to crs
us_boundaries <- st_transform(us_boundaries, crs = 4326)
co_boundaries <- st_transform(co_boundaries, crs = 4326)
us_states <- st_transform(us_states, crs = 4326)
us_states_centroids <- st_centroid(us_states)


us_states_centroids <- st_point_on_surface(us_states)

# Add x/y columns for plotting
test_coords <- st_coordinates(us_states_centroids)
us_states_centroids$lon <- test_coords[,1]
us_states_centroids$lat <- test_coords[,2]




# Plotting the keymap
ggplot() +
  # Plot US boundaries
  geom_sf(data = us_states, fill = "white", color = "black", linetype = "solid")+
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = ucrb_raw, fill = NA, color = "blue", size = 8) +
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = hru_east_ext, color = "black", size = 4) +
  geom_sf(data = hru_blue_ext,  color = "black", size = 4) +
  geom_sf(data = hru_dolores_ext, fill = "red", color = "black", size = 4) +
  geom_sf(data = hru_taylor_ext, fill = "red", color = "black", size = 4) +
  
  
  geom_raster(data = list_dem[[1]], aes(x = x, y = y, fill = elevation)) +
  geom_raster(data = list_dem[[2]], aes(x = x, y = y, fill = elevation)) +
  geom_raster(data = list_dem[[3]], aes(x = x, y = y, fill = elevation)) +
  geom_raster(data = list_dem[[4]], aes(x = x, y = y, fill = elevation)) +
  
  scale_fill_gradientn(colors = pals::cubehelix(n = 100)) +
  
  # # Add a point label for the NRCS station
  # geom_text(data = butte_coord_sf, aes(x = st_coordinates(butte_coord_sf)[,1],
  #                                      y = st_coordinates(butte_coord_sf)[,2],
  #                                      label = "East Riv."
  #                                      ),
  #           nudge_y = 0.75, color = "red") +
  
  # # Set coordinate system to WGS84 FOR CONUS
  # coord_sf(crs = 4326, xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
  
  # Add state abbreviations
  # geom_text_repel(data = us_states_centroids,
  #                 aes(x = st_coordinates(us_states_centroids)[,1],
  #                     y = st_coordinates(us_states_centroids)[,2],
  #                     label = postal),
  #                 size = 3, color = "black", max.overlaps = 30) +
  
  geom_text_repel(data = us_states_centroids,
                  aes(x = us_states_centroids$lon,
                      y = us_states_centroids$lat,
                      label = postal),
                  size = 3, color = "black", max.overlaps = 30) +
  
  # Set coordinate system to WGS84 FOP UCRB
  coord_sf(crs = 4326, xlim = c(-109.5, -105.5), ylim = c(36.5, 40), expand = FALSE) +
  
  # Add labels and title
  labs(x = "Longitude", y = "Latitude",
       #title = "Watershed Context: East River Basin, CO"
  ) +
  
  # Apply a minimal theme
  theme_minimal(base_size = 9) +
  
  theme(axis.title = element_blank())
# Fix geometry

# Get extents

ggsave(
  filename = file.path(plot_dir,"basin_mapKEY.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 2.5, height = 5, units = "in"
)



# ---------------------------------------------------------------------------
# KEYMAP SMALL (ACTUALLY USED)
# ---------------------------------------------------------------------------



hru_east_path <- file.path(here(),"example_inputs/EastRiv/GIS/model_nhru.shp")
hru_blue_path <- file.path(here(),"example_inputs/BlueRiv/GIS/model_nhru.shp")
hru_dolores_path <- file.path(here(),"example_inputs/DoloresRiv/GIS/model_nhru.shp")
hru_taylor_path <- file.path(here(),"example_inputs/TaylorRiv/GIS/model_nhru.shp")
ucrb_path <- "/Users/lnorth/Desktop/RA/RECIEVED/UCRB_shp_scibase/bdy_ucrb_plus_LittleColoradoRiver_utm83/bdy_ucrb_plus_LittleColoradoRiver_utm83.shp"

# Read in, transform all to same crs
hru_east <- st_transform(st_make_valid(st_read(hru_east_path)), crs = 4326)
hru_blue <- st_transform(st_make_valid(st_read(hru_blue_path)), crs = 4326)
hru_dolores <- st_transform(st_make_valid(st_read(hru_dolores_path)), crs = 4326)
hru_taylor <- st_transform(st_make_valid(st_read(hru_taylor_path)), crs = 4326)
ucrb_raw <- st_transform(st_make_valid(st_read(ucrb_path)), crs = 4326)
# Get extents
hru_east_ext <- st_union(hru_east[1])
hru_blue_ext <- st_union(hru_blue[1])
hru_dolores_ext <- st_union(hru_dolores[1])
hru_taylor_ext <- st_union(hru_taylor[1])


# Get basic data
library(rnaturalearth)
# Get US and Colorado boundaries
us_boundaries <- ne_countries(scale = "large", returnclass = "sf")
co_boundaries <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Colorado")
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Tranform to crs
us_boundaries <- st_transform(us_boundaries, crs = 4326)
co_boundaries <- st_transform(co_boundaries, crs = 4326)
us_states <- st_transform(us_states, crs = 4326)
us_states_centroids <- st_centroid(us_states)


us_states_centroids <- st_point_on_surface(us_states)

# Add x/y columns for plotting
test_coords <- st_coordinates(us_states_centroids)
us_states_centroids$lon <- test_coords[,1]
us_states_centroids$lat <- test_coords[,2]

# Plotting the keymap
keymap <- ggplot() +
  # # Plot US boundaries
  # geom_sf(data = us_boundaries, fill = "lightgray", color = "black") +
  
  geom_sf(data = us_states, fill = "lightgray", color = "black", linetype = "solid")+
  
  # # Overlay Colorado boundaries
  # geom_sf(data = co_boundaries, fill = "white", color = "black") +
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = ucrb_raw, fill = NA, color = "blue", linewidth = 0.25) +
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = hru_east_ext, fill = "red", color = "black", size = 4) +
  geom_sf(data = hru_blue_ext, fill = "red", color = "black", size = 4) +
  geom_sf(data = hru_dolores_ext, fill = "red", color = "black", size = 4) +
  geom_sf(data = hru_taylor_ext, fill = "red", color = "black", size = 4) +
  
  # # Add a point label for the NRCS station

  
  # Add state abbreviations
  geom_text_repel(data = us_states_centroids,
                  aes(x = us_states_centroids$lon,
                      y = us_states_centroids$lat,
                      label = postal),
                  size = 1.5, color = "black", max.overlaps = 30,
                  fontface = "bold") +
  
  # Set coordinate system to WGS84 FOP UCRB
  coord_sf(crs = 4326, xlim = c(-112.5, -105.5), ylim = c(35.5, 43.5), expand = FALSE) +
  
  scale_x_continuous(breaks = c(-112, -109, -106)) +
  scale_y_continuous(breaks = c(36, 38, 40, 42)) +
  
  # Add labels and title
  labs(x = "Longitude", y = "Latitude") +
  
  # Apply a minimal theme
  theme_minimal(base_size = 9) +
  theme(axis.title = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.3))

print(keymap)


ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/basin_key2.png"),   # Save as PDF
  plot = keymap,                       # Replace with your plot object
  width = 1.75, height = 2.5, units = "in"
)



# ---------------------------------------------------------------------------
# Test overall DEM style
# ---------------------------------------------------------------------------


# Define a bounding box (xmin, ymin, xmax, ymax)
bbox_CO <- c(xmin = -112.5, ymin = 35.5, xmax = -105.5, ymax = 43.5)

bbox_CO <- c(xmin = -109, ymin = 37, xmax = -105.5, ymax = 40)
bbox_poly_CO <- st_as_sfc(st_bbox(bbox_CO, crs = st_crs(4326)))
bbox_sf_CO <- st_sf(geometry = bbox_poly_CO)

# Get elevation raster (adjust z to control resolution: lower z = coarser)
elev_CO <- get_elev_raster(locations = bbox_sf_CO, z = 8, clip = "bbox")
elev_CO <- methods::as(elev_CO, "SpatRaster")

# Convert to df for plotting
df_dem_CO <- as.data.frame(elev_CO, xy = TRUE, na.rm = TRUE)
colnames(df_dem_CO) <- c("x", "y", "elevation")

library(pals)

# Plotting the keymap
ggplot() +
  
  # First layer is DEM
  geom_raster(data = df_dem_CO, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colors = pals::cubehelix(n = 100)) +
  #scale_fill_gradientn(colors = pals::brewer.blues(100)) +
  #scale_fill_gradient(low = "white", high = "black") +
  
  # Plot US boundaries
  geom_sf(data = us_states, fill = NA, color = "black", linetype = "solid")+
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = ucrb_raw, fill = NA, color = "blue", linewidth = 1) +
  
  # Overlay basin extent (Butte coordinate for simplicity)
  geom_sf(data = hru_east_simple, fill = NA, color = "red", linewidth = 1) +
  geom_sf(data = hru_blue_ext, fill = NA, color = "red", linewidth = 1) +
  geom_sf(data = hru_dolores_ext, fill = NA, color = "red", linewidth = 1) +
  geom_sf(data = hru_taylor_ext, fill = NA,, color = "red", linewidth = 1) +
  
  # 
  # geom_text_repel(data = us_states_centroids,
  #                 aes(x = lon,
  #                     y = lat,
  #                     label = postal),
  #                 size = 3, color = "black", max.overlaps = 30) +
  
  # Set coordinate system to WGS84 FOP UCRB
  coord_sf(crs = 4326, xlim = c(-109, -105.5), ylim = c(37, 40), expand = FALSE) +
  
  # Add labels and title
  labs(x = "Longitude", y = "Latitude") +
  
  annotation_scale(location = "br",
                   width_hint = 0.25,
                   height = unit(0.25, "cm"),
                   text_cex = 1,
                   unit_category = "metric") +   # Scale bar in bottom left (bl)
  
  # Apply a minimal theme
  theme_minimal(base_size = 9) +
  
  theme(axis.title = element_blank())






