library(sf)
library(terra)
library(tidyverse)
library(dataRetrieval)
library(hydroGOF)


# This function is designed to accept a raster stack from GEE and aggregate
# by HRUs, given a hru shapefile


pws.reduceMean.MOD10A1 <- function(raster_path,
                                   hru_path){
  
  # Read in raster and shapefile
  rast_stack <- terra::rast(raster_path)
  hru_sf <- sf::st_read(hru_path)
  
  # Extract layer names as dates
  layer_names <- names(rast_stack)
  image_dates <- as.Date(sub("^(\\d{4}_\\d{2}_\\d{2}).*", "\\1", layer_names),
                         format = "%Y_%m_%d")
  
  # Check for invalid geometry in the sf
  invalid_geometries <- hru_sf[!st_is_valid(hru_sf),]
  if (nrow(invalid_geometries) > 0) {
    hru_sf <- st_make_valid(hru_sf)
  }
  
  # Transform the sf crs onto that of the raster
  print(paste0("CRS for raster:", terra::crs(rast_stack))) # should be 4326 from the GEE export
  hru_sf <- st_transform(hru_sf, crs = terra::crs(rast_stack))
  
  
  # Call the reduceMean function
  data_df <- pws.reduceMean(rast_stack = rast_stack,
                 hru_sf = hru_sf)
  
  # Add the dates extracted from layer names as a column
  data_df$Date <- image_dates
  
  # Re-position Date to the first column
  data_df <- data_df %>%
    relocate(Date, .before = starts_with("hru_")) # Move 'Date' to the first position
  
  # Add other relevant dates
  data_df <- addWaterYear(data_df)
  
  # Add water day (custom function)
  data_df <- addWaterDay(df = data_df,
                         date_colname = Date,
                         wy_colname = waterYear)
  
  
  return(data_df)
  
} #close fn
