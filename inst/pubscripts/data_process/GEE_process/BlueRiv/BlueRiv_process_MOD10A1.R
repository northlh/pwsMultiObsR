

# This script is intended to read in raw data and perform neccessary:
#   - screening
#   - masking
#   - transformations/retrsructuring
#   - unit conversions

# EastRiv SMAP L4

source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/output_process/addWaterDay.R")

library(sf)
library(terra)
library(tidyverse)
library(dataRetrieval)
library(hydroGOF)


# This function is designed to accept a raster stack from GEE and aggregate
# by HRUs, given a hru shapefile


pws.reducemean.gee <- function(data_path,
                               hru_path){
  
  # Read in raster and shapefile
  rast_stack <- terra::rast(data_path)
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
  
  # Loop through each layer and extract data
  data_list <- list()
  
  for (i in 1:nlyr(rast_stack)){
    # Extract layer
    layer <- rast_stack[[i]]
    # Initialize
    hru_averages <- numeric(nrow(hru_sf))
    
    for(j in seq_len(nrow(hru_sf))){
      #this 2 is the second layer, the model_hru_
      hru <- hru_sf[j,2]
      # Mask each raster by each HRU
      layer_masked <- terra::crop(x = layer,
                                  y = hru,
                                  mask = TRUE,
                                  touches = TRUE) # no effect?
      
      # Calculate the areal mean, excluding NAs from the crop
      hru_averages[j] <- global(layer_masked, fun = "mean", na.rm = TRUE)[[1]]
      
    }
    # Combine
    data_list[[i]] <- hru_averages
    
    # Run counter
    #print(run)
    cat("\rProcessing layer:", i, "/", nlyr(rast_stack))
  }
  
  # This is our df created in R and terra
  data_df <- as.data.frame(do.call(rbind, data_list))
  colnames(data_df) <- c(paste0("hru_", seq_len(ncol(data_df))))
  
  # Add the dates extracted from layer names as a column
  data_df$Date <- image_dates
  
  # # Add date column
  # #n_layers <- nlyr(rast_stack)
  # data_df$Date <- seq.Date(
  #   from = as.Date(start_date), 
  #   by = time_step, 
  #   length.out = nrow(data_df)
  # )
  
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


# HERE IS WHERE THE DATA IS PROCESSED AND PUT TOGETHER

data_path1 <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/BlueRiv/MOD10A1061_BlueRiv_NDSI_raw_20000224_20120930.tif"
data_path2 <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/BlueRiv/MOD10A1061_BlueRiv_NDSI_raw_20121001_20240930.tif"
#data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/EastRiv/MOD10A1/MOD10A1061_EastRiv_NDSI_raw_20241005_20241105.tif"
hru_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_nhm_BlueRiv/GIS/model_nhru.shp"
seg_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_nhm_BlueRiv/GIS/model_nsegment.shp"

plot(st_read(seg_path)[2])

print(paste0("CRS for raster:", terra::crs(terra::rast(data_path1))))

MOD10A1_data_df1 <- pws.reducemean.gee(data_path = data_path1,
                                       hru_path = hru_path)

MOD10A1_data_df2 <- pws.reducemean.gee(data_path = data_path2,
                                       hru_path = hru_path)

combined_df <- rbind(MOD10A1_data_df1, MOD10A1_data_df2)


# This is an areal average NDSI, which is not the same as fSCA.
# However, based on recent literature referencing the C6 products
# the algorithms developed in Solomonson and Appel, 2004 
# https://www.sciencedirect.com/science/article/pii/S0034425703002864
# remain appropriate to use
# see section 2.1 in https://hess.copernicus.org/articles/23/5227/2019/
# and section 4.1 in https://tc.copernicus.org/articles/17/567/2023/

# We use the recommended linear algorithm (MB3U) for collection 6 products

# fSCA = 0.06 + 1.21*NDSI

# and apply to to values greater than zero 

# because this transformation is linear and additive, it can be applied after
# taking an areal average since these operations are distributive through
# summation. This would not be true for non-linear transformations, where it
# would need to be applied to each data point (pixel) prior to averaging.


# This is to convert 0 to 100 NDSI into 0 to 1
normalize <- function(x){
  x / 100
}

# This applied the transfomration where x ranges from 0 to 1
MB3U_transform <- function(x){
  ifelse(x > 0, 0.06 + 1.21*x, x)
}

final_df <- combined_df %>%
  dplyr::mutate(across(starts_with("hru_"), ~ MB3U_transform(normalize(.))))

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/BlueRiv/MOD10A1_data_df.csv"

write.csv(final_df, save_path, row.names = FALSE)









# # Remove columns by index
# MOD10A1_data_df1 <- MOD10A1_data_df1[, -c(1,2,3)] # Removes the first through third columns
# 
# # Read in raster and shapefile
# rast_stack1 <- terra::rast(data_path1)
# 
# # Extract layer names as dates
# layer_names1 <- names(rast_stack1)
# image_dates1 <- as.Date(sub("^(\\d{4}_\\d{2}_\\d{2}).*", "\\1", layer_names1), format = "%Y_%m_%d")
# 
# MOD10A1_data_df1$Date <- image_dates1
# MOD10A1_data_df1 <- MOD10A1_data_df1 %>%
#   relocate(Date, .before = starts_with("hru_")) # Move 'Date' to the first position
# 
# # Add other relevant dates
# MOD10A1_data_df1 <- addWaterYear(MOD10A1_data_df1)
# 
# # Add water day (custom function)
# MOD10A1_data_df1 <- addWaterDay(df = MOD10A1_data_df1,
#                        date_colname = Date,
#                        wy_colname = waterYear)
# 
# 
# 
# 
# 
# 
# 
# # Remove columns by index
# MOD10A1_data_df2 <- MOD10A1_data_df2[, -c(1,2,3)] # Removes the first through third columns
# 
# # Read in raster and shapefile
# rast_stack2 <- terra::rast(data_path2)
# 
# # Extract layer names as dates
# layer_names2 <- names(rast_stack2)
# image_dates2 <- as.Date(sub("^(\\d{4}_\\d{2}_\\d{2}).*", "\\1", layer_names2), format = "%Y_%m_%d")
# 
# MOD10A1_data_df2$Date <- image_dates2
# MOD10A1_data_df2 <- MOD10A1_data_df2 %>%
#   relocate(Date, .before = starts_with("hru_")) # Move 'Date' to the first position
# 
# # Add other relevant dates
# MOD10A1_data_df2 <- addWaterYear(MOD10A1_data_df2)
# 
# # Add water day (custom function)
# MOD10A1_data_df2 <- addWaterDay(df = MOD10A1_data_df2,
#                                 date_colname = Date,
#                                 wy_colname = waterYear)


