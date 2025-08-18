

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

# HERE IS WHERE THE DATA IS PROCESSED AND PUT TOGETHER

data_path1 <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/TaylorRiv/MOD10A1061_TaylorRiv_NDSI_raw_20000224_20120930.tif"
data_path2 <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/TaylorRiv/MOD10A1061_TaylorRiv_NDSI_raw_20121001_20240930.tif"
#data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/EastRiv/MOD10A1/MOD10A1061_EastRiv_NDSI_raw_20241005_20241105.tif"
hru_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_inputs/TaylorRiv/GIS/model_nhru.shp"


MOD10A1_data_df1 <- pws.reduceMean.MOD10A1(raster_path = data_path1,
                                           hru_path = hru_path)

MOD10A1_data_df2 <- pws.reduceMean.MOD10A1(raster_path = data_path2,
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

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/TaylorRiv/MOD10A1_data_df.csv"

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


