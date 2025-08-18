
# Script to check the ee.ReduceRegions function
# Supposed to be comparable to mask
library(sf)
library(terra)
library(tidyverse)
library(hydroGOF)

# wd <- "/Users/lnorth/Desktop/RA/RECIEVED/GEE/OpenET"
# setwd(wd)


# Read in files
#hru_sf <- st_read("/Users/lnorth/Desktop/pywatershed-SA-UA-main/EastRiv_example_GIS/model_nhru.shp")
hru_sf <- st_read("/Users/lnorth/Desktop/RA/R/GEE_workflow/st_transform_sf/DoloresRiv/12n13only/Dolores_hru_4326_3.shp")
# openet_stack <- terra::rast("/Users/lnorth/Desktop/RA/RECIEVED/GEE_raw/EastRiv/OpenET/OpenET_EastRiv_mad_raw_20130101_20131201.tif")
# openet_stack <- terra::rast("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/OpenET_Dolores1213Riv_mad_raw_20130101_20131201.tif")

# openet_stack <- terra::rast("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/OpenET_Dolores1213Riv_mad_raw_20130101_20231201-0000000000-0000000000.tif")
# nlyr(openet_stack)
# openet_stack <- terra::rast("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/OpenET_Dolores1213Riv_mad_raw_20130101_20231201-0000000000-0000003072.tif")
# nlyr(openet_stack)


# It appears that these 12s_ layers in the data were causing the issues


# Set target crs and transform
# target_crs_sf <- st_crs(4326)  # For sf objects
# Transform the CRS of the shapefile to match stack
hru_sf <- st_transform(hru_sf, crs = terra::crs(openet_stack))

invalid_geometries <- hru_sf[!st_is_valid(hru_sf),]
if (nrow(invalid_geometries) > 0) {
  hru_sf <- st_make_valid(hru_sf)
}


data_list <- list()


for (i in 1:nlyr(openet_stack)){
  # Extract layer
  layer <- openet_stack[[i]]
  # Initialize
  hru_averages <- numeric(nrow(hru_sf))
  
  for(j in seq_len(nrow(hru_sf))){
    #this 2 is the second layer, the model_hru_
    hru <- hru_sf[j,2]
    # Mask each raster by each HRU
    layer_masked <- terra::crop(x = layer,
                                 y = hru,
                                 mask = TRUE,
                                 touches = TRUE) # no effect
    
    # Calculate the areal mean, excluding NAs from the crop
    hru_averages[j] <- global(layer_masked, fun = "mean", na.rm = TRUE)[[1]]
    
  }
  # Combine
  data_list[[i]] <- hru_averages
}

# This is our df created in R and terra
data_df <- as.data.frame(do.call(rbind, data_list))
#colnames(data_df) <- c("hru_12","hru_13")
colnames(data_df) <- c(paste0("hru_", seq_len(ncol(data_df))))









# This is our df created in gee
openet_gee_df <- read.csv("/Users/lnorth/Desktop/RA/RECIEVED/GEE/OpenET/OpenET_EastRiv_mad_20130101_20231201.csv")

openet_gee_df_wide <- openet_gee_df %>%
  pivot_wider(
    names_from = model_hru_,
    values_from = actET_monthly_mm
  )

# There seems to be difference ranging from 0.0003 mm to 3 mm!!!
# An overal bias of -0.16%
# HRU 4 is the most problematic, it is the smallest one

data_df - openet_gee_df_wide[1:11, 2:11]
max(data_df - openet_gee_df_wide[1:11, 2:11])
min(data_df - openet_gee_df_wide[1:11, 2:11])

hydroGOF::pbias(sim = data_df,
                obs = openet_gee_df_wide[1:11, 2:11])
plot(hru_sf[2])
