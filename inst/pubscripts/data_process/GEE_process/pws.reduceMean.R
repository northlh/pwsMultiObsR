# Private function to take a raster, shapefile, and compute the mean for each HRU

# rast_stack: a layered terra SparRaster
# hru_sf: the model_nhru.shp processed to the same coordinate system


pws.reduceMean <- function(rast_stack,
                        hru_sf){
  
  # Loop through each layer and extract data
  data_list <- list()
  
  # Extract the hru_ids (may be out of order depending on the shapefile)
  hru_ids <- hru_sf$model_hru_
  
  
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
    cat("\rProcessing layer:", i, "/", nlyr(rast_stack))
  }
  
  # This is our df created in R and terra
  data_df <- as.data.frame(do.call(rbind, data_list))
  colnames(data_df) <- c(paste0("hru_", hru_ids))
  
  # Sort columns based on numeric HRU ids
  sorted_order <- order(hru_ids)
  data_df <- data_df[, sorted_order] # reorder columns
  # Reassign sorted column names
  colnames(data_df) <- paste0("hru_", hru_ids[sorted_order])
  
  return(data_df)
}
