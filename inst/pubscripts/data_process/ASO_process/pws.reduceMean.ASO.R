
# Similar to pws.reduceMean and pws.reduceMean.MOD10A1
# Had additional feature of automatically writing file


pws.reduceMean.ASO <- function(hru_path,
                               obs_folder,
                               output_folder, 
                               overwrite = FALSE){
  
  # CHECKS ------------------------------------------------------------------
  
  # Check
  if (!dir_exists(obs_folder)) {
    stop(paste("Observation directory:", obs_folder, "does not exist"))
  }
  
  # Set output file name
  ASO_data_df_filepath <- file.path(output_relpath, "ASO_data_df.csv")
  
  # Flag to track whether the file already existed
  file_existed <- file.exists(ASO_data_df_filepath)
  
  # If the file does not exist or if it exists and overwrite = TRUE, proceed
  if (file.exists(ASO_data_df_filepath)) {
    if (!overwrite) {
      # Read the existing file before stopping
      ASO_data_df <- read.csv(ASO_data_df_filepath)
      ASO_data_df$Date <- as.Date(ASO_data_df$Date) #ensure the Date class
      assign("ASO_data_df",ASO_data_df, envir = .GlobalEnv)
      message("Existing file found. File contents have been read in.")
      
      # Throw a warning instead of stopping, and exit the function
      warning(paste("File", ASO_data_df_filepath,
                    "already exists. If you would like to overwrite it, re-run with overwrite = TRUE."))
      #return(invisible(NULL))  # Exit the function gracefully
      return(ASO_data_df)
    }
  }
  
  # THIS IS DONE TO ENSURE FLEXIBILITY AT A GLOBAL SCALE
  # Define the target CRS (WGS 84)
  target_crs_sf <- st_crs(4326)  # For sf objects
  target_crs_terra <- "EPSG:4326"  # For terra objects, specified as a string
  
  
  
  
  
  # Read in HRU shapefile ---------------------------------------------------

  # Read the shapefile using sf (ensure it's in the same CRS)
  hru_sf <- st_read(hru_filepath)
  
  # Transform the CRS of the shapefile to match station data if necessary
  hru_sf <- st_transform(hru_sf, crs = target_crs_sf)
  
  #hru_sf_area <- as.numeric(st_area(st_make_valid(hru_sf)))
  
  
  
  
  # Identify ASO files -------------------------------------------------------
  
  # List the .tif ASO files
  ASO_src <- list.files(file.path(obs_folder),
                        pattern = "\\.tif$",
                        full.names = FALSE)
  # Check that data is in the folder
  if (length(ASO_src) == 0){
    stop("No files in the project_name/obs/ASO folder")
  }
  
  # Extract dates from filenames and convert to "YYYY-MM-DD" format
  ASO_dates <- ASO_src %>%
    str_extract("\\d{4}[A-Za-z]{3}\\d{2}") %>%          # Extract YYYYmnthDD format
    lubridate::parse_date_time(orders = "Yb%d") %>%     # Parse as date with lubridate
    format("%Y-%m-%d")                                  # Format to "YYYY-MM-DD"
  # \\d{4} matches the 4-digit year (e.g., 2023).
  # [A-Za-z]{3} matches the three-letter month abbreviation (e.g., Apr, May).
  # \\d{2} matches the 2-digit day (e.g., 01, 23).
  

  # # Initialize an empty SpatRaster (not a list)
  # ASO_stack <- NULL  
  # 
  # for (i in seq_along(ASO_src)) {
  #   
  #   # Read and transform each raster
  #   raster <- terra::rast(file.path(obs_folder, ASO_src[i]))
  #   raster <- terra::project(raster, target_crs_terra)
  #   
  #   # Combine rasters into a SpatRaster stack
  #   if (is.null(ASO_stack)) {
  #     ASO_stack <- raster  # First raster initializes the stack
  #   } else {
  #     ASO_stack <- c(ASO_stack, raster)  # Add new rasters to the stack
  #   }
  # }
  # 
  # # Check the number of layers in the stack
  # print(nlyr(ASO_stack))  # Should return the correct number of layers
  
  
  # Take areal average of ASO -----------------------------------------------
  # THIS IS ESSENTIALLY THE PWS.REDUCEMEAN FUNCTION
  # However, cannot covert ASO data into a SpatRaster stack due to
  # extents mismatch, so the rasters are called indidually
  
  # Initialize a list for easy data construction
  ASO_data_list <- list()
  
  # Extract the hru_ids (may be out of order depending on the shapefile)
  hru_ids <- hru_sf$model_hru_
  
  # Loop through each acquisition
  for(i in seq_along(ASO_src)){
    
    # Read and transform the rasters
    raster <- terra::rast(file.path(obs_folder, ASO_src[i]))
    raster <- terra::project(raster, target_crs_terra)
    
    # Initialize a vector to store areal averages for each HRU
    hru_averages <- numeric(nrow(hru_sf))
    
    # Loop through each HRU
    for(j in seq_len(nrow(hru_sf))){
      #this 2 is the second layer, the model_hru_
      hru <- hru_sf[j,2]
      
      # Mask each raster by each HRU
      raster_masked <- terra::crop(x = raster,
                                   y = hru,
                                   mask = TRUE,
                                   touches = TRUE) # no effect
      
      # Calculate the areal mean, excluding NAs from the crop
      hru_averages[j] <- global(raster_masked, fun = "mean", na.rm = TRUE)[[1]]
      
    } # close for j
    
    # Store results as a named list for the date
    ASO_data_list[[ASO_dates[i]]] <- hru_averages
    
  } # close for i
  
  # Construct df ------------------------------------------------------------
  
  # This is our df created in R and terra
  ASO_data_df <- as.data.frame(do.call(rbind, ASO_data_list))
  colnames(data_df) <- c(paste0("hru_", hru_ids))
  
  # Sort columns based on numeric HRU ids (in case of out of order .shp)
  sorted_order <- order(hru_ids)
  ASO_data_df <- ASO_data_df[, sorted_order] # reorder columns
  # Reassign sorted column names
  colnames(ASO_data_df) <- paste0("hru_", hru_ids[sorted_order])
  
  # EXTREMELY IMPORTANT STEP
  # Convert from meters to inches
  ASO_data_df <- ASO_data_df*3.2808399*12
  
  # Add date column
  ASO_data_df$Date <- as.Date(names(ASO_data_list))
  # Remove rownames
  rownames(ASO_data_df) <- NULL
  # Relocate Date column and sort by Date
  ASO_data_df <- ASO_data_df %>%
    relocate(Date, .before = hru_1) %>%        # Move 'date' to the first position
    arrange(Date)
  # Add hydrologic dates
  ASO_data_df <- addWaterYear(ASO_data_df)
  ASO_data_df <- addWaterDay(df = ASO_data_df,
                             date_colname = Date,
                             wy_colname = waterYear)
  
  
  # Write file --------------------------------------------------------------
  
  assign("ASO_data_df",
         ASO_data_df, envir = .GlobalEnv)
  
  # Save the file (either a new file or overwrite the existing one)
  write.csv(ASO_data_df, ASO_data_df_filepath, row.names = FALSE)
  if (file_existed) {
    message("File is overwritten as 'overwrite' is set to TRUE.")
  } else {
    message("File created successfully.")
  }
  
  return(ASO_data_df)
} #close fn






testt <- pws.read.ASO(hru_filepath = "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_nhm_DoloresRiv/GIS/model_nhru.shp",
                     obs_folder = "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/ASO_raw/DoloresRiv",
                     output_path = "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/DoloresRiv",
                     overwrite = TRUE)



# OLD CODE, tried to combine rasters into a stacked raster, extents mismatch

# # Initialize an empty SpatRaster (not a list)
# ASO_stack <- NULL  
# 
# for (i in seq_along(ASO_src)) {
#   
#   # Read and transform each raster
#   raster <- terra::rast(file.path(obs_folder, ASO_src[i]))
#   raster <- terra::project(raster, target_crs_terra)
#   
#   # Combine rasters into a SpatRaster stack
#   if (is.null(ASO_stack)) {
#     ASO_stack <- raster  # First raster initializes the stack
#   } else {
#     ASO_stack <- c(ASO_stack, raster)  # Add new rasters to the stack
#   }
# }
# 
# # Check the number of layers in the stack
# print(nlyr(ASO_stack))  # Should return the correct number of layers

