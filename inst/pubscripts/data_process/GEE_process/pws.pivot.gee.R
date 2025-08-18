

# This function is designed to read in the long format GEE data that is
# produced using the ReduceRegions operations
source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/output_process/addWaterDay.R")

pws.pivot.gee <- function(data_path, # a long format .csv file
                          desired_var
                          #save_dir,
                          #save_name
                          ){
  
  # Read in GEE .csv
  gee_df <- read.csv(data_path)
  
  # Dynamically subset using column names
  gee_df <- gee_df[, c("Date", "model_hru_", desired_var)]
  
  # # FOR ET, CONVERT MM TO INCHES
  # gee_df$actET_monthly_in <- gee_df$actET_monthly_mm/25.4
  # gee_df$actET_monthly_mm <- NULL
  # desired_var <- "actET_monthly_in"
  
  
  
  # Pivot wider
  gee_df_wide <- gee_df %>%
    pivot_wider(
      names_from = model_hru_,
      values_from = !!sym(desired_var)
    )
  
  # Rename columns with numeric names by appending "hru_"
  gee_df_wide <- gee_df_wide %>%
    rename_with(
      .fn = ~ paste0("hru_", .),  # Append "hru_" to each numeric column name
      .cols = matches("^[0-9]+$")  # Select columns with purely numeric names
    )
  
  # Add other columns for consistency with simulated dataframes
  # Add water year (dataRetrieval function)
  gee_df_wide <- addWaterYear(gee_df_wide)
  
  # Add water day (custom function)
  gee_df_wide <- addWaterDay(df = gee_df_wide,
                             date_colname = Date,
                             wy_colname = waterYear)
  
  # Write file --------------------------------------------------------------
   
  # assign("gee_df_wide",
  #        gee_df_wide, envir = .GlobalEnv)
  # # Set output file name
  # gee_df_wide_filepath <- file.path(save_dir, paste0(save_name,".csv"))
  # # Save the file (either a new file or overwrite the existing one)
  # write.csv(gee_df_wide, gee_df_wide_filepath, row.names = FALSE)
  # 
  # # save to dir
  # assign(save_name,
  #        gee_df_wide, envir = .GlobalEnv)
  
  return(gee_df_wide)

}