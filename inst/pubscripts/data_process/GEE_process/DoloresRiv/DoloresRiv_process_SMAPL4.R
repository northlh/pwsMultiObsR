
# This script is intended to read in raw data and perform neccessary:
#   - screening
#   - masking
#   - transformations/retrsructuring
#   - unit conversions

# EastRiv SMAP L4

source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_process/pws.pivot.gee.R")

# Read in the files ---------------------------------------------------------

# Read in raw byHRU mean 
data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/SPL4SMGP_DoloresRiv_surfwt_20150331_20240930.csv"
SMAP_byHRU <- pws.pivot.gee(data_path = data_path,
                            desired_var = "sm_surface_wetness")

# Read in raw by centroid values
data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/SPL4SMGP_DoloresRiv_surfwt_20150331_20240930_centroid.csv"
SMAP_centroid <- pws.pivot.gee(data_path = data_path,
                            desired_var = "sm_surface_wetness") #

# Lets ensure the dates are not characters for the following screening
SMAP_byHRU <- SMAP_byHRU %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))

SMAP_centroid <- SMAP_centroid %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))





# General screening steps ---------------------------------------------------
# Missing values are antricpated for the small HRUs where
# ee.ReduceRegions is not able to compute a meaningful value



# Address missing values and length issue -----------------------------------
# Uh-oh these are slightly differnt lenghths
# A few random days in 2024 got left out in the centroid calcualtions
missing_in_byHRU <- SMAP_centroid %>%
  filter(!(Date %in% SMAP_byHRU$Date))

missing_in_centroid <- SMAP_byHRU %>%
  filter(!(Date %in% SMAP_centroid$Date))

# We can't make up data, so lets remove these timesteps from the longer df
SMAP_byHRU_trimmed <- SMAP_byHRU %>%
  filter(Date %in% SMAP_centroid$Date)

# Finally, we can replace the small HRU columns with their centroid values
# Replace empty columns with centroid values for the smalll HRUs

# SPECIFIC TO EACH BASIN (ONLY THE EAST AND DOLORES HAD THIS ISSUE)

SMAP_byHRU_trimmed$hru_6 <- SMAP_centroid$hru_6





# Aggregate the sub-daily data to daily -------------------------------------

# The posix datetimes have a timezone attached, we will use floor_date
# instead of as.Date to return a day
# as this causes the date to change after certain times

# # USED TO PARSE OUT WEIRD THINGS WITH as.Date
# # Calculate daily mean
# daily_mean <- SMAP_byHRU_trimmed %>%
#   mutate(Day = as.Date(Date)) %>% # Extract date
#   group_by(Day) %>% # Group by day
#   summarize(across(hru_1:hru_10, ~ mean(.x, na.rm = TRUE))) # Compute column-wise mean
# 
# daily_mean <- SMAP_byHRU_trimmed %>%
#   mutate(Day = floor_date(Date, "day"))  # Create Day column for grouping
# 
# daily_mean <- SMAP_byHRU_trimmed %>%
#   mutate(Day = as.Date(Date))  # Create Day column for grouping


daily_mean <- SMAP_byHRU_trimmed %>%
  mutate(Day = floor_date(Date, "day")) %>%  # Create Day column for grouping
  group_by(Day, waterYear, waterDay) %>%  # Group by Day, waterYear, and waterDay
  summarize(across(starts_with("hru_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%  # Compute daily mean for hru columns
  rename(Date = Day)  # Rename Day back to Date in the final output


# UNIQUE TO DOLORES
# SHAPEFILE WAS ORIGINALLY OUT OF ORDER
# Reorder columns
daily_mean <- daily_mean %>%
  relocate(c(hru_1, hru_2, hru_3), .before = hru_4)


# Export the cleaned dataframe ----------------------------------------------

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/DoloresRiv/SMAPsfwt_data_df.csv"

write.csv(daily_mean, save_path, row.names = FALSE)


