
# This script is intended to read in raw data and perform neccessary:
#   - screening
#   - masking
#   - transformations/retrsructuring
#   - unit conversions

# EastRiv SMAP L4

source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_process/pws.pivot.gee.R")

# Read in the files ---------------------------------------------------------

# Read in raw byHRU mean 
data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/TaylorRiv/SPL4SMGP_TaylorRiv_surfwt_20150331_20240930.csv"
SMAP_byHRU <- pws.pivot.gee(data_path = data_path,
                            desired_var = "sm_surface_wetness")


# Lets ensure the dates are not characters for the following screening
SMAP_byHRU <- SMAP_byHRU %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))



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


daily_mean <- SMAP_byHRU %>%
  mutate(Day = floor_date(Date, "day")) %>%  # Create Day column for grouping
  group_by(Day, waterYear, waterDay) %>%  # Group by Day, waterYear, and waterDay
  summarize(across(starts_with("hru_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%  # Compute daily mean for hru columns
  rename(Date = Day)  # Rename Day back to Date in the final output



# Export the cleaned dataframe ----------------------------------------------

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/TaylorRiv/SMAPsfwt_data_df.csv"

write.csv(daily_mean, save_path, row.names = FALSE)


