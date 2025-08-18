
# This script is intended to read in raw data and perform neccessary:
#   - screening
#   - masking
#   - transformations/retrsructuring
#   - unit conversions

# EastRiv SMAP L4

source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_process/pws.pivot.gee.R")

# Read in the files ---------------------------------------------------------

# Read in raw byHRU mean 
# This data is the openet ensemble mean
# Band "et_ensemble_mad"
data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/EastRiv/OpenET/OpenET_EastRiv_mad_20130101_20231201.csv"
openet_byHRU <- pws.pivot.gee(data_path = data_path,
                            desired_var = "actET_monthly_mm")


# Convert mm to in ----------------------------------------------

openet_byHRU <- openet_byHRU %>%
  mutate(across(starts_with("hru_"), ~ .x/25.4))



# Export the cleaned dataframe ----------------------------------------------

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/EastRiv/openet_data_df.csv"

write.csv(openet_byHRU, save_path, row.names = FALSE)


