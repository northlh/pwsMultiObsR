source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_process/pws.pivot.gee.R")

# Read in the files ---------------------------------------------------------

# Read in raw byHRU mean 
# This data is the openet ensemble mean
# Band "et_ensemble_mad"
data_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/DoloresRiv/OpenET_DoloresRiv_mad_20130101_20231201.csv"

gee_df <- read.csv(data_path)

# Since this brings in two different layers, split them up (131 months*nhru)
gee_df_1 <- gee_df[c(1:1965), ]
gee_df_2 <- gee_df[c(1966:3930), ]


# Pivot them
gee_df_wide_1 <- gee_df_1 %>%
  pivot_wider(
    names_from = model_hru_,
    values_from = !!sym(desired_var)
  )


gee_df_wide_2 <- gee_df_2 %>%
  pivot_wider(
    names_from = model_hru_,
    values_from = !!sym(desired_var)
  )


# Take the 12 and 13 columns from layer 2 and put into layer 1
gee_df_wide_1$`12` <- gee_df_wide_2$`12`
gee_df_wide_1$`13` <- gee_df_wide_2$`13`

# Check to make sure it is complete now
for(i in 2:13) {print(sum(is.na(gee_df_wide_1[,i])))}


# Rename columns with numeric names by appending "hru_"
gee_df_wide_1 <- gee_df_wide_1 %>%
  rename_with(
    .fn = ~ paste0("hru_", .),  # Append "hru_" to each numeric column name
    .cols = matches("^[0-9]+$")  # Select columns with purely numeric names
  )

# Add other columns for consistency with simulated dataframes
# Add water year (dataRetrieval function)
gee_df_wide_1 <- addWaterYear(gee_df_wide_1)

# Add water day (custom function)
gee_df_wide_1 <- addWaterDay(df = gee_df_wide_1,
                           date_colname = Date,
                           wy_colname = waterYear)


# openet_byHRU <- pws.pivot.gee(data_path = data_path,
#                             desired_var = "actET_monthly_mm")


# Convert mm to in ----------------------------------------------




gee_df_wide_1 <- gee_df_wide_1 %>%
  mutate(across(starts_with("hru_"), ~ .x/25.4))



# Export the cleaned dataframe ----------------------------------------------

save_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_obs/DoloresRiv/openet_data_df.csv"

write.csv(gee_df_wide_1, save_path, row.names = FALSE)


