


# Set trial -----------------------------------------------------------

# Set trial
source("source/run_process/pws.set.trial.R")
pws.set.trial(project_name = "day_EastRiv",
              trial_number = 1) #if things went okay this should be 3


# Read in sim -----------------------------------------------------------
# Ratio from 0 to 1 (ie, saturation)

run = 1
nruns_dev = 1
#list_GOF_SMAPsfwt = NULL

# hru_out_names <- "soil_lower_ratio"
# hru_out_names <- "soil_moist" 
hru_out_names <- "soil_rechr"
default = TRUE

# Loops through every run folder in the trial specified by the output dir
for (run in 1:nruns_dev) {
  model <- paste0("run_",run)
  for (j in seq_along(hru_out_names)) {
    # Add suffix based on model index
    output_name <- paste0(hru_out_names[j], "_", run)

    # Opens the nhm folder in the default_output_dir
    if (default == TRUE){
      file_path <- file.path(dynamic_output_dir, "nhm",
                             paste0(hru_out_names[j],".nc"))
    } else {
      # Opens the run_xx folder in the trial dynamic output dir
      file_path <- file.path(dynamic_output_dir, model,
                             paste0(hru_out_names[j], ".nc")) #adds .nc suffix
    }

    nc <- nc_open(file_path) # Open the netCDF file
    variable <- ncvar_get(nc, hru_out_names[j]) # Get the variable
    variable <- t(variable) # Transpose
    time_dim <- ncvar_get(nc, "time") # Get the time variable
    nc_close(nc) # Close the netCDF file

    # Convert time variable to Date format, PRMS uses 1970-01-01 as reference
    time_date <- as.Date("1970-01-01") + time_dim

    # Add time vector to hru:time variable matrix
    hru_output <- data.frame(Date = time_date, variable)

    # Generate new column names
    hru_names <- paste0("hru_", 1:(ncol(hru_output)-1))

    # Rename the last 10 columns
    hru_output <- hru_output %>%
      rename_at(vars((ncol(hru_output) -
                        (ncol(variable)-1)):ncol(hru_output)),
                ~ hru_names)

    # Add water year (dataRetrieval function)
    hru_output <- addWaterYear(hru_output)

    # Add water day (custom function)
    hru_output <- addWaterDay(df = hru_output,
                              date_colname = Date,
                              wy_colname = waterYear)

    assign(output_name, hru_output, envir = .GlobalEnv)
    }


} # close loop to read in sims




# Read in SMS --------------------
# VWC? Ranges are all over the place, many outliers
NRCS_data_list <- fromJSON(file.path(obs_dir,"SNOTEL/NRCS_data_list.json"))
NRCS_stations <- fromJSON(file.path(obs_dir,"SNOTEL/NRCS_stations.json"))

SMS2_obs_380_raw <- NRCS_data_list$CO$`380`[,-c(4, 6, 7)] #exclude SWE column
summary(SMS2_obs_380_raw)
SMS2_obs_380 <- SMS2_obs_380_raw[!is.na(SMS2_obs_380_raw$SMS2), ] #remove NAs
SMS2_obs_380 <- SMS2_obs_380[!(SMS2_obs_380$SMS2 >100), ] # remove errors
summary(SMS2_obs_380)
# sat = VWC/porosity (normalized by max, ranges 0 to 1)
# NORMALIZE SMS DATA
# As called out in Hay, 2023 "parameterization of..."
porosity_380 <- max(SMS2_obs_380$SMS2)
SMS2_obs_380$SMS2 <- SMS2_obs_380$SMS2 / porosity_380
summary(SMS2_obs_380)

#SMS2_obs_737_raw <- NRCS_data_list$CO$`737`[,-4] #exclude SWE column


# Read in SMAP -------------------------------
# Supposed to be saturation but never dries out to 0
SMAP_obs_raw <- read.csv(file.path(obs_dir,"SMAPL4/SMAPsfwt_data_df.csv"))

# Prelim diagnostics ------------------------------------------------
summary(soil_lower_ratio_1)
summary(soil_moist_1)
summary(soil_rechr_1)
summary(SMS2_obs_380)
summary(SMAP_obs_raw)

hist(soil_lower_ratio_1$hru_8)
#hist(soil_moist_1$hru_8)
hist(soil_moist_1$hru_8/max(soil_moist_1$hru_8))
hist(soil_rechr_1$hru_8/max(soil_rechr_1$hru_8))

hist(SMS2_obs_380$SMS2)
hist(SMAP_obs_raw$hru_8)

# NORMALIZE SMAP DATA
# As called out in Hay, 2023 "parameterization of..."
SMAP_obs_raw <- SMAP_obs_raw %>%
  mutate(across(starts_with("hru_"), ~ (. - min(.)) / (max(.) - min(.))))

# SMAP_obs_raw$hru_8 <- (SMAP_obs_raw$hru_8 - min(SMAP_obs_raw$hru_8))/
#   (max(SMAP_obs_raw$hru_8) - min(SMAP_obs_raw$hru_8))
hist(SMAP_obs_raw$hru_8)


# NORMALIZE SIMULATED DATA
# As called out in Hay, 2023 "parameterization of..."
soil_lower_ratio_1$hru_8 <- (soil_lower_ratio_1$hru_8 - min(soil_lower_ratio_1$hru_8))/
  (max(soil_lower_ratio_1$hru_8) - min(soil_lower_ratio_1$hru_8))

soil_moist_1$hru_8 <- (soil_moist_1$hru_8 - min(soil_moist_1$hru_8))/
  (max(soil_moist_1$hru_8) - min(soil_moist_1$hru_8))

soil_rechr_1$hru_8 <- (soil_rechr_1$hru_8 - min(soil_rechr_1$hru_8))/
  (max(soil_rechr_1$hru_8) - min(soil_rechr_1$hru_8))


SMS2_obs_380$Date <- as.Date(SMS2_obs_380$Date)
SMAP_obs_raw$Date <- as.Date(SMAP_obs_raw$Date)

summary(SMS2_obs_380)
summary(SMAP_obs_raw)
summary(soil_rechr_1)

# PRELIM 

ggplot()+
    # geom_line(data = soil_lower_ratio_1 %>% filter(waterYear == 2020:2022),
    #           aes(x = Date, y = hru_8), color = "black")+
    # geom_line(data = soil_moist_1 %>% filter(waterYear == 2020:2022),
    #         aes(x = Date, y = hru_8), color = "darkgray")+
    geom_line(data = soil_rechr_1 %>% filter(waterYear == 2022),
            aes(x = Date, y = hru_8), color = "black")+
    geom_line(data = SMS2_obs_380 %>% filter(waterYear == 2022),
              aes(x = Date, y = SMS2), color = "skyblue")+
    geom_line(data = SMAP_obs_raw %>% filter(waterYear == 2022),
            aes(x = Date, y = hru_8), color = "orange")+
    labs(#title = "test",
         x = "Time", y = "Normalized soil moisture") +
    theme_linedraw(base_size = 9)


# MORE POLISHED PLOT

ggplot() +
  geom_line(data = soil_rechr_1 %>% filter(waterYear == 2022),
            aes(x = Date, y = hru_8, color = "soil_rechr", linetype = "soil_rechr"),
            linewidth = 0.8) +
  
  geom_line(data = SMS2_obs_380 %>% filter(waterYear == 2022),
            aes(x = Date, y = SMS2, color = "SMS2 obs", linetype = "SMS2 obs"),
            linewidth = 0.8, alpha = 0.7) +
  
  geom_line(data = SMAP_obs_raw %>% filter(waterYear == 2022),
            aes(x = Date, y = hru_8, color = "SMAP obs", linetype = "SMAP obs"),
            linewidth = 0.8, alpha = 0.7) +
  
  # scale_color_manual(name = "Source",
  #                    values = c("soil_rechr" = "black", "SMS2 obs" = "skyblue", "SMAP obs" = "orange")) +
  
  scale_color_manual(name = "Source",
                     values = c("soil_rechr" = "#000000", "SMS2 obs" = "#56B4E9", "SMAP obs" = "#E69F00")) +
  
  scale_linetype_manual(name = "Source",
                        values = c("soil_rechr" = "solid", "SMS2 obs" = "solid", "SMAP obs" = "solid")) +
  
  labs(x = "Time", y = "Normalized soil moisture") +
  theme_linedraw(base_size = 9)


ggsave(
  filename = file.path(here(),"source/manuscript_figures/FIGS/SM_diagnostic.png"),   # Save as PDF
  plot = last_plot(),                       # Replace with your plot object
  width = 6.5, height = 4.5, units = "in", dpi = 300
)


# ggplot()+
#   # geom_line(data = soil_lower_ratio_1 %>% filter(waterYear %in% 2020:2022),
#   #           aes(x = Date, y = hru_8), color = "black")+
#   geom_line(data = soil_moist_trans %>% filter(waterYear %in% 2020:2022),
#             aes(x = Date, y = hru_8), color = "darkgray")+
#   # geom_line(data = recharge_trans %>% filter(waterYear %in% 2019:2022),
#   #           aes(x = Date, y = hru_8), color = "lightgray")+
#   geom_line(data = SMS2_obs_380 %>% filter(waterYear %in% 2020:2022),
#             aes(x = Date, y = SMS2), color = "skyblue")+
#   geom_line(data = SMAP_obs_raw %>% filter(waterYear %in% 2020:2022),
#             aes(x = Date, y = hru_8), color = "orange")+
#   labs(title = "test", x = "WaterDay", y = "lower_ratio")
# 
# 
# ggplot()+
#   geom_line(data = soil_lower_ratio_1 %>% filter(waterYear == 2016),
#             aes(x = waterDay, y = hru_8), color = "black")+
#   geom_line(data = SMS2_obs_380 %>% filter(waterYear == 2016),
#             aes(x = waterDay, y = SMS2), color = "skyblue")+
#   geom_line(data = SMAP_obs %>% filter(waterYear == 2016),
#             aes(x = waterDay, y = hru_8), color = "orange")+
#   labs(title = "test", x = "WaterDay", y = "lower_ratio")
 

# Calc NSE for SMS
shared_dates <- as.Date(intersect(soil_lower_ratio_1$Date,
                                  SMS2_obs_380$Date))

soil_lower_ratio_1 <- soil_lower_ratio_1 %>% filter(Date %in% shared_dates)
soil_moist_1 <- soil_moist_1 %>% filter(Date %in% shared_dates)
soil_rechr_1 <- soil_rechr_1 %>% filter(Date %in% shared_dates)
SMS2_obs_380 <- SMS2_obs_380 %>% filter(Date %in% shared_dates)

test1 <- hydroGOF::NSE(sim = soil_lower_ratio_1$hru_8,
                      obs = SMS2_obs_380$SMS2)

test1 <- hydroGOF::NSE(sim = soil_moist_1$hru_8,
                      obs = SMS2_obs_380$SMS2)

test1 <- hydroGOF::NSE(sim = soil_rechr_1$hru_8,
                       obs = SMS2_obs_380$SMS2)


# Calc NSE for SMAP
# There is a considerable improvement after normalization (-6 to -1.5 ish)
shared_dates_SMAP <- as.Date(intersect(soil_lower_ratio_1$Date,
                                  SMAP_obs_raw$Date))

soil_lower_ratio_1SMAP <- soil_lower_ratio_1 %>% filter(Date %in% shared_dates_SMAP)
soil_moist_1SMAP <- soil_moist_1 %>% filter(Date %in% shared_dates_SMAP)
soil_rechr_1SMAP <- soil_rechr_1 %>% filter(Date %in% shared_dates_SMAP)
SMS2_obs_380SMAP <- SMS2_obs_380 %>% filter(Date %in% shared_dates_SMAP)
SMAP_obs_raw <- SMAP_obs_raw %>% filter(Date %in% shared_dates_SMAP)

test1 <- hydroGOF::NSE(sim = soil_lower_ratio_1SMAP$hru_8,
                      obs = SMAP_obs_raw$hru_8)

test1 <- hydroGOF::NSE(sim = soil_moist_1SMAP$hru_8,
                       obs = SMAP_obs_raw$hru_8)

test1 <- hydroGOF::NSE(sim = soil_rechr_1SMAP$hru_8,
                       obs = SMAP_obs_raw$hru_8)


