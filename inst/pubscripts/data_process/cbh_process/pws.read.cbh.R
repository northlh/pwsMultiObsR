



pws.read.cbh <- function(filepath) {
  # Step 1: Read the entire file
  lines <- readLines(filepath)
  
  # Step 2: Identify and skip the header
  data_start <- grep("^[0-9]{4}", lines)[1]  # Finds the first line of actual data
  data_lines <- lines[data_start:length(lines)]  # Extract data lines
  
  # Step 3: Read data into a data frame
  data <- read.table(text = data_lines, header = FALSE)
  
  # Step 4: Combine the first 3 columns into a date column
  library(dplyr)
  data <- data %>%
    mutate(Date = as.Date(paste(V1, V2, V3, sep = "-"))) %>%
    select(Date, everything(), -V1, -V2, -V3, -V4, -V5, -V6)
  
  # Step 5: Rename columns
  colnames(data) <- c("Date", paste0("hru_", 1:(ncol(data) - 1)))
  
  # Return the processed data frame
  return(data)
}




# # Read in NHM daymet 1979-2022
# # Example usage
# cbh_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/EastRiv_example_defaults/prcp.cbh"
# cbh_data <- pws.read.cbh(cbh_path)
# 
# # View the resulting data frame
# print(head(cbh_data))
# 
# 
# 
# # Read in GEE daymet 1980-2023
# source("/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_process/pws.pivot.gee.R")
# 
# daymet_path <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/source/data_process/GEE_raw/EastRiv/Daymet/Daymet_EastRiv_all_19800101_20231231.csv"
# 
# daymet_data <- pws.pivot.gee(data_path = daymet_path,
#                              desired_var = "prcp")
# daymet_data <- daymet_data[,c(-2,-3)]
# print(head(daymet_data))
# 
# daymet_data <- daymet_data %>%
#   mutate(across(starts_with("hru_"), ~ .x/25.4))
# 
# 
# 
# # Find the shared dates
# cbh_data$Date <- as.Date(cbh_data$Date) #sometimes charc if read through .csv
# daymet_data$Date <- as.Date(daymet_data$Date)
# shared_dates <- as.Date(intersect(cbh_data$Date, daymet_data$Date))
# 
# # Subset both data frames by shared dates (unique cols for point obs)
# cbh_data <- cbh_data %>% filter(Date %in% shared_dates)
# daymet_data <- daymet_data %>% filter(Date %in% shared_dates)
# 
# 
# 
# 
# library(ggplot2)
# ggplot() +
#   geom_line(aes(x = cbh_data$Date, y = cbh_data$hru_1), color = "blue") +
#   geom_line(aes(x = daymet_data$Date, y = daymet_data$hru_1), color = "red") +
#   labs(x = "Date", y = "Value", title = "Comparison of Data Frames")
# 
# 
# 
# 
# 
# total_err <- sum(abs(cbh_data$hru_1 - daymet_data$hru_1))
# total_difference <- sum(cbh_data$hru_1) - sum(daymet_data$hru_1)
# 
# print(total_err)
# print(total_difference)
# 
# 
# 
# 
# cor(cbh_data$hru_1, daymet_data$hru_1)
# 
# 
# 
# 
# wilcox_result <- wilcox.test(cbh_data$hru_1, daymet_data$hru_1, paired = TRUE)
# print(wilcox_result)
