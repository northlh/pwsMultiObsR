

pws.write.cbh <- function(data, filepath, header_type = "prcp") {
  # Step 1: Ensure the data has the correct structure
  if (!"Date" %in% colnames(data)) {
    stop("The data.frame must contain a 'Date' column.")
  }
  
  # Step 2: Split the Date column into Year, Month, and Day
  # data <- data %>%
  #   mutate(
  #     Year = as.integer(format(Date, "%Y")),          # Extract Year as an integer
  #     Month = as.POSIXlt(Date)$mon + 1,               # Extract Month as an integer
  #     Day = as.POSIXlt(Date)$mday                     # Extract Day as an integer
  #   ) %>%
  #   select(Year, Month, Day, everything(), -Date)
  
  data <- data %>%
    mutate(Year = format(Date, "%Y"),
           Month = as.integer(format(Date, "%m")),  # Drop leading zeros
           Day = as.integer(format(Date, "%d"))) %>%  # Drop leading zeros
    select(Year, Month, Day, everything(), -Date)
  
  
  # data <- data %>%
  #   mutate(Year = format(Date, "%Y"),
  #          Month = format(Date, "%m"),
  #          Day = format(Date, "%d")) %>%
  #   select(Year, Month, Day, everything(), -Date)
  
  
  
  # Step 3: Add three zeros for the time columns
  data <- data %>%
    mutate(Zero1 = 0, Zero2 = 0, Zero3 = 0) %>%
    select(Year, Month, Day, Zero1, Zero2, Zero3, everything())
  
  #print(head(data))
  
  # Step 4: Create the header
  # Determine the number of HRUs
  num_hru <- ncol(data) - 6  # Exclude the first 6 columns (Year, Month, Day, Zero1, Zero2, Zero3)
  
  # Get current working directory and timestamp
  wd <- getwd()
  timestamp <- Sys.time()
  
  # Construct the header lines
  header <- c(
    paste("Written by:", wd, "at", timestamp),
    paste(header_type, num_hru),
    paste("########################################")
  )
  
  # Step 5: Prepare the data lines
  # Combine all columns into space-separated rows
  data_lines <- apply(data, 1, function(row) {
    paste(row, collapse = " ")
  })
  
  # Combine the header and data
  output_lines <- c(header, data_lines)
  
  # Step 6: Write to the .cbh file
  writeLines(output_lines, filepath)
  message(paste("File written:", filepath))
}



# pws.write.cbh <- function(data, filepath, header_type = "prcp") {
#   # Step 1: Ensure the data has the correct structure
#   if (!"Date" %in% colnames(data)) {
#     stop("The data.frame must contain a 'Date' column.")
#   }
#   
#   # Step 2: Split the Date column into Year, Month, and Day
#   data <- data %>%
#     mutate(
#       Year = as.integer(format(Date, "%Y")),       # Year as integer
#       Month = as.integer(format(Date, "%m")),      # Month as integer (removes leading zeros)
#       Day = as.integer(format(Date, "%d"))         # Day as integer (removes leading zeros)
#     ) %>%
#     select(Year, Month, Day, everything(), -Date)
#   
#   # Step 3: Add three zeros for the time columns
#   data <- data %>%
#     mutate(Zero1 = 0, Zero2 = 0, Zero3 = 0) %>%
#     select(Year, Month, Day, Zero1, Zero2, Zero3, everything())
#   
#   # Step 4: Create the header
#   # Determine the number of HRUs (exclude first 6 columns)
#   num_hru <- ncol(data) - 6
#   
#   # Get current working directory and timestamp
#   wd <- getwd()
#   timestamp <- Sys.time()
#   
#   # Construct the header lines
#   header <- c(
#     paste("Written by Bandit"),
#     paste(header_type, num_hru),
#     paste("########################################"),
#     paste("# Working directory:", wd),
#     paste("# File created on:", timestamp),
#     paste("########################################")
#   )
#   
#   # Step 5: Prepare the data lines as formatted text
#   data_lines <- apply(data, 1, function(row) {
#     # Manually format each row
#     formatted_row <- paste(
#       row[1], row[2], row[3], row[4], row[5], row[6],   # Year, Month, Day, Zero1, Zero2, Zero3
#       paste(row[7:length(row)], collapse = " ")          # HRU values (after Zero1, Zero2, Zero3)
#     )
#     return(formatted_row)
#   })
#   
#   # Combine header and data
#   output_lines <- c(header, data_lines)
#   
#   # Step 6: Write to the .cbh file
#   writeLines(output_lines, filepath)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# pws.write.cbh <- function(data, filepath, header_type = "prcp") {
#   # Step 1: Ensure the data has the correct structure
#   if (!"Date" %in% colnames(data)) {
#     stop("The data.frame must contain a 'Date' column.")
#   }
#   
#   # Step 2: Split the Date column into Year, Month, and Day
#   data <- data %>%
#     mutate(Year = as.integer(format(Date, "%Y")),
#            Month = as.integer(format(Date, "%m")),  # Month as character to keep the leading zero
#            Day = as.integer(format(Date, "%d"))) %>%  # Day as character to keep the leading zero
#     select(Year, Month, Day, everything(), -Date)
#   
#   # Step 3: Add three zeros for the time columns
#   data <- data %>%
#     mutate(Zero1 = 0, Zero2 = 0, Zero3 = 0) %>%
#     select(Year, Month, Day, Zero1, Zero2, Zero3, everything())
#   
#   # Step 4: Create the header
#   num_hru <- ncol(data) - 6  # Exclude the first 6 columns (Year, Month, Day, Zero1, Zero2, Zero3)
#   
#   # Get current working directory and timestamp
#   wd <- getwd()
#   timestamp <- Sys.time()
#   
#   # Construct the header lines
#   header <- c(
#     paste("Written by Bandit"),
#     paste(header_type, num_hru),
#     paste("########################################"),
#     paste("# Working directory:", wd),
#     paste("# File created on:", timestamp),
#     paste("########################################")
#   )
#   
#   # Step 5: Prepare the data lines with consistent spacing
#   data_lines <- apply(data, 1, function(row) {
#     # Format each column with appropriate width and alignment
#     formatted_row <- paste(
#       sprintf("%4s", row[1]),  # Year (4-digit integer)
#       sprintf("%2s", row[2]),  # Month (2-digit string, keeps the leading zero if needed)
#       sprintf("%2s", row[3]),  # Day (2-digit string, keeps the leading zero if needed)
#       sprintf("%3d", row[4]),  # Zero1
#       sprintf("%3d", row[5]),  # Zero2
#       sprintf("%3d", row[6]),  # Zero3
#       paste(sapply(row[7:length(row)], function(x) sprintf("%6.2f", x)), collapse = " ")  # HRU values
#     )
#     return(formatted_row)
#   })
#   
#   # Combine header and data lines
#   output_lines <- c(header, data_lines)
#   
#   # Step 6: Write to the .cbh file
#   writeLines(output_lines, filepath)
# }

