
pws.delta.cbh <- function(delta_prcp_pct = 0, # daily p*(1+(delta/100))
                          delta_t = 0, # daily t + delta_t
                          default_input_dir) {
  
  # Since this function directly overwrites files, we verify with the user
  response <- readline(prompt = paste0(
    "pws.delta.cbh will overwrite the .cbh files in: ",
    default_input_dir, 
    "\n- Are you sure you want to run (Y/N): "
  ))
  
  # Handle the response
  if (toupper(response) == "Y") {
    cat("Executing the function...\n")
    
    # Check for changes
    if (delta_prcp_pct == 0 && delta_t == 0) {
      message("Both precip. and temp. deltas == 0. Aborting function.")
      return(invisible(NULL)) # gently abort instead of error
    }
    
    # Change precipitation file
    if (delta_prcp_pct != 0) {
      # Read in files
      prcp <- pws.read.cbh(file.path(default_input_dir, "prcp.cbh"))
      
      # Apply delta_prcp_pct
      prcp <- prcp %>%
        mutate(across(starts_with("hru_"), ~ .x * (1 + (delta_prcp_pct / 100))))
      
      # Overwrite the prcp file
      pws.write.cbh(data = prcp,
                    filepath = file.path(default_input_dir, "prcp.cbh"),
                    header_type = "prcp")
    } # change prcp
    
    # Change temperature 
    if (delta_t != 0) {
      # Read in files
      tmin <- pws.read.cbh(file.path(default_input_dir, "tmin.cbh"))
      tmax <- pws.read.cbh(file.path(default_input_dir, "tmax.cbh"))
      
      # Apply delta_t
      tmin <- tmin %>%
        mutate(across(starts_with("hru_"), ~ .x + delta_t))
      tmax <- tmax %>%
        mutate(across(starts_with("hru_"), ~ .x + delta_t))
      
      # Overwrite the tmin and tmax files
      pws.write.cbh(data = tmin,
                    filepath = file.path(default_input_dir, "tmin.cbh"),
                    header_type = "tmin")
      pws.write.cbh(data = tmax,
                    filepath = file.path(default_input_dir, "tmax.cbh"),
                    header_type = "tmax")
    } # delta t
    
    print("Function executed!")
    
  } else if (toupper(response) == "N") {
    cat("pws.delta.cbh not executed.\n")
  } else {
    cat("Invalid input. Please enter Y or N.\n")
    pws.delta.cbh(delta_prcp_pct,
                  delta_t,
                  default_input_dir)  # Retry prompt by calling the function again
  } 
} # close function

  
  