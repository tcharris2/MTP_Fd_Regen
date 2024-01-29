

diagnosticColsFunction <- function (df) {
  
  # Create list of names to modify
  list_names <- names(df %>% select(starts_with("h_group")))
  resid_names <- c() # place to store modified names
  fitted_names <- c() # "
  
  # Create and store residual names
  for (i in 1:length(list_names)) {
    resid_name <- paste("resid", list_names[[i]], sep = "_")
    resid_names[i] <- resid_name
  }
  
  # Create and store fitted names
  for (i in 1:length(list_names)) {
    fitted_name <- paste("fitted", list_names[[i]], sep = "_")
    fitted_names[i] <- fitted_name
  }
  
  # Add the names to the dataframe as new columns
  df[resid_names] <- list(list(NA)) # Double listed so the class is "list" 
  df[fitted_names] <- list(list(NA)) # Needed to store data in the next step
  
  # Function output
  return(df)
  
}




extractMetrics <- function(df) { 
  
  # Create a list of model names
  model_list <- names(df %>% select(starts_with("h_group")))

  # Nested for-loop 1 - residuals
  for (model in model_list) { # for each model name 
    
    for (i in 1:nrow(df)) { # operate on the column
      
      # Extract residuals
      residuals <- resid(df[[model]][[i]])
      
      # using paste to specify a new but related column name for storage
      df[[paste("resid", model, sep = "_")]][[i]] <- residuals
      
    }
    
  }
  
  # Nested for-loop 2 - fitted 
  for (model in model_list) { # for each model name
    
    for (i in 1:nrow(df)) { # operate on the column
      
      # Extract fitted values
      fits <- fitted(df[[model]][[i]])
      
      # using paste to specify a new but related column name for storage
      df[[paste("fitted", model, sep = "_")]][[i]] <- fits
      
    }
    
  }
  
  # Function output
  return(df)
  
}



model_ResidFits <- function (df) {
  
  # Create place to store residuals 
  for ( i in 1:nrow(df)) {
    
    df[["data"]][[i]]$resid <- NA
    
  }
  
  # Create place to store fitted
  for ( i in 1:nrow(df)) {
    
    df[["data"]][[i]]$fitted <- NA
    
  }
  
  # Extract residuals
  for (i in 1:nrow(df)) {
    
    resid <- resid(df[[3]][[i]])
    
    df[["data"]][[i]]$resid <- resid
    
  }
  
  # Extract model fits 
  for (i in 1:nrow(df)) {
    
    fits <- fitted(df[[3]][[i]])
    
    df[["data"]][[i]]$fitted <- fits
    
  }
  
  return(df)
  
}
