# Content: lrtest Function -----------------------------------------------------

# Author: Thomson Harris
# Date: 5th Oct, 2023

# Function to test models ------------------------------------------------------

lrtestFunction <- function(test_models) {
  lrtest(test_models[[1]], test_models[[2]]) %>%  # Test models is temporarily created list used for iteration
    nest()                                        # Nesting to be able to store the output in a tibble
}


# Generic test function that can operate on a specified input -----------------

modelsTest <- function (df, model_x, model_y) { # Function head
                      # Input legend:
                      # df = dataframe
                      # model_x/y = df$model_x/y
  
  # Creating a generic place for output to be stored.
  df$lrtest <- NA  
  
  for (i in 1:nrow(df)) { # Head of the for-loop
    
    test_models <- c(model_x[[i]], model_y[[i]])
    
    # Testing and storing the models in a generic column
    df$lrtest[i] <- lrtestFunction(test_models)
    
  } # End of for-loop
  
  # Function Output (store in a new dataframe column)
  return(df$lrtest)   
}


# Extracting p-values from models ----------------------------------------------

extractPVals <- function(df) {
  
  # Create a list of lr_test names
  lr_test_list <- names(df %>% select(starts_with("lr_test")))
  
  # Nested for-loop 1 - residuals
  for (lr_test in lr_test_list) { # for each model name 
    
    for (i in 1:nrow(df)) { # operate on the column
      
      p_values <- c(df[[lr_test]][[i]][["Pr(>Chisq)"]][[2]]) 
      
      df[[paste("p_val", lr_test, sep = "_")]][i] <- p_values
      
    }
    
  }
  
  # Rename columns to make them tidier
  names(df) = gsub(pattern = "_lr_test", replacement = "", x = names(df))
  
  # Function Output
  return(df)
  
}
  
# Removing Non-Significant P-Values --------------------------------------------

removeNonSigPVals <- function (df) {
  
  # Create a list of p_val names
  p_val_list <- names(df %>% select(starts_with("p_val")))
  
  # Outer-for-loop to operate on columns
  for (p_val in p_val_list) {
    
    # Inner-for-loop to operate on rows
    for ( i in 1:nrow(df)) {
    
      # if statement to replace non-sig values with NA
       if (df[[p_val]][[i]] >= 0.05)
      
         df[[p_val]][[i]] <- NA
    }
    
  }
  # Function Output
  return(df)
}



