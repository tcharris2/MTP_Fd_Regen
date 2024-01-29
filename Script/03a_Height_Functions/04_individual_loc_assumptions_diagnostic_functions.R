# 1. Individual Location Diagnostic Columns -----------------------------------

indivDiagnosticColsFunction <- function (df) {
  
  # Outer for-loop allowing for operations across multiple rows 
  for(j in 1:nrow(df)) {
  
  # Create list of names to modify
    list_names <- names(df %>% select(starts_with("model")))
    resid_names <- c() # place to store modified names
    fitted_names <- c() # "
  
  # Inner for-loop 1: Create and store residual names
  for (i in 1:length(list_names)) {
    resid_name <- paste("resid", list_names[[i]], sep = "_")
    resid_names[i] <- resid_name
  }
  
  # Inner for-loop 2: Create and store fitted names
  for (i in 1:length(list_names)) {
    fitted_name <- paste("fitted", list_names[[i]], sep = "_")
    fitted_names[i] <- fitted_name
  }
  
  # Add the names to the dataframe as new columns
    df[["data"]][[j]][resid_names] <- NA # NAs to fill 
    df[["data"]][[j]][fitted_names] <- NA 
  

  } # end of outer for-loop
  
  # Function output
  return(df)
  
}


# 2. Individual Location Diagnostic Metric Extraction ------------------------

indivExtractMetrics <- function(df) { 
  
  # Create a list of model names
  model_list <- names(df %>% select(starts_with("model")))

  # Outer for-loop 1 - residuals
  for (model in model_list) { # for each model name 
    
    # Inner for-loop 1 - residuals
    for (i in 1:nrow(df)) { # operate on the column
      
      # Extract residuals
      residuals <- resid(df[[model]][[i]])
      
      # using paste to specify a new but related column name for storage
      df[["data"]][[i]][[paste("resid", model, sep = "_")]] <- residuals
      
    }
    
  }
  
  # Outer for-loop 2 - fitted 
  for (model in model_list) { # for each model name
    
    # Inner for-loop 2 - fitted 
    for (i in 1:nrow(df)) { # operate on the column
      
      # Extract fitted values
      fits <- fitted(df[[model]][[i]])
      
      # using paste to specify a new but related column name for storage
      df[["data"]][[i]][[paste("fitted", model, sep = "_")]] <- fits
      
    }
    
  }
  
  # Function output
  return(df)
  
}

# 3. Model Resid/Fits for Individual Models--------------

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


# 4. Extracting Metrics into a Graphing format -----------------------


graphingDfFunction <- function(df) { # function operates on an 
                                     # "renested" harvest models dataframe
  # List for new df's to go
  graphing_df <- list(NA)
  
  # for-loop to create new df's
  for(i in 1:nrow(df)) { # ex. df = renest_height_harvest_models
    
    # pulling data and models from the ith climatic variable
      new_df <- unnest(df[i, ])
    
    # ungrouping removes an error (not sure why)
      new_df <- ungroup(new_df)
    
    # creates new columns in new_df$data for the resids and fits
     new_df <- indivDiagnosticColsFunction(new_df)
    
    # fills the columns
      new_df <- indivExtractMetrics(new_df)
    
    # Keeping only needed information (removing models)
      new_df <- subset(new_df, select = c("climatic_var", "location", "data"))
    
    # Expanding dataframe so that it can be graphed
     new_df <- unnest(new_df)
    
    # storing expanded dataframe in a list
      graphing_df[[i]] <- new_df
    
  }
  
  # Function Output
  return(graphing_df)
  
}



# 5. Melting graphing_df -------------------------------------------------------

meltGraphingDF <- function(df) {
  
  # Create a new list object to store function output 
  melt_graphing_df <- list(NA)
  
  # For-loop to melt residuals/fitted values into a single column
  for(i in 1:length(df)) {
    
    # Pull the ith dataframe from the list
     singular_df <- df[[i]]
    
    # create two seperate dataframes to operate on
      resid_df <- subset(singular_df, select = -c(fitted_model_1, fitted_model_2, fitted_model_3))
      fitted_df <- subset(singular_df, select = -c(resid_model_1, resid_model_2, resid_model_3))
    
    # pull the names from the dataframe
      names <- names(singular_df)
    
    # Remove the residaul/fitted names. Needed to melt the appropriate columns
      names <- names[! names %in% c("resid_model_1", "resid_model_2", "resid_model_3", 
                                    "fitted_model_1", "fitted_model_2", "fitted_model_3")]
      
      
    # Melt 1 - combining resid_model_1/2/3 into a combined column 
      melt_resid_df <- melt(resid_df, id.var = names, variable.name = "resid_model_name")
    
    # Renaming the created "value" column so there wont be duplicates
      names(melt_resid_df)[names(melt_resid_df) == "value"] <- "residaul"
    
    
    # Melt 2 - combining fitted_model_1/2/3 into a combined column 
      melt_fitted_df <- melt(fitted_df, id.var = names, variable.name = "fitted_model_name")
      names(melt_fitted_df)[names(melt_fitted_df) == "value"] <- "fitted"
    
    # Subsetting melt_fitted so that it can be merged with melt_resid without duplicating data
      melt_fitted_df_subset <- subset(melt_fitted_df, select = c("fitted_model_name", "fitted"))
    
    # cbind to merge datasets. Works because the values line up with each other
    # Dont need a key variable for the merge
      melt_graphing_df[[i]] <-cbind(melt_resid_df, melt_fitted_df_subset)
    
  }
  
  # Function Output
  return(melt_graphing_df)
  
}



# 6. For-Loop Graphing Diagnostic plots Resid Vs Fitted -----------------------

graphingMeltFunction <-  function(df) {
  
  for (i in 1:length(df)) {
    
    my_title <- df[[i]][["climatic_var"]]
    
    print(ggplot(data = df[[i]]) + 
            geom_point(aes(x = fitted, y = residaul, colour = resid_model_name), size = 0.25) +
            facet_wrap( ~ location, nrow = 2) +
            scale_colour_discrete(labels=c('Model 1', 'Model 2', "Model 3")) +
            theme(legend.title=element_blank()) +
            labs(title = my_title, x = "Fitted", y = "Residuals"))
    
    # Saving plots as PDFs
    # Long piece of code that just specifics the name of the file
    # Could be done manually if wanted 
    ggsave(paste0(Sys.Date(),
      
      # check to see if it a natural log transformed dataset            
      if (grepl("ln_", names(df[1]))) {
        
        paste0("_ln_")
        
      } else {
        
        paste0("_")
        
      },
      
      # check to see if the treatment is harvest or tree cover
      if (grepl("harvest", names(df[1]))) {
        
        paste("harvest_")
        
      } else {
        
        paste("canopy_")
      
    }, 
    
    "mod_123_resid_fit_", df[[i]][["climatic_var"]], ".pdf"))
    
  }
  
} 


# graphingFunction <- function() {
  
#  for (i in 1:length(graphing_df)) {
    
    # Title for individual plots
#    my_title <- graphing_df[[i]][["climatic_var"]]
    
#    print(ggplot(data = graphing_df[[i]]) +
            # model 1 = BLUE
#            geom_point(aes(x = fitted_model_1,
#                           y = resid_model_1), 
#                       colour = "blue", size = 0.25) +
            # model 2 = GREEN
#            geom_point(aes(x = fitted_model_2,
#                           y = resid_model_2),
#                       colour = "green", size = 0.25) +
            # model 3 = RED
#            geom_point(aes(x = fitted_model_3,
#                           y = resid_model_3),
#                       colour = "red", size = 0.25) +
            # Wrapping by location
#            facet_wrap( ~ location, nrow = 2) +
#            labs(title = my_title, x = "Fitted", y = "Residuals"))
    
    # Saving plots as PDFs
#    ggsave(paste0("20231218_model_1_2_3_resid_fitted_", graphing_df[[i]][["climatic_var"]], ".pdf"))
#  }
  
  # Function output is the printed plots 
  
# }