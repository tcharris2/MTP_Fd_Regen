# 1. Grouped Location Diagnostic Columns -----------------------------------

groupDiagnosticColsFunction <- function (df) {
  
  # Outer for-loop allowing for operations across multiple rows 
  for(j in 1:nrow(df)) {
    
    # Create list of names to modify
    list_names <- names(df %>% select(contains("model")))
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

groupExtractMetrics <- function(df) { 
  
  # Create a list of model names
  model_list <- names(df %>% select(contains("model")))
  
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


# 3. Melting Dataframes ---------------------------------------------------------

groupMeltDF <-function(df) {
  
  df$melt_data <- list(NA)
  
  for(i in 1:nrow(df)) {
    
    test_df <- df$data[[i]]
    
    test_resid_names <- names(test_df %>% select(contains("resid")))
    test_resid_df <- subset(test_df, select = c("tree_number", test_resid_names))
    
    
    melt_resid_df <- melt(test_resid_df, id.var = "tree_number", variable.name = "resid_model_name")
    
    names(melt_resid_df)[names(melt_resid_df) == "value"] <- "residaul"
    
    
    test_fitted_names <- names(test_df %>% select(contains("fitted")))
    test_fitted_df <- subset(test_df, select = c("tree_number", test_fitted_names))
    
    
    melt_fitted_df <- melt(test_fitted_df, id.var = "tree_number", variable.name = "fitted_model_name")
    
    names(melt_fitted_df)[names(melt_fitted_df) == "value"] <- "fitted"
  
    
    melt_fitted_df_subset <- subset(melt_fitted_df, select = -c(tree_number))
    
    melt_graphing_df <-cbind(melt_resid_df, melt_fitted_df_subset)
    
    df$melt_data[[i]] <- melt_graphing_df
  }
  
  # Function Output
  return(df)
  
}

# 4. Resid vs Fitted Graphing -------------------------------------------------------------------

groupGraphingMeltFunction <-  function(df) {
  
  for (i in 1:nrow(df)) {
    
    my_title <- df$ClimaticVarList[[i]]
    
    print(ggplot(data = df$melt_data[[i]]) + 
            geom_point(aes(x = fitted, y = residaul, colour = resid_model_name), size = 0.25) +
    
            theme(legend.title=element_blank()) +
            labs(title = my_title, x = "Fitted", y = "Residuals"))
    
    # Saving plots as PDFs
    # Long piece of code that just specifics the name of the file
    # Could be done manually if wanted 
    ggsave(paste0(Sys.Date(), paste0("_group_"), my_title,
                  
                  # check to see if it a natural log transformed dataset            
                  if (grepl("ln_", df[4][[1]][[1]])) {
                    
                    paste0("_ln_")
                    
                  } else {
                    
                    paste0("_")
                    
                  },
                  
                  # check to see if the treatment is harvest or tree cover
                  if (grepl("harvest", df[4][[1]][[1]])) {
                    
                    paste("harvest_")
                    
                  } else {
                    
                    paste("canopy_")
                    
                  }, 
                  
                  ".pdf"))
    
  }
  
  } 


# 4. QQ plots Graphing -------------------------------------------------------------------

groupQQGraphingFunction <-  function(df) {
  
  for (i in 1:nrow(df)) {
    
    my_title <- df$ClimaticVarList[[i]]
    
    print(ggplot(data = df$melt_data[[i]]) + 
            geom_qq(aes(sample = residaul, colour = resid_model_name), size = 0.25) +
            
            theme(legend.title=element_blank()) +
            labs(title = my_title))
    
    # Saving plots as PDFs
    # Long piece of code that just specifics the name of the file
    # Could be done manually if wanted 
    ggsave(paste0(Sys.Date(), paste0("_QQplot_group_"), my_title,
                  
                  # check to see if it a natural log transformed dataset            
                  if (grepl("ln_", df[4][[1]][[1]])) {
                    
                    paste0("_ln_")
                    
                  } else {
                    
                    paste0("_")
                    
                  },
                  
                  # check to see if the treatment is harvest or tree cover
                  if (grepl("harvest", df[4][[1]][[1]])) {
                    
                    paste("harvest")
                    
                  } else {
                    
                    paste("canopy")
                    
                  }, 
                  
                  ".pdf"))
    
  }
  
} 
