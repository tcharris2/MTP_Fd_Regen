library(reshape2)

# This runs  ---------------------------------------------------------------------

verticallyExpandGraphingDF <- function(df) {
  
  # Create a new list object to store function output 
  melt_graphing_df <- list(NA)
  
  # For-loop to melt residuals/fitted values into a single column
  for(i in 1:length(graphing_df)) {
    
    # Pull the ith dataframe from the list
      singular_df <- df[[i]]
    
    # create two seperate dataframes to operate on
     resid_df <- singular_df[-c(56:58)]
     fitted_df <- singular_df[-c(53:55)]
    
    # pull the names from the dataframe
     names <- names(singular_df)
    
    # Remove the residaul/fitted names. Needed to melt the appropriate columns
      names <- names[-c(53:58)]

    
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

melt_graphing_df <- verticallyExpandGraphingDF(graphing_df)



# This version also runs ---------------------------------------------------------------------

middleFunction <-  function(test_df) {
  
  test_df_r <- test_df[-c(56:58)]
  test_df_f <- test_df[-c(53:55)]
  
  
  names <- names(test_df)
  names <- names[-c(53:58)]
  
  
  melt_df_r <- melt(test_df_r, id.var = names, variable.name = "model_r")

  names(melt_df_r)[names(melt_df_r) == "value"] <- "residaul"

  
  
  melt_df_f <- melt(test_df_f, id.var = names, variable.name = "model_f")

  names(melt_df_f)[names(melt_df_f) == "value"] <- "fitted"

  melt_df_f_subset <- subset(melt_df_f, select = c("model_f", "fitted"))
  
  
  merged_test_df <- cbind(melt_df_r, melt_df_f_subset)
  
  return(merged_test_df)
  
}



verticallyExpandGraphingDF <- function(df) {
  
  merged_graphing_df <- list(NA)
  
  for(i in 1:length(graphing_df)) {
    
    test_df <- df[[i]]
    
    merged_test_df <- list(middleFunction(test_df))
    
    merged_graphing_df[i] <- merged_test_df
    
  }
  
  return(merged_graphing_df)
  
}


merged_graphing_df <- verticallyExpandGraphingDF(graphing_df)





# This is the tester  ---------------------------------------------------------------------

test_df <- graphing_df[[1]]


test_df_r <- test_df[-c(56:58)]

test_df_f <- test_df[-c(53:55)]


names <- names(test_df)
names
names <- names[-c(53:58)]
names


melt_df_r <- melt(test_df_r, id.var = names, variable.name = "model_r")
melt_df_r
names(melt_df_r)[names(melt_df_r) == "value"] <- "residaul"
melt_df_r


melt_df_f <- melt(test_df_f, id.var = names, variable.name = "model_f")
melt_df_f
names(melt_df_f)[names(melt_df_f) == "value"] <- "fitted"
melt_df_f
melt_df_f_subset <- subset(melt_df_f, select = c("model_f", "fitted"))




test_merge <- cbind(melt_df_r, melt_df_f_subset)



# Graphing ---------------------------------------------------------------------


graphingMeltFunction <-  function() {
  
  for (i in 1:length(melt_graphing_df)) {
    
    my_title <- melt_graphing_df[[i]][["climatic_var"]]
    
    print(ggplot(data = melt_graphing_df[[i]]) + 
            geom_point(aes(x = fitted, y = residaul, colour = resid_model_name), size = 0.25) +
            facet_wrap( ~ location, nrow = 2) +
            scale_colour_discrete(labels=c('Model_1', 'Model_2', "Model_3")) +
            theme(legend.title=element_blank()) +
            labs(title = my_title, x = "Fitted", y = "Residuals"))

    # Saving plots as PDFs
    ggsave(paste0(Sys.Date(), "_model_1_2_3_resid_fitted_", melt_graphing_df[[i]][["climatic_var"]], ".pdf"))
    
  }
  
} 


graphingMeltFunction()

print(Sys.Date())

paste(Sys.Date())

paste0(Sys.Date(), "_model_1_2_3_resid_fitted_")

ggplot(data = test_merge) + 
  geom_point(aes(x = fitted, y = residaul, colour = model_r), size = 0.25) +
  facet_wrap( ~ location, nrow = 2) +
  scale_colour_discrete(labels=c('Model_1', 'Model_2', "Model_3")) +
  theme(legend.title=element_blank())
