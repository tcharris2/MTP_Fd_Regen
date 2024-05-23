# Content: lrtest Function -----------------------------------------------------

# Author: Thomson Harris
# Date: 5th Oct, 2023

# Function to test models ------------------------------------------------------

lrtestFunction <- function(test_models) {
  lrtest(test_models[[1]], test_models[[2]]) %>%  # Test models is temporarily created list used for iteration
    nest()                                        # Nesting to be able to store the output in a tibble
}

# function to iterate test across all models -----------------------------------

modelsTest_0_1 <- function (){ # Function head
  
  loc_models$lr_test_1 <- NA  # Creating a place for the test results to be stored. 
                              # This label is arbitrary but placement (loc_model$) is i believe important 
  
  for (i in 1:nrow(loc_models)) { # Head of the for loop
    
    test_models <- c(loc_models$model_0[[i]], loc_models$model_1[[i]]) # index is important. 
                                                                 # If the data structure were to change this would need to change
    loc_models$lr_test_1[i] <- lrtestFunction(test_models)         # Testing and storing the models 
    
  }
  
  loc_models$lr_test_1       # Creating a function output  
}

modelsTest_1_2 <- function (){ # see "modelsTest_0_1 for fully annotated function
  
  loc_models$lr_test_2 <- NA
  
  for (i in 1:nrow(loc_models)) { 
    
    test_models <- c(loc_models$model_1[[i]], loc_models$model_2[[i]])
    loc_models$lr_test_2[i] <- lrtestFunction(test_models)
    
  }
  
  loc_models$lr_test_2
}


modelsTest_2_3 <- function (){ # see "modelsTest_0_1 for fully annotated function
  
  loc_models$lr_test_3 <- NA
  
  for (i in 1:nrow(loc_models)) {  
    
    test_models <- c(loc_models$model_2[[i]], loc_models$model_3[[i]])
    loc_models$lr_test_3[i] <- lrtestFunction(test_models)
    
  }
  
  loc_models$lr_test_3
}

modelsTest_0_h <- function (){ # see "modelsTest_0_1 for fully annotated function
  
  loc_models$lr_test_0h <- NA
  
  for (i in 1:nrow(loc_models)) {  
    
    test_models <- c(loc_models$model_0[[i]], loc_models$model_h[[i]])
    loc_models$lr_test_0h[i] <- lrtestFunction(test_models)
    
  }
  
  loc_models$lr_test_0h
}

modelsTest_h_2 <- function (){ # see "modelsTest_0_1 for fully annotated function
  
  loc_models$lr_test_h2 <- NA
  
  for (i in 1:nrow(loc_models)) {  
    
    test_models <- c(loc_models$model_h[[i]], loc_models$model_2[[i]])
    loc_models$lr_test_h2[i] <- lrtestFunction(test_models)
    
  }
  
  loc_models$lr_test_h2
}

# Extracting p-values from models ----------------------------------------------

# extract values for test one
pValExtract <- function() {
  
  # create empty columns
  loc_models$p_val_0_1 <- NA
  loc_models$p_val_1_2 <- NA
  loc_models$p_val_2_3 <- NA
  loc_models$p_val_0_h <- NA
  loc_models$p_val_h_2 <- NA
  
  
  
  # Populate the data frame. Index placement (first value) is important 
  for (i in 1:nrow(loc_models)) {
    p_values <- c(loc_models$lr_test_0_1[[i]][["Pr(>Chisq)"]][[2]]) 
    loc_models$p_val_0_1[i] <- p_values
  }
  
  for (i in 1:nrow(loc_models)) {
    p_values <- c(loc_models$lr_test_1_2[[i]][["Pr(>Chisq)"]][[2]])
    loc_models$p_val_1_2[i] <- p_values
  }
  
  for (i in 1:nrow(loc_models)) {
    p_values <- c(loc_models$lr_test_2_3[[i]][["Pr(>Chisq)"]][[2]])
    loc_models$p_val_2_3[i] <- p_values
  }
  
  for (i in 1:nrow(loc_models)) {
    p_values <- c(loc_models$lr_test_0_h[[i]][["Pr(>Chisq)"]][[2]])
    loc_models$p_val_0_h[i] <- p_values
  }
  
  for (i in 1:nrow(loc_models)) {
    p_values <- c(loc_models$lr_test_h_2[[i]][["Pr(>Chisq)"]][[2]])
    loc_models$p_val_h_2[i] <- p_values
  }
  
  loc_models
}

# Removing Non-Significant P-Values --------------------------------------------

removeNonSigPVals <- function () { # Function that removes all the non-signficant p-values 
  
  p_val_sig <- p_val_df
  p_val_sig$p_val_0_1[p_val_sig$p_val_0_1 >= 0.05] <- NA
  p_val_sig$p_val_1_2[p_val_sig$p_val_1_2 >= 0.05] <- NA
  p_val_sig$p_val_2_3[p_val_sig$p_val_2_3 >= 0.05] <- NA
  p_val_sig$p_val_0_h[p_val_sig$p_val_0_h >= 0.05] <- NA
  p_val_sig$p_val_h_2[p_val_sig$p_val_h_2 >= 0.05] <- NA
  
  p_val_sig
}


