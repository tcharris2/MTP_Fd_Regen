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


# function to iterate test across all models -----------------------------------

modelsTest_0_1 <- function (df){ # Function head
  
  df$lr_test_1 <- NA  # Creating a place for the test results to be stored. 
  
  for (i in 1:nrow(df)) { # Head of the for loop
    
    test_models <- c(df$model_0[[i]], df$model_1[[i]]) # index is important. 
                                                                 # If the data structure were to change this would need to change
    df$lr_test_1[i] <- lrtestFunction(test_models)         # Testing and storing the models 
    
  }
  
  df$lr_test_1       # Creating a function output  
}

modelsTest_1_2 <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_2 <- NA
  
  for (i in 1:nrow(df)) { 
    
    test_models <- c(df$model_1[[i]], df$model_2[[i]])
    df$lr_test_2[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_2
}


modelsTest_2_3 <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_3 <- NA
  
  for (i in 1:nrow(df)) {  
    
    test_models <- c(df$model_2[[i]], df$model_3[[i]])
    df$lr_test_3[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_3
}

modelsTest_0_h <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_0h <- NA
  
  for (i in 1:nrow(df)) {  
    
    test_models <- c(df$model_0[[i]], df$model_h[[i]])
    df$lr_test_0h[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_0h
}

modelsTest_h_2 <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_h2 <- NA
  
  for (i in 1:nrow(df)) {  
    
    test_models <- c(df$model_h[[i]], df$model_2[[i]])
    df$lr_test_h2[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_h2
}

modelsTest_0_c <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_0c <- NA
  
  for (i in 1:nrow(df)) {  
    
    test_models <- c(df$model_0[[i]], df$model_c[[i]])
    df$lr_test_0c[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_0c
}

modelsTest_c_2 <- function (df){ # see "modelsTest_0_1 for fully annotated function
  
  df$lr_test_c2 <- NA
  
  for (i in 1:nrow(df)) {  
    
    test_models <- c(df$model_c[[i]], df$model_2[[i]])
    df$lr_test_c2[i] <- lrtestFunction(test_models)
    
  }
  
  df$lr_test_c2
}

# Extracting p-values from models ----------------------------------------------

# extract values for test one
pValExtract <- function(df) {
  
  # create empty columns
  df$p_val_0_1 <- NA
  df$p_val_1_2 <- NA
  df$p_val_2_3 <- NA
  df$p_val_0_trt <- NA
  df$p_val_trt_2 <- NA
  
  
  
  # Populate the data frame. Index placement (first value) is important 
  for (i in 1:nrow(df)) {
    p_values <- c(df$lr_test_0_1[[i]][["Pr(>Chisq)"]][[2]]) 
    df$p_val_0_1[i] <- p_values
  }
  
  for (i in 1:nrow(df)) {
    p_values <- c(df$lr_test_1_2[[i]][["Pr(>Chisq)"]][[2]])
    df$p_val_1_2[i] <- p_values
  }
  
  for (i in 1:nrow(df)) {
    p_values <- c(df$lr_test_2_3[[i]][["Pr(>Chisq)"]][[2]])
    df$p_val_2_3[i] <- p_values
  }
  
  for (i in 1:nrow(df)) {
    p_values <- c(df$lr_test_0_trt[[i]][["Pr(>Chisq)"]][[2]])
    df$p_val_0_trt[i] <- p_values
  }
  
  for (i in 1:nrow(df)) {
    p_values <- c(df$lr_test_trt_2[[i]][["Pr(>Chisq)"]][[2]])
    df$p_val_trt_2[i] <- p_values
  }
  
  df
}

# Removing Non-Significant P-Values --------------------------------------------

removeNonSigPVals <- function (df) { # Function that removes all the non-significant p-values 
  
  p_val_sig <- df
  p_val_sig$p_val_0_1[p_val_sig$p_val_0_1 >= 0.05] <- NA
  p_val_sig$p_val_1_2[p_val_sig$p_val_1_2 >= 0.05] <- NA
  p_val_sig$p_val_2_3[p_val_sig$p_val_2_3 >= 0.05] <- NA
  p_val_sig$p_val_0_trt[p_val_sig$p_val_0_trt >= 0.05] <- NA
  p_val_sig$p_val_trt_2[p_val_sig$p_val_trt_2 >= 0.05] <- NA
  
  p_val_sig
}


