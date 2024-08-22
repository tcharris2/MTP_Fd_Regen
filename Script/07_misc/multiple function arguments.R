

modelsTest <- function (df, model_x, model_y){ # Function head
  
  df$lrtest <- NA  # Creating a place for the test results to be stored. 
  
  for (i in 1:nrow(df)) { # Head of the for loop
    
    test_models <- c(model_x[[i]], model_y[[i]]) # index is important. 
    # If the data structure were to change this would need to change
    df$lrtest[i] <- lrtestFunction(test_models)         # Testing and storing the models 
    
  }
  
  df$lrtest       # Creating a function output  
}


survival_group_harvest_models_df$lr_test_a_b <- unlist(modelsTest(df = survival_group_harvest_models_df, 
                                                                  model_x = survival_group_harvest_models_df$model_0,
                                                                  model_y = survival_group_harvest_models_df$model_h), 
                                                       recursive = FALSE)

survival_group_harvest_models_df$lr_test_1_2 <- unlist(modelsTest(df = survival_group_harvest_models_df, 
                                                                  a = survival_group_harvest_models_df$model_1,
                                                                  b = survival_group_harvest_models_df$model_2), 
                                                       recursive = FALSE)

survival_group_harvest_models_df$lr_test_0_1
survival_group_harvest_models_df$lr_test_1_2

survival_group_harvest_models_df[["model_0"]]


test <- function (x, y, z) {
  
  obj <- as.numeric(c("x", "y", "z"))
  
  obj2 <- sum(obj)
  
  return(obj2)
  
}

test(1, 2, 3)


obj <- as.numeric(c("1", "2", "3"))
sum(obj)
