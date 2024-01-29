# 3 functions that are used for testing models
# function to test models
lrtest_function <- function(test_models) {
  lrtest(test_models$model.x[[1]], test_models$model.y[[1]]) %>%
    nest()
}

# function to join data frames
model_join <- function() {
  
  models_df <- left_join(models.0, models.1, by ='location')
  models_df$lr_test <- NA
  
  models_df
}
# this is the only function that will need to be modified for different tests

# function to iterate test across all models

models_test <- function (){
  
  models_df <- model_join()
  
  for (i in 1:nrow(models_df)) {
    test_models <- c(models_df[i, 3], models_df[i, 5])
    models_df$lr_test[i] <- lrtest_function(test_models)
  }
  
  names(models_df$lr_test) <- c("Alex Fraser", "Jaffray", "John Prince", 
                                "Redfish")
  models_df$lr_test
  
}
