#' @Content: Functions needed to create GLMER models for survival ----------------
  
  #' @Author: Thomson Harris 
  #' @Date: Oct 4th 2023
  


# Model Functions --------------------------------------------------------------

# NullModel 
ln_HeightModelNull <- function(df) {
  
  lmer(log(height) ~ 1 + (1|locationF/blockF/plotF/splitplotF), data = df,
        REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# HarvestModel 
ln_HeightModelHarvest <- function(df) {
  
  lmer(log(height) ~ harvestF + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# CoverModel 
ln_HeightModelCover <- function(df) {
  
  lmer(log(height) ~ sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}





# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

ln_HeightClimate_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste("+ (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  




# Model_2: Model Containing the climatic variables and harvestF term
# Stored as a large list inside the dataframe 

ln_HeightHarvest_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" + harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}





# Model_3: Model Containing the climatic variables, harvestF, and interaction term
# Stored as a large list inside the dataframe

ln_HeightHarvest_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" * harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}



# Model_2: Model Containing the climatic variables and harvestF term
# Stored as a large list inside the dataframe 

ln_HeightCover_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" + sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_3: Model Containing the climatic variables, harvestF, and interaction term
# Stored as a large list inside the dataframe

ln_HeightCover_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" * sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}
