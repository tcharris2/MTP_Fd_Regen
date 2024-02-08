# Content: Functions needed to create GLMER models for survival ----------------

# Will create functions that run all the specified climatic variables 
# created in the vector "ClimaticVarList". 
# If more climatic variables are wanted they need to be manually entered 
# into "climaticVarListFunction"

# Author: Thomson Harris 
# Date: Oct 4th 2023

# Model Functions --------------------------------------------------------------


# Null models only needs 6 repeats as nothing changes in it 
ln_heightHarvestNull <- function(df) {
  
  lmer(ln_height ~ 1 + (1|blockF/plotF/splitplotF), data = df,
       REML = FALSE,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}

# Models with only harvest. Needs 6 repeats as nothing changes in it 

ln_heightHarvest <- function(df) {
  
  lmer(ln_height ~ harvestF + (1|blockF/plotF/splitplotF), data = df,
       REML = FALSE,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

ln_heightHarvest_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("ln_height ~", var, paste("+ (1|blockF/plotF/splitplotF)")), 
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

ln_heightHarvest_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("ln_height ~", var, paste(" + harvestF + (1|blockF/plotF/splitplotF)")), 
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

ln_heightHarvest_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("ln_height ~", var, paste("+ harvestF +"), var, paste(" * harvestF + (1|blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}
