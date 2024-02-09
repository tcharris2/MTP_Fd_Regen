# Content: Functions needed to create GLMER models for survival ----------------

# Will create functions that run all the specified climatic variables 
# created in the vector "ClimaticVarList". 
# If more climatic variables are wanted they need to be manually entered 
# into "climaticVarListFunction"

# Author: Thomson Harris 
# Date: Oct 4th 2023

# Model Functions --------------------------------------------------------------


# Null models only needs 6 repeats as nothing changes in it 
survivalModelNullS <- function(df) {
  
  glmer(survival ~ 1 + (1|splitplotF), data = df,
        family = binomial)
}

# Models with only harvest. Needs 6 repeats as nothing changes in it 

survivalModelCoverS <- function(df) {
  
  glmer(survival ~ tree_cover + (1|splitplotF), data = df,
        family = binomial)
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

survivalCover_1S <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ (1|splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  



# Model_2: Model Containing the climatic variables and harvestF term
# Stored as a large list inside the dataframe 

survivalCover_2S <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + tree_cover + (1|splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}



# Model_3: Model Containing the climatic variables, harvestF, and interaction term
# Stored as a large list inside the dataframe

survivalCover_3S <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ tree_cover +"), var, paste(" * tree_cover + (1|splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}
