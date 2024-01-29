#' @Content: Functions needed to create GLMER models for survival ----------------
  
  #' @Author: Thomson Harris 
  #' @Date: Oct 4th 2023
  
# Climatic Variables Functions --------------------------------------------------------------

climaticVarListFunction <- function(df) {
  
  ClimaticVarList <- names(df %>% select(starts_with("d_")))

  ClimaticVarList
  
}

# Model Functions --------------------------------------------------------------

# NullModel 
groupSurvivalModelNull <- function(df) {
  
  glmer(survival ~ 1 + (1|locationF/blockF/plotF/splitplotF), data = df,
        family = binomial)
}


# HarvestModel 
groupSurvivalModelHarvest <- function(df) {
  
  glmer(survival ~ harvestF + (1|locationF/blockF/plotF/splitplotF), data = df,
        family = binomial)
}


# CanopyModel 
groupSurvivalModelCanopy <- function(df) {
  
  glmer(survival ~ tree_cover + (1|locationF/blockF/plotF/splitplotF), data = df,
        family = binomial)
}

# AgeModel
groupSurvivalModelAge <- function(df) {
  
  glmer(survival ~ age + (1|locationF/blockF/plotF/splitplotF), data = df,
        family = binomial)
}

# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

groupSurvivalHarvest_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  

# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

groupSurvivalHarvest_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
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

groupSurvivalHarvest_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}

# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

groupSurvivalHarvest_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
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

groupSurvivalHarvest_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ harvestF +"), var, paste(" * harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}

# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

groupSurvivalHarvest_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ harvestF +"), var, paste(" * harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

groupSurvivalCanopy_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  

# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

groupSurvivalCanopy_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
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

groupSurvivalCanopy_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + tree_cover + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

groupSurvivalCanopy_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + tree_cover + age + (1|locationF/blockF/plotF/splitplotF)")), 
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

groupSurvivalCanopy_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ tree_cover +"), var, paste(" * tree_cover + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

groupSurvivalCanopy_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ tree_cover +"), var, paste(" * tree_cover + age + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}
