#' @Content: Functions needed to create GLMER models for survival ----------------
  
  #' @Author: Thomson Harris 
  #' @Date: Oct 4th 2023
  
# Climatic Variables Functions --------------------------------------------------------------

climaticVarListFunction <- function() {
  
  ClimaticVarList <- names(regen %>% select(starts_with("d_")))
  
  ClimaticVarList
  
}

ClimaticVarList <- climaticVarListFunction()

# Model Functions --------------------------------------------------------------

# NullModel 
groupHeightModelNull <- function(df) {
  
  lmer(height ~ 1 + (1|locationF/blockF/plotF/splitplotF), data = df,
        REML = FALSE)
}


# HarvestModel 
groupHeightModelHarvest <- function(df) {
  
  lmer(height ~ harvestF + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE)
}


# CanopyModel 
groupHeightModelCanopy <- function(df) {
  
  lmer(height ~ tree_cover + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE)
}


# AgeModel 
groupHeightModelAge <- function(df) {
  
  lmer(height ~ age + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE)
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

groupHeightHarvest_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  

# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

groupHeightHarvest_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
} 


# Model_2: Model Containing the climatic variables and harvestF term
# Stored as a large list inside the dataframe 

groupHeightHarvest_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste(" + harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}

# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

groupHeightHarvest_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste(" + harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}



# Model_3: Model Containing the climatic variables, harvestF, and interaction term
# Stored as a large list inside the dataframe

groupHeightHarvest_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ harvestF +"), var, paste(" * harvestF + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

groupHeightHarvest_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ harvestF +"), var, paste(" * harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

groupHeightCanopy_1 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}  


# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

groupHeightCanopy_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
} 


# Model_2: Model Containing the climatic variables and harvestF term
# Stored as a large list inside the dataframe 

groupHeightCanopy_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste(" + tree_cover + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}

# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

groupHeightCanopy_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste(" + tree_cover + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}



# Model_3: Model Containing the climatic variables, harvestF, and interaction term
# Stored as a large list inside the dataframe

groupHeightCanopy_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ tree_cover +"), var, paste(" * tree_cover + (1|locationF/blockF/plotF/splitplotF)")), 
                   data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

groupHeightCanopy_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("height ~", var, paste("+ tree_cover +"), var, paste(" * tree_cover + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE)
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}