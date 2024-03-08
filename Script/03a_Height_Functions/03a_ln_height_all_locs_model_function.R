#' @Content: Functions needed to create GLMER models for survival ----------------
  
  #' @Author: Thomson Harris 
  #' @Date: Oct 4th 2023
  


# Model Functions --------------------------------------------------------------

# NullModel 
ln_groupHeightModelNull <- function(df) {
  
  lmer(log(height) ~ 1 + (1|locationF/blockF/plotF/splitplotF), data = df,
        REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# HarvestModel 
ln_groupHeightModelHarvest <- function(df) {
  
  lmer(log(height) ~ harvestF + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# CoverModel 
ln_groupHeightModelCover <- function(df) {
  
  lmer(log(height) ~ sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# AgeModel 
ln_groupHeightModelAge <- function(df) {
  
  lmer(log(height) ~ age + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# AgeHarvestModel 
ln_groupHeightModelAgeHarvest <- function(df) {
  
  lmer(log(height) ~ age + harvestF + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# AgeCovertModel 
ln_groupHeightModelAgeCover <- function(df) {
  
  lmer(log(height) ~ age + sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), data = df,
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

ln_groupHeightHarvest_1 <- function(df) {
  
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

# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

ln_groupHeightHarvest_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
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

ln_groupHeightHarvest_2 <- function(df) {
  
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

# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

ln_groupHeightHarvest_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" + harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
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

ln_groupHeightHarvest_3 <- function(df) {
  
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


# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

ln_groupHeightHarvest_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" * harvestF + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

ln_groupHeightCover_1 <- function(df) {
  
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


# Model_1a: Model Containing only the climatic variables and AGE
# Stored as a large list inside the dataframe 

ln_groupHeightCover_1a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste("+ age + (1|locationF/blockF/plotF/splitplotF)")), 
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

ln_groupHeightCover_2 <- function(df) {
  
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

# Model_2a: Model Containing the climatic variables and harvestF term and AGE
# Stored as a large list inside the dataframe 

ln_groupHeightCover_2a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" + sqrt(tree_cover) + age + (1|locationF/blockF/plotF/splitplotF)")), 
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

ln_groupHeightCover_3 <- function(df) {
  
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


# Model_3a: Model Containing the climatic variables, harvestF, and interaction term and AGE
# Stored as a large list inside the dataframe

ln_groupHeightCover_3a <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- lmer(paste("log(height) ~", paste0("scale(", var, ")"), paste(" * sqrt(tree_cover) + age + (1|locationF/blockF/plotF/splitplotF)")), 
                  data = df, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}