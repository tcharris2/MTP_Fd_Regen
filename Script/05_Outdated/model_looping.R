# Contents: Vertical Model Loop ------------------------------------------------

# Author: Thomson Harris 
# Date: Oct 4th 2023

# Creating the required Data Structures ----------------------------------------

# Nesting by location 
loc_group <- regen %>% 
  group_by(location) %>% 
  nest()

# Creating a list of climatic variables 
ClimaticVarList <- c("d_AHM", "d_CMD", "d_CMI", "d_DD_0", "d_DD_18", "d_DD1040",
                       "d_DD18", "d_DD5", "d_EMT", "d_Eref", "d_EXT", "d_FFP", 
                       "d_MAP", "d_MAT", "d_MCMT", "d_MSP", "d_MWMT", "d_NFFD",
                       "d_PAS", "d_RH", "d_SHM", "d_TD")


# Creating Model Function ------------------------------------------------------


# Null models only needs 6 repeats as nothing changes in it 

modelNull <- function(df) {
  
  glmer(survival ~ 1 + (1|plotF/splitplotF), data = df,
        family = binomial)
}


# Model_1: Model Containing only the climatic variables 
# Stored as a large list inside the dataframe 

allClimaticVars_1 <- function(df) {
  
  # Create an empty list to fill 
    results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
  # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ (1|plotF/splitplotF)")), 
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

allClimaticVars_2 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste(" + harvestF + (1|plotF/splitplotF)")), 
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

allClimaticVars_3 <- function(df) {
  
  # Create an empty list to fill 
  results <- list() 
  
  # Loop over the variables
  for (var in ClimaticVarList) {
    # Perform the regression
    model <- glmer(paste("survival ~", var, paste("+ harvestF *"), var, paste(" + harvestF + (1|plotF/splitplotF)")), 
                   data = df, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    # Store the results in the list
    results[[var]] <- model
  }
  # Create an output
  results
  
}


# Storing models in a tibble -------------------------------------------------

# Create a new tibble 
loc_group_models <- loc_group

# Run the model functions with mutate to store them 
loc_group_models <- loc_group_models %>% 
  mutate(model_0 = map(data, modelNull))

loc_group_models <- loc_group_models %>% 
  mutate(model_1 = map(data, allClimaticVars_1))

loc_group_models <- loc_group_models %>% 
  mutate(model_2 = map(data, allClimaticVars_2))

loc_group_models <- loc_group_models %>% 
  mutate(model_3 = map(data, allClimaticVars_3))


# looking at the models within the tibble 
loc_group_models

loc_group_models$model_1
loc_group_models$model_2
loc_group_models$model_3




