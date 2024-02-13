# CONTENT: Graphing - Survival (Harvest) ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data -------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)
# check for most recent CSV file

#### 1.1 Importing Functions --------

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


#### 1.2 Correcting Variable types -------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

regen_survival <- subset(regen_survival, 
                         !regen_survival$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                          "Peterhope future Fd", "Alex Fraser future Fd", 
                                                          "Twobit B class Fd"))

str(regen_survival)

#### 1.3 Calling RDS file  ------

survival_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-02-05_survival_group_harvest_models_NoFutures.rds"))

survival_harvest_models

# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(survival_harvest_models) <- c("ClimaticVarList", "model_0", "model_c", "model_a", 
                                         "model_1", "model_1a", "model_2", "model_2a", 
                                         "model_3", "model_3a")


climatic_models <- survival_harvest_models[c(1:6, 10, 12:13), c("ClimaticVarList", "model_1")]

inter_models <- survival_harvest_models[c(8:9, 11, 15), c("ClimaticVarList", "model_3")]


# 3. Clean up Global env. ------------------------------------------------------

rm(regen, regen_prepped, universalDataPrepFunction)


# 4. Estimated Probability of Survival --------------------------------------------------

##### 4.1 Adding data -----

climatic_models$data <- list(regen_survival)

climatic_models


survivalProbs <- function (df, model_column) {
  
  for (i in 1:length(model_column)) {
    
    # Creates new column "survival_probs" and fills it with the estimated probabilities 
    df$data[[i]][["survival_probs"]] <- (exp(fitted(model_column[[i]]))) / (1 + exp(fitted(model_column[[i]])))
    
  }
  
  # Function output
  return(df)
  
}

fun_test <- survivalProbs(climatic_models, climatic_models$model_1)


c_var <- "d_MAT"


ggplot(data = fun_test$data[[1]], mapping = aes(x = .data[[c_var]], y = survival_probs)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  labs(title = c_var, x = paste(c_var, "Climatic Distance"), y = "Estimated Probability of Survival")


graphingESTSurvivalProb <-  function(df) {
  
  for (i in 1:length(df)) {
    
    c_var <- df[["ClimaticVarList"]][[i]]
    
    print(ggplot(data = df$data[[i]], mapping = aes(x = noqoute(c_var), y = survival_probs)) + 
            geom_point() +
            geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
            labs(title = c_var, x = paste(c_var, "Climatic Distance"), y = "Estimated Probability of Survival") +
            theme(legend.title=element_blank()))
          
    
    # Saving plots as PDFs
    # Long piece of code that just specifics the name of the file
    # Could be done manually if wanted 
    ggsave(paste0(Sys.Date(),
                  
                  # check to see if it a natural log transformed dataset            
                  if (grepl("ln_", names(df[1]))) {
                    
                    paste0("_ln_")
                    
                  } else {
                    
                    paste0("_")
                    
                  },
                  
                  # check to see if the treatment is harvest or tree cover
                  if (grepl("harvest", names(df[1]))) {
                    
                    paste("harvest_")
                    
                  } else {
                    
                    paste("canopy_")
                    
                  }, 
                  
                  "mod_123_resid_fit_", df[[i]][["climatic_var"]], ".pdf"))
    
  }
  
} 

