# CONTENT: Graphing - Survival (Harvest) ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date: Feb 13th, 2024

# 1. Importing Data -------------------------------------------------------------

# calling RDS file 

survival_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-02-05_survival_group_harvest_models_NoFutures.rds"))

survival_harvest_models

# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(survival_harvest_models) <- c("ClimaticVarList", "model_0", "model_c", "model_a", 
                                         "model_1", "model_1a", "model_2", "model_2a", 
                                         "model_3", "model_3a")


model_1 <- survival_harvest_models[c(1:6, 10, 12:13), c("ClimaticVarList", "model_1")]

model_3 <- survival_harvest_models[c(8:9, 11, 15), c("ClimaticVarList", "model_3")]


