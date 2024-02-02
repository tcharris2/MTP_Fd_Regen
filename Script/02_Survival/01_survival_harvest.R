# Content: Survival ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/01_survival_harvest_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

source("Script/01a_Survival_Functions/lrtest_function.R")


# Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)


# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# Grouping Data ----------------------------------------------------------------

loc_group_harvest <- regen_survival %>% 
  group_by(location) %>% 
  nest()

# Storing models in a tibble ---------------------------------------------------

# Create a new tibble 
survival_harvest_models <- loc_group_harvest

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_harvest_models <- survival_harvest_models %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_harvest_models <- survival_harvest_models %>% 
  mutate(model_1 = map(data, survivalHarvest_1))

# Stores Model_2
survival_harvest_models <- survival_harvest_models %>% 
  mutate(model_2 = map(data, survivalHarvest_2))

# Stores Model_3
survival_harvest_models <- survival_harvest_models %>% 
  mutate(model_3 = map(data, survivalHarvest_3))

# Stores ModelHarvest 
survival_harvest_models <- survival_harvest_models %>% 
  mutate(model_h = map(data, survivalModelHarvest))


# looking at the models within the tibble 
survival_harvest_models

survival_harvest_models$model_0
survival_harvest_models$model_1
survival_harvest_models$model_2
survival_harvest_models$model_3
survival_harvest_models$model_h


# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_harvest_models_dataframe_Bv1.rds")))

# Calling model RDS files  -----------------------------------------------------

survival_harvest_models_dataframe <- readRDS(file = here("Data/04_Temp", "survival_harvest_models_dataframe_BG.rds"))

survival_harvest_models_dataframe

# Testing models ---------------------------------------------------------------

# Expanding the lists 

SH_models <- subset(survival_harvest_models_dataframe, select = -c(data))

SH_models <- SH_models %>%
  unnest(c(model_1, model_2, model_3))

SH_models$climatic_var <- rep(ClimaticVarList, times = 6)

SH_models

# Testing models 

SH_models$lr_test_0_1 <- unlist(modelsTest_0_1(SH_models), recursive = FALSE)

SH_models$lr_test_1_2 <- unlist(modelsTest_1_2(SH_models), recursive = FALSE)

SH_models$lr_test_2_3 <- unlist(modelsTest_2_3(SH_models), recursive = FALSE)

SH_models$lr_test_0_trt <- unlist(modelsTest_0_h(SH_models), recursive = FALSE)

SH_models$lr_test_trt_2 <- unlist(modelsTest_h_2(SH_models), recursive = FALSE)

# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


SH_models
SH_models$lr_test_0_1
SH_models$lr_test_1_2
SH_models$lr_test_2_3
SH_models$lr_test_0_trt
SH_models$lr_test_trt_2


# Extracting P-Values ----------------------------------------------------------

SH_models_p_vals <- pValExtract(SH_models)

SH_models_p_vals

SH_p_vals <- subset(SH_models_p_vals, 
                   select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                              "p_val_2_3", "p_val_0_trt", "p_val_trt_2"))
SH_p_vals

# Isolating Significant P-Values 

SH_sig_p_vals <- removeNonSigPVals(SH_p_vals)

SH_sig_p_vals


write.csv(SH_p_vals, file = here("Data/Output", "SH_p_vals_BG.csv"), row.names = FALSE)

write.csv(SH_sig_p_vals, file = here("Data/Output", "SH_sig_p_vals_BG.csv"), row.names = FALSE)
