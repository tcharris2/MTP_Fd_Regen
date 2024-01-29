# Content: Survival ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "survival_fd_b.csv"), header = TRUE)

# Importing Functions ----------------------------------------------------------

source("Script/01a_Survival_Functions/canopy_model_function.R")

source("Script/01a_Survival_Functions/data_prep_function.R")

source("Script/01a_Survival_Functions/canopy_lrtest_function.R")


# Correcting Variable types ----------------------------------------------------

regen <- dataPrepFunction()

str(regen)

regen_canopy <- subset(regen, !(is.na(tree_cover)))
regen_canopy <- regen %>% drop_na(tree_cover)

# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# Grouping Data ----------------------------------------------------------------

loc_group_canopy <- regen_canopy %>% 
  group_by(location) %>% 
  nest()

# Storing models in a tibble ---------------------------------------------------

# Create a new tibble 
survival_canopy_models <- loc_group_canopy

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_canopy_models <- survival_canopy_models %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_canopy_models <- survival_canopy_models %>% 
  mutate(model_1 = map(data, survivalCanopy_1))

# Stores Model_2
survival_canopy_models <- survival_canopy_models %>% 
  mutate(model_2 = map(data, survivalCanopy_2))

# Stores Model_3
survival_canopy_models <- survival_canopy_models %>% 
  mutate(model_3 = map(data, survivalCanopy_3))

# Stores ModelCanopy
survival_canopy_models <- survival_canopy_models %>% 
  mutate(model_c = map(data, survivalModelCanopy))


# looking at the models within the tibble 
survival_canopy_models

survival_canopy_models$model_0
survival_canopy_models$model_1
survival_canopy_models$model_2
survival_canopy_models$model_3
survival_canopy_models$model_c


# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_canopy_models, file = here("Data/04_Temp", "survival_canopy_models_dataframe.rds"))

# Calling RDS files ------------------------------------------------------------

survival_canopy_models_dataframe <- readRDS(file = here("Data/04_Temp", "survival_canopy_models_dataframe.rds"))

survival_canopy_models_dataframe

# Testing models ---------------------------------------------------------------

# Expanding the lists 

SC_models <- subset(survival_canopy_models_dataframe, select = -c(data))

SC_models <- SC_models %>%
  unnest(c(model_1, model_2, model_3))

SC_models$climatic_var <- rep(ClimaticVarList, times = 6)

SC_models

# Testing models 

SC_models$lr_test_0_1 <- unlist(modelsTest_0_1(SC_models), recursive = FALSE)

SC_models$lr_test_1_2 <- unlist(modelsTest_1_2(SC_models), recursive = FALSE)

SC_models$lr_test_2_3 <- unlist(modelsTest_2_3(SC_models), recursive = FALSE)

SC_models$lr_test_0_trt <- unlist(modelsTest_0_c(SC_models), recursive = FALSE)

SC_models$lr_test_trt_2 <- unlist(modelsTest_c_2(SC_models), recursive = FALSE)


SC_models
SC_models$lr_test_0_1
SC_models$lr_test_1_2
SC_models$lr_test_2_3
SC_models$lr_test_0_trt
SC_models$lr_test_trt_2


# Extracting P-Values ----------------------------------------------------------

SC_models_p_vals <- pValExtract(SC_models)

SC_models_p_vals

SC_p_vals <- subset(SC_models_p_vals, 
                   select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                              "p_val_2_3", "p_val_0_trt", "p_val_trt_2"))
SC_p_vals

# Isolating Significant P-Values 

SC_sig_p_vals <- removeNonSigPVals(SC_p_vals)

SC_sig_p_vals

write.csv(SC_p_vals, file = here("Data/Output", "SC_p_vals.csv"), row.names = FALSE)

write.csv(SC_sig_p_vals, file = here("Data/Output", "SC_sig_p_vals.csv"), row.names = FALSE)
