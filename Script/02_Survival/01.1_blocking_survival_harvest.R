# Content: Blocking Survival Harvest ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/01_survival_harvest_model_function.R")

source("Script/02a_Survival_Functions/01.1_survival_harvest_model_function_blocked.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

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

# Create a new tibble 
survival_harvest_blocking <- loc_group_harvest

survival_harvest_blocking

# need to remove narrows and twobit

survival_harvest_blocking <- survival_harvest_blocking[survival_harvest_blocking$location != "Narrows" & 
                                                       survival_harvest_blocking$location != "Twobit", ]

survival_harvest_blocking
# Unblocked Models  ---------------------------------------------------

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_1 = map(data, survivalHarvest_1))

# Stores Model_2
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_2 = map(data, survivalHarvest_2))

# Stores Model_3
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_3 = map(data, survivalHarvest_3))

# Stores ModelHarvest 
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_h = map(data, survivalModelHarvest))


# Blocked Models --------------------------------------------------------------

# Stores the null model as Model_0
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_0B = map(data, survivalModelNullB))

# Stores Model_1
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_1B = map(data, survivalHarvest_1B))

# Stores Model_2
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_2B = map(data, survivalHarvest_2B))

# Stores Model_3
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_3B = map(data, survivalHarvest_3B))

# Stores ModelHarvest 
survival_harvest_blocking <- survival_harvest_blocking %>% 
  mutate(model_hB = map(data, survivalModelHarvestB))



survival_harvest_blocking


# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_harvest_blocking, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_harvest_models_blocking.rds")))

# Calling model RDS files  -----------------------------------------------------

survival_harvest_blocking <- readRDS(file = here("Data/04_Temp", "2024-02-02_survival_harvest_models_blocking.rds"))

survival_harvest_blocking


# Testing models ---------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(survival_harvest_blocking$location)

SH_models_B <- subset(survival_harvest_blocking, select = -c(data, location))

SH_models_B <- SH_models_B %>%
  unnest(c(model_1, model_2, model_3, model_1B, model_2B, model_3B))

SH_models_B$climatic_var <- rep(ClimaticVarList, times = 4)

SH_models_B$location <- rep(loc_list, each = 15)

SH_models_B

# Testing models 

# Null vs 1 variable
SH_models_B$lr_test_0_0B <- unlist(modelsTest(df = SH_models_B,
                                           model_x = SH_models_B$model_0,
                                           model_y = SH_models_B$model_0B), 
                                recursive = FALSE)

SH_models_B$lr_test_1_1B <- unlist(modelsTest(df = SH_models_B,
                                           model_x = SH_models_B$model_1,
                                           model_y = SH_models_B$model_1B), 
                                recursive = FALSE)

SH_models_B$lr_test_2_2B <- unlist(modelsTest(df = SH_models_B,
                                           model_x = SH_models_B$model_2,
                                           model_y = SH_models_B$model_2B), 
                                recursive = FALSE)

SH_models_B$lr_test_3_3B <- unlist(modelsTest(df = SH_models_B,
                                           model_x = SH_models_B$model_3,
                                           model_y = SH_models_B$model_3B), 
                                recursive = FALSE)

SH_models_B$lr_test_h_hB<- unlist(modelsTest(df = SH_models_B,
                                           model_x = SH_models_B$model_h,
                                           model_y = SH_models_B$model_hB), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


SH_models_B


# Extracting P-Values ----------------------------------------------------------

SH_models_B_p_vals <- extractPVals(SH_models_B)

SH_models_B_p_vals

SH_B_p_vals <- subset(SH_models_B_p_vals, 
                    select = c("location", "climatic_var", "p_val_0_0B", "p_val_1_1B", 
                               "p_val_2_2B", "p_val_3_3B", "p_val_h_hB"))
SH_B_p_vals

# Isolating Significant P-Values 

SH_B_sig_p_vals <- removeNonSigPVals(SH_B_p_vals)

SH_B_sig_p_vals


write.csv(SH_B_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_pvals_Blocked.csv")), 
          row.names = FALSE)

write.csv(SH_B_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_sig_pvals_Blocked.csv")), 
          row.names = FALSE)


