# CONTENT: Survival - Harvest ----------------------------------------------------
# VARIABLE RANDOM EFFEFTS -------------------------------------------------------

# Author: Thomson Harris
# Date: Feb 9th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/01.1_survival_harvest_model_function_splitplot.R")
source("Script/02a_Survival_Functions/01.2_survival_harvest_model_function_plot.R")
source("Script/02a_Survival_Functions/01.3_survival_harvest_model_function_block.R")


source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)


# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Grouping Data by Random Effects ---------------------------------------------

loc_group_harvest <- regen_survival %>% 
  group_by(location) %>% 
  nest()

###### 4.1 Block dataframe ----
survival_harvest_models_B <- loc_group_harvest[loc_group_harvest$location == "Redfish", ]

###### 4.2 Plot dataframe ----
survival_harvest_models_P <- loc_group_harvest[loc_group_harvest$location %in% c("Alex Fraser", "John Prince", "Twobit"), ]

###### 4.3 Splitplot dataframe ----
survival_harvest_models_S <- loc_group_harvest[loc_group_harvest$location %in% c("Jaffray", "Narrows"), ]


# 5. Storing Block models  ---------------------------------------------------

###### 5.1 Model_0B ----
survival_harvest_models_B <- survival_harvest_models_B %>% 
  mutate(model_0 = map(data, survivalModelNullB))

###### 5.2 Model_1B ----
survival_harvest_models_B <- survival_harvest_models_B %>% 
  mutate(model_1 = map(data, survivalHarvest_1B))

###### 5.3 Model_2B ----
survival_harvest_models_B <- survival_harvest_models_B %>% 
  mutate(model_2 = map(data, survivalHarvest_2B))

###### 5.4 Model_3B ----
survival_harvest_models_B <- survival_harvest_models_B %>% 
  mutate(model_3 = map(data, survivalHarvest_3B))

###### 5.5 Model_hB ----
survival_harvest_models_B <- survival_harvest_models_B %>% 
  mutate(model_h = map(data, survivalModelHarvestB))


# 6. Storing Plot models  ---------------------------------------------------


###### 6.1 Model_0P ----
survival_harvest_models_P <- survival_harvest_models_P %>% 
  mutate(model_0 = map(data, survivalModelNullP))

###### 6.2 Model_1P ----
survival_harvest_models_P <- survival_harvest_models_P %>% 
  mutate(model_1 = map(data, survivalHarvest_1P))

###### 6.3 Model_2P ----
survival_harvest_models_P <- survival_harvest_models_P %>% 
  mutate(model_2 = map(data, survivalHarvest_2P))

###### 6.4 Model_3P ----
survival_harvest_models_P <- survival_harvest_models_P %>% 
  mutate(model_3 = map(data, survivalHarvest_3P))

###### 6.5 Model_hP ----
survival_harvest_models_P <- survival_harvest_models_P %>% 
  mutate(model_h = map(data, survivalModelHarvestP))

# 7. Storing Splitplot models  ---------------------------------------------------


###### 7.1 Model_0S ----
survival_harvest_models_S <- survival_harvest_models_S %>% 
  mutate(model_0 = map(data, survivalModelNullS))

###### 7.2 Model_1S ----
survival_harvest_models_S <- survival_harvest_models_S %>% 
  mutate(model_1 = map(data, survivalHarvest_1S))

###### 7.3 Model_2S ----
survival_harvest_models_S <- survival_harvest_models_S %>% 
  mutate(model_2 = map(data, survivalHarvest_2S))

###### 7.4 Model_3S ----
survival_harvest_models_S <- survival_harvest_models_S %>% 
  mutate(model_3 = map(data, survivalHarvest_3S))

###### 7.5 Model_hS ----
survival_harvest_models_S <- survival_harvest_models_S %>% 
  mutate(model_h = map(data, survivalModelHarvestS))



# 8. Merging Model Dataframes -----------------------------------------------

survival_harvest_models <- rbind(survival_harvest_models_B, survival_harvest_models_P, survival_harvest_models_S) 


survival_harvest_models



# 9. Saving models as a .RDS file --------------------------------------------------

saveRDS(survival_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_harvest_models_dataframe_Ran_Effs.rds")))

# 10. Calling model .RDS files  -----------------------------------------------------

survival_harvest_models <- readRDS(file = here("Data/04_Temp", "2024-02-09_survival_harvest_models_dataframe_Ran_Effs.rds"))

survival_harvest_models

# 11. Testing models ---------------------------------------------------------------

###### 11.1 Importing Functions ----
source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 11.2 Un-nesting Models ----

loc_list <- unique(survival_harvest_models$location)

SH_models <- subset(survival_harvest_models, select = -c(data, location))

SH_models <- SH_models %>%
  unnest(c(model_1, model_2, model_3))

SH_models$climatic_var <- rep(ClimaticVarList, times = 6)

SH_models$location <- rep(loc_list, each = 15)

SH_models

###### 11.3 Running Tests -----

# Null vs 1 variable
SH_models$lr_test_0_1 <- unlist(modelsTest(df = SH_models,
                                          model_x = SH_models$model_0,
                                          model_y = SH_models$model_1), 
                                  recursive = FALSE)

SH_models$lr_test_1_2 <- unlist(modelsTest(df = SH_models,
                                           model_x = SH_models$model_1,
                                           model_y = SH_models$model_2), 
                                recursive = FALSE)

SH_models$lr_test_2_3 <- unlist(modelsTest(df = SH_models,
                                           model_x = SH_models$model_2,
                                           model_y = SH_models$model_3), 
                                recursive = FALSE)

SH_models$lr_test_0_h <- unlist(modelsTest(df = SH_models,
                                           model_x = SH_models$model_0,
                                           model_y = SH_models$model_h), 
                                recursive = FALSE)

SH_models$lr_test_h_2 <- unlist(modelsTest(df = SH_models,
                                           model_x = SH_models$model_h,
                                           model_y = SH_models$model_2), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


SH_models
SH_models$lr_test_0_1
SH_models$lr_test_1_2
SH_models$lr_test_2_3
SH_models$lr_test_0_h
SH_models$lr_test_h_2


####### 11.4 Extracting P-Values ------

SH_models_p_vals <- extractPVals(SH_models)

SH_models_p_vals

SH_p_vals <- subset(SH_models_p_vals, 
                   select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                              "p_val_2_3", "p_val_0_h", "p_val_h_2"))
SH_p_vals

###### 11.5 Isolating Significant P-Values ------

SH_sig_p_vals <- removeNonSigPVals(SH_p_vals)

SH_sig_p_vals

# 12. Saving P-Values as .CSV ------------------------------------------------

write.csv(SH_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_pvals_Ran_Effs.csv")), 
          row.names = FALSE)

write.csv(SH_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_sig_pvals_Ran_Effs.csv")), 
          row.names = FALSE)

