# CONTENT: Survival - Cover ----------------------------------------------------
# VARIABLE RANDOM EFFEFTS -------------------------------------------------------

# Author: Thomson Harris
# Date: Feb 9th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/02.1_survival_cover_model_function_splitplot.R")
source("Script/02a_Survival_Functions/02.2_survival_cover_model_function_plot.R")
source("Script/02a_Survival_Functions/02.3_survival_cover_model_function_block.R")


source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)


# This function converts survival, coverF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Grouping Data by Random Effects ---------------------------------------------

loc_group_cover <- regen_survival %>% 
  group_by(location) %>% 
  nest()


###### 4.1 Plot dataframe ----
survival_cover_models_P <- loc_group_cover[loc_group_cover$location %in% 
                                               c("Alex Fraser", "John Prince", "Redfish", "Twobit"), ]

###### 4.2 Splitplot dataframe ----
survival_cover_models_S <- loc_group_cover[loc_group_cover$location %in% c("Jaffray", "Narrows"), ]



# 5. Storing Plot models  ---------------------------------------------------


###### 5.1 Model_0P ----
survival_cover_models_P <- survival_cover_models_P %>% 
  mutate(model_0 = map(data, survivalModelNullP))

###### 5.2 Model_1P ----
survival_cover_models_P <- survival_cover_models_P %>% 
  mutate(model_1 = map(data, survivalCover_1P))

###### 5.3 Model_2P ----
survival_cover_models_P <- survival_cover_models_P %>% 
  mutate(model_2 = map(data, survivalCover_2P))

###### 5.4 Model_3P ----
survival_cover_models_P <- survival_cover_models_P %>% 
  mutate(model_3 = map(data, survivalCover_3P))

###### 5.5 Model_cP ----
survival_cover_models_P <- survival_cover_models_P %>% 
  mutate(model_c = map(data, survivalModelCoverP))

# 6. Storing Splitplot models  ---------------------------------------------------


###### 6.1 Model_0S ----
survival_cover_models_S <- survival_cover_models_S %>% 
  mutate(model_0 = map(data, survivalModelNullS))

###### 6.2 Model_1S ----
survival_cover_models_S <- survival_cover_models_S %>% 
  mutate(model_1 = map(data, survivalCover_1S))

###### 6.3 Model_2S ----
survival_cover_models_S <- survival_cover_models_S %>% 
  mutate(model_2 = map(data, survivalCover_2S))

###### 6.4 Model_3S ----
survival_cover_models_S <- survival_cover_models_S %>% 
  mutate(model_3 = map(data, survivalCover_3S))

###### 6.5 Model_cS ----
survival_cover_models_S <- survival_cover_models_S %>% 
  mutate(model_c = map(data, survivalModelCoverS))



# 7. Merging Model Dataframes -----------------------------------------------

survival_cover_models <- rbind(survival_cover_models_P, survival_cover_models_S) 

survival_cover_models

# 8. Saving models as a .RDS file --------------------------------------------------

saveRDS(survival_cover_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_cover_models_dataframe_Ran_Effs.rds")))

# 9. Calling model .RDS files  -----------------------------------------------------

survival_cover_models <- readRDS(file = here("Data/04_Temp", "2024-02-09_survival_cover_models_dataframe_Ran_Effs.rds"))

survival_cover_models

# 10. Testing models ---------------------------------------------------------------

###### 10.1 Importing Functions ----
source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 10.2 Un-nesting Models ----

loc_list <- unique(survival_cover_models$location)

SC_models <- subset(survival_cover_models, select = -c(data, location))

SC_models <- SC_models %>%
  unnest(c(model_1, model_2, model_3))

SC_models$climatic_var <- rep(ClimaticVarList, times = 6)

SC_models$location <- rep(loc_list, each = 15)

SC_models

###### 10.3 Running Tests -----

# Null vs 1 variable
SC_models$lr_test_0_1 <- unlist(modelsTest(df = SC_models,
                                          model_x = SC_models$model_0,
                                          model_y = SC_models$model_1), 
                                  recursive = FALSE)

SC_models$lr_test_1_2 <- unlist(modelsTest(df = SC_models,
                                           model_x = SC_models$model_1,
                                           model_y = SC_models$model_2), 
                                recursive = FALSE)

SC_models$lr_test_2_3 <- unlist(modelsTest(df = SC_models,
                                           model_x = SC_models$model_2,
                                           model_y = SC_models$model_3), 
                                recursive = FALSE)

SC_models$lr_test_0_c <- unlist(modelsTest(df = SC_models,
                                           model_x = SC_models$model_0,
                                           model_y = SC_models$model_c), 
                                recursive = FALSE)

SC_models$lr_test_c_2 <- unlist(modelsTest(df = SC_models,
                                           model_x = SC_models$model_c,
                                           model_y = SC_models$model_2), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. cover of tree_cover)
# Look at the name  of the file to find the variable used. 


####### 10.4 Extracting P-Values ------

SC_models_p_vals <- extractPVals(SC_models)

SC_models_p_vals

SC_p_vals <- subset(SC_models_p_vals, 
                   select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                              "p_val_2_3", "p_val_0_c", "p_val_c_2"))
SC_p_vals

###### 10.5 Isolating Significant P-Values ------

SC_sig_p_vals <- removeNonSigPVals(SC_p_vals)

SC_sig_p_vals

# 11. Saving P-Values as .CSV ------------------------------------------------

write.csv(SC_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_pvals_Ran_Effs.csv")), 
          row.names = FALSE)

write.csv(SC_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_sig_pvals_Ran_Effs.csv")), 
          row.names = FALSE)

