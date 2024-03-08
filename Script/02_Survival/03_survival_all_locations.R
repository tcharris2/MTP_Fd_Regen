# SURVIVAL ALL LOCATIONS --------------------------------------------------------

#' @Content: Survival - All Locations 

  #' @Author: Thomson Harris
  #' @Date: Oct 4th, 2023

# *Steps  1-6  run as a background job* -----------------------------------------

#' Run @survival_all_locs_background_job.R
  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/01a_Survival_Functions/survival_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------



regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

regen_survival <- subset(regen_survival, !regen_survival$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                                           "Peterhope future Fd", "Alex Fraser future Fd", 
                                                                           "Twobit B class Fd"))

str(regen_survival)
# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
s_group_model_null_harvest <- list(groupSurvivalModelNull(regen_survival))
s_group_model_null_cover <- list(groupSurvivalModelNull(regen_survival))

###### 4.2 Treatment Models ----
s_group_model_harvest <- list(groupSurvivalModelHarvest(regen_survival))
s_group_model_cover <- list(groupSurvivalModelcover(regen_survival))
s_group_model_age_har <- list(groupSurvivalModelAge(regen_survival))
s_group_model_age_can <- list(groupSurvivalModelAge(regen_survival))

###### 4.3 Harvest Models ----
s_group_model_harvest_1 <- groupSurvivalHarvest_1(regen_survival)
s_group_model_harvest_1a <- groupSurvivalHarvest_1a(regen_survival)
s_group_model_harvest_2 <- groupSurvivalHarvest_2(regen_survival)
s_group_model_harvest_2a <- groupSurvivalHarvest_2a(regen_survival)
s_group_model_harvest_3 <- groupSurvivalHarvest_3(regen_survival)
s_group_model_harvest_3a <- groupSurvivalHarvest_3a(regen_survival)

###### 4.4 Cover Models ----
s_group_model_cover_1 <- groupSurvivalCover_1(regen_survival)
s_group_model_cover_1a <- groupSurvivalCover_1a(regen_survival)
s_group_model_cover_2 <- groupSurvivalCover_2(regen_survival)
s_group_model_cover_2a <- groupSurvivalCover_2a(regen_survival)
s_group_model_cover_3 <- groupSurvivalCover_3(regen_survival)
s_group_model_cover_3a <- groupSurvivalCover_3a(regen_survival)



harvest_1 <- groupSurvivalHarvest_1(regen_survival)
harvest_1_sqrd <- groupSurvivalHarvest_1_sqrd(regen_survival)

sqrd_1_df <- tibble(harvest_1, harvest_1_sqrd, ClimaticVarList)



# 5. Grouping Models ------------------------------------------------------------
survival_group_harvest_models <- tibble(s_group_model_null_harvest,
                                        s_group_model_harvest, s_group_model_age_har,
                                        s_group_model_harvest_1, s_group_model_harvest_1a,
                                        s_group_model_harvest_2, s_group_model_harvest_2a,
                                        s_group_model_harvest_3, s_group_model_harvest_3a)

survival_group_harvest_models


survival_group_cover_models <- tibble(s_group_model_null_cover,
                                       s_group_model_cover,s_group_model_age_can,
                                       s_group_model_cover_1, s_group_model_cover_1a,
                                       s_group_model_cover_2, s_group_model_cover_2a,
                                       s_group_model_cover_3, s_group_model_cover_3a)

survival_group_cover_models

# Adding Climatic Variables
survival_group_harvest_models$climatic_var <- ClimaticVarList

survival_group_cover_models$climatic_var <- ClimaticVarList


# 6. Saving models as a RDS file --------------------------------------------------

# Harvest models
# saveRDS(survival_group_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_harvest_models_Bv1.rds")))

# cover models
# saveRDS(survival_group_cover_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_cover_models_Bv1.rds")))

# 7.Calling RDS File  ------------------------------------------------------------

survival_group_harvest_mods <- readRDS(file = here("Data/04_Temp", "2024-02-20_survival_group_harvest_models_NoFutures_sqrt.rds"))

survival_group_harvest_mods

survival_group_cover_mods <- readRDS(file = here("Data/04_Temp", "2024-02-20_survival_group_cover_models_NoFutures_sqrt.rds"))

survival_group_cover_mods



# 8. Testing Harvest Models  ------------------------------------------------------

# rename columns
colnames(survival_group_harvest_mods) <- c("ClimaticVarList", "model_0", "model_h", "model_a", 
                                                "model_1", "model_1a", "model_2", "model_2a", 
                                                "model_3", "model_3a")
survival_group_harvest_mods

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 8.1 Test models ----

# Null vs 1 variable
survival_group_harvest_mods$lr_test_0_h <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_0,
                                                             model_y = survival_group_harvest_mods$model_h), 
                                                       recursive = FALSE)

survival_group_harvest_mods$lr_test_0_a <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_0,
                                                             model_y = survival_group_harvest_mods$model_a), 
                                                  recursive = FALSE)

survival_group_harvest_mods$lr_test_0_1 <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_0,
                                                             model_y = survival_group_harvest_mods$model_1), 
                                                  recursive = FALSE)

# Model 1 vs n + 1
survival_group_harvest_mods$lr_test_1_1a <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_1,
                                                             model_y = survival_group_harvest_mods$model_1a), 
                                                  recursive = FALSE)

survival_group_harvest_mods$lr_test_1_2 <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                              model_x = survival_group_harvest_mods$model_1,
                                                              model_y = survival_group_harvest_mods$model_2), 
                                                   recursive = FALSE)

# Harvest model vs Havest + Climatic
survival_group_harvest_mods$lr_test_h_2 <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_h,
                                                             model_y = survival_group_harvest_mods$model_2), 
                                                  recursive = FALSE)

# Model 2 vs n + 1
survival_group_harvest_mods$lr_test_2_2a <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_2,
                                                             model_y = survival_group_harvest_mods$model_2a), 
                                                  recursive = FALSE)

survival_group_harvest_mods$lr_test_2_3 <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                              model_x = survival_group_harvest_mods$model_2,
                                                              model_y = survival_group_harvest_mods$model_3), 
                                                   recursive = FALSE)
# Model 3 vs n + 1
survival_group_harvest_mods$lr_test_3_3a <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_3,
                                                             model_y = survival_group_harvest_mods$model_3a), 
                                                  recursive = FALSE)

survival_group_harvest_mods$lr_test_1_3 <- unlist(modelsTest(df = survival_group_harvest_mods,
                                                             model_x = survival_group_harvest_mods$model_1,
                                                             model_y = survival_group_harvest_mods$model_3), 
                                                  recursive = FALSE)

survival_group_harvest_mods


###### 8.2 Extracting p-values ----
SH_group_p_vals <- extractPVals(survival_group_harvest_mods)

SH_group_p_vals

SH_group_p_vals <- subset(SH_group_p_vals, 
                    select = c("ClimaticVarList", "p_val_0_h", "p_val_0_a", 
                               "p_val_0_1", "p_val_1_1a", "p_val_1_2", "p_val_h_2",
                               "p_val_2_2a", "p_val_2_3", "p_val_3_3a", "p_val_1_3"))
SH_group_p_vals

# Isolating Significant P-Values 
SH_group_sig_p_vals <- removeNonSigPVals(SH_group_p_vals)

SH_group_sig_p_vals

###### 8.3 Saving p-values ----
write.csv(SH_group_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_group_p_vals_NoFutures_sqrt.csv")),
          row.names = FALSE)

write.csv(SH_group_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_group_sig_p_vals_NoFutures_sqrt.csv")),
          row.names = FALSE)


# 9. Testing cover Models  -------------------------------------------------------

# rename columns
colnames(survival_group_cover_mods) <- c("ClimaticVarList", "model_0", "model_c", "model_a", 
                                           "model_1", "model_1a", "model_2", "model_2a", 
                                           "model_3", "model_3a")
survival_group_cover_mods

###### 9.1 Test models ----
# Null vs 1 variable
survival_group_cover_mods$lr_test_0_c <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_0,
                                                             model_y = survival_group_cover_mods$model_c), 
                                                  recursive = FALSE)

survival_group_cover_mods$lr_test_0_a <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_0,
                                                             model_y = survival_group_cover_mods$model_a), 
                                                  recursive = FALSE)

survival_group_cover_mods$lr_test_0_1 <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_0,
                                                             model_y = survival_group_cover_mods$model_1), 
                                                  recursive = FALSE)

# Model 1 vs n + 1
survival_group_cover_mods$lr_test_1_1a <- unlist(modelsTest(df = survival_group_cover_mods,
                                                              model_x = survival_group_cover_mods$model_1,
                                                              model_y = survival_group_cover_mods$model_1a), 
                                                   recursive = FALSE)

survival_group_cover_mods$lr_test_1_2 <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_1,
                                                             model_y = survival_group_cover_mods$model_2), 
                                                  recursive = FALSE)

# cover model vs Havest + Climatic
survival_group_cover_mods$lr_test_c_2 <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_c,
                                                             model_y = survival_group_cover_mods$model_2), 
                                                  recursive = FALSE)

# Model 2 vs n + 1
survival_group_cover_mods$lr_test_2_2a <- unlist(modelsTest(df = survival_group_cover_mods,
                                                              model_x = survival_group_cover_mods$model_2,
                                                              model_y = survival_group_cover_mods$model_2a), 
                                                   recursive = FALSE)

survival_group_cover_mods$lr_test_2_3 <- unlist(modelsTest(df = survival_group_cover_mods,
                                                             model_x = survival_group_cover_mods$model_2,
                                                             model_y = survival_group_cover_mods$model_3), 
                                                  recursive = FALSE)
# Model 3 vs n + 1
survival_group_cover_mods$lr_test_3_3a <- unlist(modelsTest(df = survival_group_cover_mods,
                                                              model_x = survival_group_cover_mods$model_3,
                                                              model_y = survival_group_cover_mods$model_3a), 
                                                   recursive = FALSE)

survival_group_cover_mods



###### 9.2 Extracting p-values ----
SC_group_p_vals <- extractPVals(survival_group_cover_mods)

SC_group_p_vals

SC_group_p_vals <- subset(SC_group_p_vals, 
                          select = c("ClimaticVarList", "p_val_0_c", "p_val_0_a", 
                                     "p_val_0_1", "p_val_1_1a", "p_val_1_2", "p_val_c_2",
                                     "p_val_2_2a", "p_val_2_3", "p_val_3_3a"))
SC_group_p_vals

# Isolating Significant P-Values 
SC_group_sig_p_vals <- removeNonSigPVals(SC_group_p_vals)

SC_group_sig_p_vals

###### 9.3 Saving p-values ----
write.csv(SC_group_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_group_p_vals_NoFutures_sqrt.csv")),
          row.names = FALSE)

write.csv(SC_group_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_group_sig_p_vals_NoFutures_sqrt.csv")),
          row.names = FALSE)


# 10. Testing Squared Models ----------------------------------------------


# rename columns
colnames(sqrd_1_df) <- c("model_1", "model_1s", "ClimaticVarList")
sqrd_1_df

###### 10.1 Test models ----
# Null vs 1 variable
sqrd_1_df$lr_test_1_1s <- unlist(modelsTest(df = sqrd_1_df,
                                            model_x = sqrd_1_df$model_1,
                                            model_y = sqrd_1_df$model_1s), 
                                            recursive = FALSE)


###### 10.2 Extracting p-values ----
sqrd_1_pval <- extractPVals(sqrd_1_df)

sqrd_1_pval

sqrd_1_pval <- subset(sqrd_1_pval, 
                          select = c("ClimaticVarList", "p_val_1_1s"))
sqrd_1_pval

# Isolating Significant P-Values 
sqrd_1_pval <- removeNonSigPVals(sqrd_1_pval)

sqrd_1_pval

###### 10.3 Saving p-values ----
write.csv(sqrd_1_pval, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_group_squared_terms.csv")),
          row.names = FALSE)



