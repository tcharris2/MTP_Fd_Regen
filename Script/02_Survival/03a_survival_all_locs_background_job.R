# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2.  Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/03_survival_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# 3. Fixing Variables ---------------------------------------------------------


regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
s_group_model_null_harvest <- list(groupSurvivalModelNull(regen_survival))
s_group_model_null_canopy <- list(groupSurvivalModelNull(regen_survival))

###### 4.2 Treatment Models ----
s_group_model_harvest <- list(groupSurvivalModelHarvest(regen_survival))
s_group_model_canopy <- list(groupSurvivalModelCanopy(regen_survival))
s_group_model_age_har <- list(groupSurvivalModelAge(regen_survival))
s_group_model_age_can <- list(groupSurvivalModelAge(regen_survival))

###### 4.3 Harvest Models ----
s_group_model_harvest_1 <- groupSurvivalHarvest_1(regen_survival)
s_group_model_harvest_1a <- groupSurvivalHarvest_1a(regen_survival)
s_group_model_harvest_2 <- groupSurvivalHarvest_2(regen_survival)
s_group_model_harvest_2a <- groupSurvivalHarvest_2a(regen_survival)
s_group_model_harvest_3 <- groupSurvivalHarvest_3(regen_survival)
s_group_model_harvest_3a <- groupSurvivalHarvest_3a(regen_survival)

###### 4.4 Canopy Models ----
s_group_model_canopy_1 <- groupSurvivalCanopy_1(regen_survival)
s_group_model_canopy_1a <- groupSurvivalCanopy_1a(regen_survival)
s_group_model_canopy_2 <- groupSurvivalCanopy_2(regen_survival)
s_group_model_canopy_2a <- groupSurvivalCanopy_2a(regen_survival)
s_group_model_canopy_3 <- groupSurvivalCanopy_3(regen_survival)
s_group_model_canopy_3a <- groupSurvivalCanopy_3a(regen_survival)


# 5. Grouping Models ------------------------------------------------------------
survival_group_harvest_models <- tibble(ClimaticVarList,
                                        s_group_model_null_harvest,
                                        s_group_model_harvest, s_group_model_age_har,
                                        s_group_model_harvest_1, s_group_model_harvest_1a,
                                        s_group_model_harvest_2, s_group_model_harvest_2a,
                                        s_group_model_harvest_3, s_group_model_harvest_3a)

survival_group_harvest_models


survival_group_canopy_models <- tibble(ClimaticVarList,
                                       s_group_model_null_canopy,
                                       s_group_model_canopy,s_group_model_age_can,
                                       s_group_model_canopy_1, s_group_model_canopy_1a,
                                       s_group_model_canopy_2, s_group_model_canopy_2a,
                                       s_group_model_canopy_3, s_group_model_canopy_3a)

survival_group_canopy_models


# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(survival_group_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_harvest_models_Bv1.rds")))

# Canopy models
saveRDS(survival_group_canopy_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_canopy_models_Bv1.rds")))
