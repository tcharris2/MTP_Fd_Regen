# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

# 2.  Importing Functions ----------------------------------------------------------

source("Script/01a_Survival_Functions/survival_all_locs_model_function.R")

source("Script/01a_Survival_Functions/survival_data_prep_function.R")

# 3. Fixing Variables ---------------------------------------------------------

ClimaticVarList <- climaticVarListFunction(regen)

regen <- dataPrepFunction(regen)

str(regen)

regen_canopy <- subset(regen, !(is.na(tree_cover)))



# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
s_group_model_null_harvest <- list(groupSurvivalModelNull(regen))
s_group_model_null_canopy <- list(groupSurvivalModelNull(regen_canopy))

###### 4.2 Treatment Models ----
s_group_model_harvest <- list(groupSurvivalModelHarvest(regen))
s_group_model_canopy <- list(groupSurvivalModelCanopy(regen_canopy))
s_group_model_age_har <- list(groupSurvivalModelAge(regen))
s_group_model_age_can <- list(groupSurvivalModelAge(regen_canopy))

###### 4.3 Harvest Models ----
s_group_model_harvest_1 <- groupSurvivalHarvest_1(regen)
s_group_model_harvest_1a <- groupSurvivalHarvest_1a(regen)
s_group_model_harvest_2 <- groupSurvivalHarvest_2(regen)
s_group_model_harvest_2a <- groupSurvivalHarvest_2a(regen)
s_group_model_harvest_3 <- groupSurvivalHarvest_3(regen)
s_group_model_harvest_3a <- groupSurvivalHarvest_3a(regen)

###### 4.4 Canopy Models ----
s_group_model_canopy_1 <- groupSurvivalCanopy_1(regen_canopy)
s_group_model_canopy_1a <- groupSurvivalCanopy_1a(regen_canopy)
s_group_model_canopy_2 <- groupSurvivalCanopy_2(regen_canopy)
s_group_model_canopy_2a <- groupSurvivalCanopy_2a(regen_canopy)
s_group_model_canopy_3 <- groupSurvivalCanopy_3(regen_canopy)
s_group_model_canopy_3a <- groupSurvivalCanopy_3a(regen_canopy)


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

# Adding Climatic Variables
survival_group_harvest_models$climatic_var <- ClimaticVarList

survival_group_canopy_models$climatic_var <- ClimaticVarList


# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(survival_group_harvest_models, file = here("Data/04_Temp", "survival_group_harvest_models.rds"))

# Canopy models
saveRDS(survival_group_canopy_models, file = here("Data/04_Temp", "survival_group_canopy_models.rds"))