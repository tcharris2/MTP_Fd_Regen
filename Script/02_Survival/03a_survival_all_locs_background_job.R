# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2.  Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/03.1_survival_all_locs_model_function_sqrt.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# 3. Fixing Variables ---------------------------------------------------------


regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

regen_survival <- subset(regen_survival, !regen_survival$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                                     "Peterhope future Fd", "Alex Fraser future Fd", 
                                                                     "Twobit B class Fd"))

regen_survival$sqrt_tree_cover <- sqrt(regen_survival$tree_cover)

str(regen_survival)

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
s_group_model_null_harvest <- list(groupSurvivalModelNull(regen_survival))
s_group_model_null_cover <- list(groupSurvivalModelNull(regen_survival))

###### 4.2 Treatment Models ----
s_group_model_harvest <- list(groupSurvivalModelHarvest(regen_survival))
s_group_model_cover <- list(groupSurvivalModelCover(regen_survival))
s_group_model_age_har <- list(groupSurvivalModelAge(regen_survival))
s_group_model_age_can <- list(groupSurvivalModelAge(regen_survival))

###### 4.3 Harvest Models ----
s_group_model_harvest_1 <- groupSurvivalHarvest_1(regen_survival)
s_group_model_harvest_1a <- groupSurvivalHarvest_1a(regen_survival)
s_group_model_harvest_2 <- groupSurvivalHarvest_2(regen_survival)
s_group_model_harvest_2a <- groupSurvivalHarvest_2a(regen_survival)
s_group_model_harvest_3 <- groupSurvivalHarvest_3(regen_survival)
s_group_model_harvest_3a <- groupSurvivalHarvest_3a(regen_survival)

###### 4.4 cover Models ----
s_group_model_cover_1 <- groupSurvivalCover_1(regen_survival)
s_group_model_cover_1a <- groupSurvivalCover_1a(regen_survival)
s_group_model_cover_2 <- groupSurvivalCover_2(regen_survival)
s_group_model_cover_2a <- groupSurvivalCover_2a(regen_survival)
s_group_model_cover_3 <- groupSurvivalCover_3(regen_survival)
s_group_model_cover_3a <- groupSurvivalCover_3a(regen_survival)


# 5. Grouping Models ------------------------------------------------------------
survival_group_harvest_models <- tibble(ClimaticVarList,
                                        s_group_model_null_harvest,
                                        s_group_model_harvest, s_group_model_age_har,
                                        s_group_model_harvest_1, s_group_model_harvest_1a,
                                        s_group_model_harvest_2, s_group_model_harvest_2a,
                                        s_group_model_harvest_3, s_group_model_harvest_3a)

survival_group_harvest_models


survival_group_cover_models <- tibble(ClimaticVarList,
                                       s_group_model_null_cover,
                                       s_group_model_cover,s_group_model_age_can,
                                       s_group_model_cover_1, s_group_model_cover_1a,
                                       s_group_model_cover_2, s_group_model_cover_2a,
                                       s_group_model_cover_3, s_group_model_cover_3a)

survival_group_cover_models


# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(survival_group_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_harvest_models_NoFutures_sqrt.rds")))

# cover models
saveRDS(survival_group_cover_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_group_cover_models_NoFutures_sqrt.rds")))
