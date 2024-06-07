# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), 
                  header = TRUE)
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/03_survival_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Correcting Variable types ---------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

# This function converts survival, harvestF, provenanceF, and 
# all the random effects into factors. 
# It also normalizes all the climatic distance variables. 

regen_survival <- regen_prepped
str(regen_prepped)

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
sur_model_null <- list(groupSurvivalModelNull(regen_survival))

###### 4.2 Treatment Models ----
sur_model_harvest <- list(groupSurvivalModelHarvest(regen_survival))
sur_model_cover <- list(groupSurvivalModelCover(regen_survival))
sur_model_age <- list(groupSurvivalModelAge(regen_survival))

###### 4.3 Climate Models -----
sur_model_1 <- list(groupSurvivalClimate_1(regen_survival))
sur_model_1a <- list(groupSurvivalClimate_1(regen_survival))

###### 4.4 Harvest Models ----
sur_model_harvest_2 <- groupSurvivalHarvest_2(regen_survival)
sur_model_harvest_2a <- groupSurvivalHarvest_2a(regen_survival)
sur_model_harvest_3 <- groupSurvivalHarvest_3(regen_survival)
sur_model_harvest_3a <- groupSurvivalHarvest_3a(regen_survival)

###### 4.5 Cover Models ----
sur_model_cover_2 <- groupSurvivalCover_2(regen_survival)
sur_model_cover_2a <- groupSurvivalCover_2a(regen_survival)
sur_model_cover_3 <- groupSurvivalCover_3(regen_survival)
sur_model_cover_3a <- groupSurvivalCover_3a(regen_survival)



# 5. Grouping Models ------------------------------------------------------------
survival_harvest_models <- tibble("model_0" = sur_model_null,
                                  "model_h" = sur_model_harvest,
                                  "model_a" = sur_model_age,
                                  "model_1" = sur_model_1,
                                  "model_1a" = sur_model_1a,
                                  "model_2" = sur_model_harvest_2, 
                                  "model_2a" = sur_model_harvest_2a,
                                  "model_3" = sur_model_harvest_3, 
                                  "model_3a" = sur_model_harvest_3a,
                                  ClimaticVarList)

survival_harvest_models


survival_cover_models <- tibble("model_0" = sur_model_null,
                                "model_c" = sur_model_cover, 
                                "model_a" = sur_model_age,
                                "model_1" = sur_model_1, 
                                "model_1a" = sur_model_1a,
                                "model_2" = sur_model_cover_2,
                                "model_2a" = sur_model_cover_2a,
                                "model_3" = sur_model_cover_3,
                                "model_3a" = sur_model_cover_3a,
                                ClimaticVarList)

survival_cover_models


# 6. Saving models as a RDS file --------------------------------------------------

# Harvest models
saveRDS(survival_harvest_models, 
        file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_harvest_models.rds")))

# Cover models
saveRDS(survival_cover_models, 
        file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_cover_models.rds")))
