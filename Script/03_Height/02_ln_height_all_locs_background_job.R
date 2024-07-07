# 1. Importing Data ---------------------------------------------------------------
### 1.1. Loading Packages ----
library(here)
library(tidyverse)
library(lmer)

### 1.2. Loading Data -----

regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/03a_ln_height_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Preparing Data ----------------------------------------------------

###### 3.1 Universal prep ----
regen_prepped <- universalDataPrepFunction(regen)

# This function converts survival, harvestF, provenanceF, and 
# all the random effects into factors. 
# It also normalizes all the climatic distance variables. 

###### 3.2 Height specific prep ----

# Remove outliers
regen_height <- subset(regen_prepped, !regen_prepped$tree_number %in% 
                         c(3904, 9861, 8248, 12846, 13432, 14752))

# Remove NAs
regen_height <-  subset(regen_height, !(is.na(height)))


str(regen_height)

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
ln_h_model_null <- list(ln_HeightModelNull(regen_height))

###### 4.2 Treatment Models ----
ln_h_model_harvest <- list(ln_HeightModelHarvest(regen_height))
ln_h_model_cover <- list(ln_HeightModelCover(regen_height))
ln_h_model_age <- list(ln_HeightModelAge(regen_height))
ln_h_model_age_har <- list(ln_HeightModelAgeHarvest(regen_height))
ln_h_model_age_can <- list(ln_HeightModelAgeCover(regen_height))

###### 4.3 Climate Models ----
ln_h_model_1 <- ln_HeightClimate_1(regen_height)
ln_h_model_1a <- ln_HeightClimate_1a(regen_height)

###### 4.4 Harvest Models ----
ln_h_model_harvest_2 <- ln_HeightHarvest_2(regen_height)
ln_h_model_harvest_3 <- ln_HeightHarvest_3(regen_height)
ln_h_model_harvest_2a <- ln_HeightHarvest_2a(regen_height)
ln_h_model_harvest_3a <- ln_HeightHarvest_3a(regen_height)

###### 4.5 Cover Models ----
ln_h_model_cover_2 <- ln_HeightCover_2(regen_height)
ln_h_model_cover_3 <- ln_HeightCover_3(regen_height)
ln_h_model_cover_2a <- ln_HeightCover_2a(regen_height)
ln_h_model_cover_3a <- ln_HeightCover_3a(regen_height)



# 5. Grouping Models -----------------------------------------------------------
ln_height_harvest_models <- tibble("model_0" = ln_h_model_null,
                                   "model_h" = ln_h_model_harvest,
                                   "model_a" = ln_h_model_age,
                                   "model_ah" = ln_h_model_age_har,
                                   "model_1" = ln_h_model_1,
                                   "model_1a" = ln_h_model_1a,
                                   "model_2" = ln_h_model_harvest_2, 
                                   "model_2a" = ln_h_model_harvest_2a,
                                   "model_3" = ln_h_model_harvest_3, 
                                   "model_3a" = ln_h_model_harvest_3a,
                                   ClimaticVarList)

ln_height_harvest_models


ln_height_cover_models <- tibble("model_0" = ln_h_model_null,
                                 "model_c" = ln_h_model_cover, 
                                 "model_a" = ln_h_model_age,
                                 "model_ac" = ln_h_model_age_can,
                                 "model_1" = ln_h_model_1, 
                                 "model_1a" = ln_h_model_1a,
                                 "model_2" = ln_h_model_cover_2,
                                 "model_2a" = ln_h_model_cover_2a,
                                 "model_3" = ln_h_model_cover_3,
                                 "model_3a" = ln_h_model_cover_3a,
                                 ClimaticVarList)

ln_height_cover_models

# 6. Saving models as a RDS file --------------------------------------------------

# Harvest models
saveRDS(ln_height_harvest_models, file = here("Data/04_Temp",
                                              paste0(Sys.Date(), 
                                                     "_ln_height_harvest_models.rds" )))

# cover models
saveRDS(ln_height_cover_models, file = here("Data/04_Temp", 
                                            paste0(Sys.Date(), 
                                                   "_ln_height_cover_models.rds" )))