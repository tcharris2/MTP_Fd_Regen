# 1. Importing Data ---------------------------------------------------------------
### 1.1. Loading Packages ----
library(here)
library(tidyverse)
library(lme4)

### 1.2. Loading Data -----
regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Height_Functions/height_all_locs_model_function.R")

source("Script/02a_Height_Functions/height_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_harvest_height <- heightDataPrepFunction(regen)

str(regen_harvest_height)

regen_cover_height <- subset(regen_harvest_height, !(is.na(tree_cover)))

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
h_group_model_null_harvest <- list(groupHeightModelNull(regen_harvest_height))
h_group_model_null_cover <- list(groupHeightModelNull(regen_cover_height))

###### 4.2 Treatment Models ----
h_group_model_harvest <- list(groupHeightModelHarvest(regen_harvest_height))
h_group_model_cover <- list(groupHeightModelCover(regen_cover_height))
h_group_model_age_har <- list(groupHeightModelAge(regen_harvest_height))
h_group_model_age_can <- list(groupHeightModelAge(regen_cover_height))

###### 4.3 Harvest Models ----
h_group_model_harvest_1 <- groupHeightHarvest_1(regen_harvest_height)
h_group_model_harvest_2 <- groupHeightHarvest_2(regen_harvest_height)
h_group_model_harvest_3 <- groupHeightHarvest_3(regen_harvest_height)
h_group_model_harvest_1a <- groupHeightHarvest_1a(regen_harvest_height)
h_group_model_harvest_2a <- groupHeightHarvest_2a(regen_harvest_height)
h_group_model_harvest_3a <- groupHeightHarvest_3a(regen_harvest_height)

###### 4.4 cover Models ----
h_group_model_cover_1 <- groupHeightCover_1(regen_cover_height)
h_group_model_cover_2 <- groupHeightCover_2(regen_cover_height)
h_group_model_cover_3 <- groupHeightCover_3(regen_cover_height)
h_group_model_cover_1a <- groupHeightCover_1a(regen_cover_height)
h_group_model_cover_2a <- groupHeightCover_2a(regen_cover_height)
h_group_model_cover_3a <- groupHeightCover_3a(regen_cover_height)


# 5. Grouping Models -----------------------------------------------------------
height_group_harvest_models <- tibble(ClimaticVarList,
                                      h_group_model_null_harvest,
                                      h_group_model_harvest, h_group_model_age_har,
                                      h_group_model_harvest_1, h_group_model_harvest_1a, 
                                      h_group_model_harvest_2, h_group_model_harvest_2a, 
                                      h_group_model_harvest_3, h_group_model_harvest_3a)

height_group_harvest_models


height_group_cover_models <- tibble(ClimaticVarList,
                                     h_group_model_null_cover,
                                     h_group_model_cover, h_group_model_age_can, 
                                     h_group_model_cover_1, h_group_model_cover_1a, 
                                     h_group_model_cover_2, h_group_model_cover_2a,
                                     h_group_model_cover_3, h_group_model_cover_3a)

height_group_cover_models


# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(height_group_harvest_models, file = here("Data/04_Temp", "height_group_harvest_models.rds"))

# cover models
saveRDS(height_group_cover_models, file = here("Data/04_Temp", "height_group_cover_models.rds"))
