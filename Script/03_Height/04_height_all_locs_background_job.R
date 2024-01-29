# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Height_Functions/height_all_locs_model_function.R")

source("Script/02a_Height_Functions/height_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_harvest_height <- heightDataPrepFunction(regen)

str(regen_harvest_height)

regen_canopy_height <- subset(regen_harvest_height, !(is.na(tree_cover)))

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
h_group_model_null_harvest <- list(groupHeightModelNull(regen_harvest_height))
h_group_model_null_canopy <- list(groupHeightModelNull(regen_canopy_height))

###### 4.2 Treatment Models ----
h_group_model_harvest <- list(groupHeightModelHarvest(regen_harvest_height))
h_group_model_canopy <- list(groupHeightModelCanopy(regen_canopy_height))
h_group_model_age_har <- list(groupHeightModelAge(regen_harvest_height))
h_group_model_age_can <- list(groupHeightModelAge(regen_canopy_height))

###### 4.3 Harvest Models ----
h_group_model_harvest_1 <- groupHeightHarvest_1(regen_harvest_height)
h_group_model_harvest_2 <- groupHeightHarvest_2(regen_harvest_height)
h_group_model_harvest_3 <- groupHeightHarvest_3(regen_harvest_height)
h_group_model_harvest_1a <- groupHeightHarvest_1a(regen_harvest_height)
h_group_model_harvest_2a <- groupHeightHarvest_2a(regen_harvest_height)
h_group_model_harvest_3a <- groupHeightHarvest_3a(regen_harvest_height)

###### 4.4 Canopy Models ----
h_group_model_canopy_1 <- groupHeightCanopy_1(regen_canopy_height)
h_group_model_canopy_2 <- groupHeightCanopy_2(regen_canopy_height)
h_group_model_canopy_3 <- groupHeightCanopy_3(regen_canopy_height)
h_group_model_canopy_1a <- groupHeightCanopy_1a(regen_canopy_height)
h_group_model_canopy_2a <- groupHeightCanopy_2a(regen_canopy_height)
h_group_model_canopy_3a <- groupHeightCanopy_3a(regen_canopy_height)


# 5. Grouping Models -----------------------------------------------------------
height_group_harvest_models <- tibble(ClimaticVarList,
                                      h_group_model_null_harvest,
                                      h_group_model_harvest, h_group_model_age_har,
                                      h_group_model_harvest_1, h_group_model_harvest_1a, 
                                      h_group_model_harvest_2, h_group_model_harvest_2a, 
                                      h_group_model_harvest_3, h_group_model_harvest_3a)

height_group_harvest_models


height_group_canopy_models <- tibble(ClimaticVarList,
                                     h_group_model_null_canopy,
                                     h_group_model_canopy, h_group_model_age_can, 
                                     h_group_model_canopy_1, h_group_model_canopy_1a, 
                                     h_group_model_canopy_2, h_group_model_canopy_2a,
                                     h_group_model_canopy_3, h_group_model_canopy_3a)

height_group_canopy_models


# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(height_group_harvest_models, file = here("Data/04_Temp", "height_group_harvest_models.rds"))

# Canopy models
saveRDS(height_group_canopy_models, file = here("Data/04_Temp", "height_group_canopy_models.rds"))
