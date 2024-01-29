# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Height_Functions/ln_height_all_locs_model_function.R")

source("Script/02a_Height_Functions/height_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_harvest_height <- heightDataPrepFunction(regen)

str(regen_harvest_height)

regen_canopy_height <- subset(regen_harvest_height, !(is.na(tree_cover)))

# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
ln_h_group_model_null_harvest <- list(ln_groupHeightModelNull(regen_harvest_height))
ln_h_group_model_null_canopy <- list(ln_groupHeightModelNull(regen_canopy_height))

###### 4.2 Treatment Models ----
ln_h_group_model_harvest <- list(ln_groupHeightModelHarvest(regen_harvest_height))
ln_h_group_model_canopy <- list(ln_groupHeightModelCanopy(regen_canopy_height))
ln_h_group_model_age_har <- list(ln_groupHeightModelAge(regen_harvest_height))
ln_h_group_model_age_can <- list(ln_groupHeightModelAge(regen_canopy_height))

###### 4.3 Harvest Models ----
ln_h_group_model_harvest_1 <- ln_groupHeightHarvest_1(regen_harvest_height)
ln_h_group_model_harvest_2 <- ln_groupHeightHarvest_2(regen_harvest_height)
ln_h_group_model_harvest_3 <- ln_groupHeightHarvest_3(regen_harvest_height)
ln_h_group_model_harvest_1a <- ln_groupHeightHarvest_1a(regen_harvest_height)
ln_h_group_model_harvest_2a <- ln_groupHeightHarvest_2a(regen_harvest_height)
ln_h_group_model_harvest_3a <- ln_groupHeightHarvest_3a(regen_harvest_height)

###### 4.4 Canopy Models ----
ln_h_group_model_canopy_1 <- ln_groupHeightCanopy_1(regen_canopy_height)
ln_h_group_model_canopy_2 <- ln_groupHeightCanopy_2(regen_canopy_height)
ln_h_group_model_canopy_3 <- ln_groupHeightCanopy_3(regen_canopy_height)
ln_h_group_model_canopy_1a <- ln_groupHeightCanopy_1a(regen_canopy_height)
ln_h_group_model_canopy_2a <- ln_groupHeightCanopy_2a(regen_canopy_height)
ln_h_group_model_canopy_3a <- ln_groupHeightCanopy_3a(regen_canopy_height)


# 5. Grouping Models -----------------------------------------------------------
ln_height_group_harvest_models <- tibble(ClimaticVarList,
                                         ln_h_group_model_null_harvest,
                                         ln_h_group_model_harvest, ln_h_group_model_age_har,
                                         ln_h_group_model_harvest_1, ln_h_group_model_harvest_1a, 
                                         ln_h_group_model_harvest_2, ln_h_group_model_harvest_2a, 
                                         ln_h_group_model_harvest_3, ln_h_group_model_harvest_3a)

ln_height_group_harvest_models


ln_height_group_canopy_models <- tibble(ClimaticVarList,
                                        ln_h_group_model_null_canopy,
                                        ln_h_group_model_canopy, ln_h_group_model_age_can, 
                                        ln_h_group_model_canopy_1, ln_h_group_model_canopy_1a, 
                                        ln_h_group_model_canopy_2, ln_h_group_model_canopy_2a,
                                        ln_h_group_model_canopy_3, ln_h_group_model_canopy_3a)

ln_height_group_canopy_models



# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(ln_height_group_harvest_models, file = here("Data/04_Temp", "ln_height_group_harvest_models.rds"))

# Canopy models
saveRDS(ln_height_group_canopy_models, file = here("Data/04_Temp", "ln_height_group_canopy_models.rds"))