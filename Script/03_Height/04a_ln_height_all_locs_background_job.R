# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/03a_ln_height_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

# Removing Outliers
regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_harvest_height <-  subset(regen_prepped, !(is.na(height)))

str(regen_harvest_height)

regen_cover_height <- subset(regen_harvest_height, !(is.na(tree_cover)))
# 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
ln_h_group_model_null_harvest <- list(ln_groupHeightModelNull(regen_harvest_height))
ln_h_group_model_null_cover <- list(ln_groupHeightModelNull(regen_cover_height))

###### 4.2 Treatment Models ----
ln_h_group_model_harvest <- list(ln_groupHeightModelHarvest(regen_harvest_height))
ln_h_group_model_cover <- list(ln_groupHeightModelCover(regen_cover_height))
ln_h_group_model_age_har <- list(ln_groupHeightModelAge(regen_harvest_height))
ln_h_group_model_age_can <- list(ln_groupHeightModelAge(regen_cover_height))

###### 4.3 Harvest Models ----
ln_h_group_model_harvest_1 <- ln_groupHeightHarvest_1(regen_harvest_height)
ln_h_group_model_harvest_2 <- ln_groupHeightHarvest_2(regen_harvest_height)
ln_h_group_model_harvest_3 <- ln_groupHeightHarvest_3(regen_harvest_height)
ln_h_group_model_harvest_1a <- ln_groupHeightHarvest_1a(regen_harvest_height)
ln_h_group_model_harvest_2a <- ln_groupHeightHarvest_2a(regen_harvest_height)
ln_h_group_model_harvest_3a <- ln_groupHeightHarvest_3a(regen_harvest_height)

###### 4.4 cover Models ----
ln_h_group_model_cover_1 <- ln_groupHeightCover_1(regen_cover_height)
ln_h_group_model_cover_2 <- ln_groupHeightCover_2(regen_cover_height)
ln_h_group_model_cover_3 <- ln_groupHeightCover_3(regen_cover_height)
ln_h_group_model_cover_1a <- ln_groupHeightCover_1a(regen_cover_height)
ln_h_group_model_cover_2a <- ln_groupHeightCover_2a(regen_cover_height)
ln_h_group_model_cover_3a <- ln_groupHeightCover_3a(regen_cover_height)


# 5. Grouping Models -----------------------------------------------------------
ln_height_group_harvest_models <- tibble(ClimaticVarList,
                                         ln_h_group_model_null_harvest,
                                         ln_h_group_model_harvest, ln_h_group_model_age_har,
                                         ln_h_group_model_harvest_1, ln_h_group_model_harvest_1a, 
                                         ln_h_group_model_harvest_2, ln_h_group_model_harvest_2a, 
                                         ln_h_group_model_harvest_3, ln_h_group_model_harvest_3a)

ln_height_group_harvest_models


ln_height_group_cover_models <- tibble(ClimaticVarList,
                                        ln_h_group_model_null_cover,
                                        ln_h_group_model_cover, ln_h_group_model_age_can, 
                                        ln_h_group_model_cover_1, ln_h_group_model_cover_1a, 
                                        ln_h_group_model_cover_2, ln_h_group_model_cover_2a,
                                        ln_h_group_model_cover_3, ln_h_group_model_cover_3a)

ln_height_group_cover_models



# 6. Saving output ---------------------------------------------------------------

# Harvest models
saveRDS(ln_height_group_harvest_models, file = here("Data/04_Temp", "20240131_ln_height_group_harvest_models_OutEdit.rds"))

# cover models
saveRDS(ln_height_group_cover_models, file = here("Data/04_Temp", "20240131_ln_height_group_cover_models_OutEdit.rds"))