# HEIGHT ALL LOCATIONS ---------------------------------------------------------
#' @Content: Natural log Height all locations
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023


# *Steps  1-6  run as a background job* -----------------------------------------

#' Run @ln_height_all_locs_background_job.R

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/03a_ln_height_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

source("Script/02a_Height_Functions/lrtest_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_harvest_height <-  subset(regen_prepped, !(is.na(height)))

str(regen_harvest_height)

regen_canopy_height <- subset(regen_harvest_height, !(is.na(tree_cover)))
## 4. Building out models ----------------------------------------------------------

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
ln_height_group_harvest_models <- tibble(ln_h_group_model_null_harvest,
                                         ln_h_group_model_harvest, ln_h_group_model_age_har,
                                         ln_h_group_model_harvest_1, ln_h_group_model_harvest_1a, 
                                         ln_h_group_model_harvest_2, ln_h_group_model_harvest_2a, 
                                         ln_h_group_model_harvest_3, ln_h_group_model_harvest_3a)

ln_height_group_harvest_models


ln_height_group_canopy_models <- tibble(ln_h_group_model_null_canopy,
                                        ln_h_group_model_canopy, ln_h_group_model_age_can, 
                                        ln_h_group_model_canopy_1, ln_h_group_model_canopy_1a, 
                                        ln_h_group_model_canopy_2, ln_h_group_model_canopy_2a,
                                        ln_h_group_model_canopy_3, ln_h_group_model_canopy_3a)

ln_height_group_canopy_models



# Adding Climatic Variables
height_group_harvest_models$climatic_var <- ClimaticVarList

height_group_canopy_models$climatic_var <- ClimaticVarList


# 6. Saving models as a RDS file --------------------------------------------------

# Harvest models
# saveRDS(height_group_harvest_models, file = here("Data/04_Temp", "height_group_harvest_models_df.rds"))

# Canopy models
# saveRDS(height_group_canopy_models, file = here("Data/04_Temp", "height_group_canopy_models_df.rds"))

# 7. Calling RDS File  ------------------------------------------------------------

ln_height_group_harvest_models <- readRDS(file = here("Data/04_Temp", "202401301_ln_height_group_harvest_models_OutEdit.rds"))

ln_height_group_harvest_models

ln_height_group_canopy_models <- readRDS(file = here("Data/04_Temp", "202401301_ln_height_group_canopy_models_OutEdit.rds"))

ln_height_group_canopy_models


# 8. Testing Model Assumptions -----------------------------------------------------------

# importing functions
source("Script/03a_Height_Functions/05_grouped_loc_assumption_functions.R")

###### 8.1 Harvest -------

# creating a new nested dataframe for inputing resids and fits
harvest_tree_numbers <- subset(regen_harvest_height, select = c(tree_number))

ln_height_group_harvest_models$data <- rep(list(harvest_tree_numbers))

# creating columns for residuals and fits 
ln_height_group_harvest_models <- groupDiagnosticColsFunction(ln_height_group_harvest_models)

# extracting metrics to fill columns 
ln_height_group_harvest_models <- groupExtractMetrics(ln_height_group_harvest_models)


# melting dfs so they can be graphed 
ln_height_group_harvest_fits <- groupMeltDF(ln_height_group_harvest_models)

# removing models from dataset for tidyness
ln_height_group_harvest_fits_names <- names(ln_height_group_harvest_fits %>% select(contains("data")))

ln_height_group_harvest_fits_data <- subset(ln_height_group_harvest_fits, select = c("ClimaticVarList", ln_height_group_harvest_fits_names))

# adding a column for proper graphings names
ln_height_group_harvest_fits_data$names <- rep("ln_harvest")
ln_height_group_harvest_fits_data

# Graphing

# Residual Vs Fitted
groupGraphingResidFitsFunction(ln_height_group_harvest_fits_data)


# QQ plot
groupQQGraphingFunction(ln_height_group_harvest_fits_data)

###### 8.2 Canopy ---------

# creating a new nested dataframe for inputing resids and fits
canopy_tree_numbers <- subset(regen_canopy_height, select = c(tree_number))

ln_height_group_canopy_models$data <- rep(list(canopy_tree_numbers))

# creating columns for residuals and fits 
ln_height_group_canopy_models <- groupDiagnosticColsFunction(ln_height_group_canopy_models)

# extracting metrics to fill columns 
ln_height_group_canopy_models <- groupExtractMetrics(ln_height_group_canopy_models)


# melting dfs so they can be graphed 
ln_height_group_canopy_fits <- groupMeltDF(ln_height_group_canopy_models)

# removing models from dataset for tidyness
ln_height_group_canopy_fits_names <- names(ln_height_group_canopy_fits %>% select(contains("data")))

ln_height_group_canopy_fits_data <- subset(ln_height_group_canopy_fits, select = c("ClimaticVarList", ln_height_group_canopy_fits_names))

# adding a column for proper graphings names
ln_height_group_canopy_fits_data$names <- rep("ln_canopy")
ln_height_group_canopy_fits_data


# Graphing

# Residual Vs Fitted
groupGraphingResidFitsFunction(ln_height_group_canopy_fits_data)


# QQ plot
groupQQGraphingFunction(ln_height_group_canopy_fits_data)




