# HEIGHT ALL LOCATIONS ---------------------------------------------------------
#' @Content: Height all locations
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023

  
# *Steps  1-6  run as a background job* -----------------------------------------

#' Run @height_all_locs_background_job.R


# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

# 2. Importing Functions ----------------------------------------------------------

source("Script/02a_Height_Functions/height_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

source("Script/02a_Height_Functions/lrtest_function.R")

# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_harvest_height <-  subset(regen_prepped, !(is.na(height)))

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
height_group_harvest_models <- tibble(h_group_model_null_harvest,
                                      h_group_model_harvest, h_group_model_age_har,
                                      h_group_model_harvest_1, h_group_model_harvest_1a, 
                                      h_group_model_harvest_2, h_group_model_harvest_2a, 
                                      h_group_model_harvest_3, h_group_model_harvest_3a)

height_group_harvest_models


height_group_cover_models <- tibble(h_group_model_null_cover,
                                     h_group_model_cover, h_group_model_age_can, 
                                     h_group_model_cover_1, h_group_model_cover_1a, 
                                     h_group_model_cover_2, h_group_model_cover_2a,
                                     h_group_model_cover_3, h_group_model_cover_3a)

height_group_cover_models


# Adding Climatic Variables
height_group_harvest_models$climatic_var <- ClimaticVarList

height_group_cover_models$climatic_var <- ClimaticVarList


# 6. Saving models as a RDS file --------------------------------------------------


saveRDS(height_background_job_results, file = here("Data/04_Temp", "background_environ_height_group.rds"))

# Harvest models
saveRDS(height_group_harvest_models, file = here("Data/04_Temp", "height_group_harvest_models_df.rds"))

# cover models
saveRDS(height_group_cover_models, file = here("Data/04_Temp", "height_group_cover_models_df.rds"))

# 7. Calling RDS File  ------------------------------------------------------------

height_group_harvest_models <- readRDS(file = here("Data/04_Temp", "height_group_harvest_models.rds"))

height_group_harvest_models

height_group_cover_models <- readRDS(file = here("Data/04_Temp", "height_group_cover_models.rds"))

height_group_cover_models


# 8. Testing Model Assumptions -------------------------------------------------------

# importing functions
source("Script/03a_Height_Functions/05_grouped_loc_assumption_functions.R")

###### 8.1 Harvest -------

# creating a new nested dataframe for inputing resids and fits
harvest_tree_numbers <- subset(regen_harvest_height, select = c(tree_number))

height_group_harvest_models$data <- rep(list(harvest_tree_numbers))

# creating columns for residuals and fits 
height_group_harvest_models <- groupDiagnosticColsFunction(height_group_harvest_models)

# extracting metrics to fill columns 
height_group_harvest_models <- groupExtractMetrics(height_group_harvest_models)


# melting dfs so they can be graphed 
height_group_harvest_fits <- groupMeltDF(height_group_harvest_models)

# removing models from dataset for tidyness
height_group_harvest_fits_names <- names(height_group_harvest_fits %>% select(contains("data")))

height_group_harvest_fits_data <- subset(height_group_harvest_fits, select = c("ClimaticVarList", height_group_harvest_fits_names))

# adding a column for proper graphings names
height_group_harvest_fits_data$names <- rep("harvest")
height_group_harvest_fits_data

# Graphing

# Residual Vs Fitted
groupGraphingMeltFunction(height_group_harvest_fits_data)


# QQ plot
groupQQGraphingFunction(height_group_harvest_fits_data)


###### 8.2 Cover ---------

# creating a new nested dataframe for inputing resids and fits
cover_tree_numbers <- subset(regen_cover_height, select = c(tree_number))

height_group_cover_models$data <- rep(list(cover_tree_numbers))

# creating columns for residuals and fits 
height_group_cover_models <- groupDiagnosticColsFunction(height_group_cover_models)

# extracting metrics to fill columns 
height_group_cover_models <- groupExtractMetrics(height_group_cover_models)


# melting dfs so they can be graphed 
height_group_cover_fits <- groupMeltDF(height_group_cover_models)

# removing models from dataset for tidyness
height_group_cover_fits_names <- names(height_group_cover_fits %>% select(contains("data")))

height_group_cover_fits_data <- subset(height_group_cover_fits, select = c("ClimaticVarList", height_group_cover_fits_names))

# adding a column for proper graphings names
height_group_cover_fits_data$names <- rep("cover")
height_group_cover_fits_data

# Graphing

# Residual Vs Fitted
groupGraphingMeltFunction(height_group_cover_fits_data)


# QQ plot
groupQQGraphingFunction(height_group_cover_fits_data)
