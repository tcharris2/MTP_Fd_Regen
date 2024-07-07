# HEIGHT ALL LOCATIONS ---------------------------------------------------------
#' @Content: Natural log Height all locations
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023


# *Steps  1-6  run as a background job* -----------------------------------------

#' Run @ln_height_all_locs_background_job.R

  
# 1. Importing Data ---------------------------------------------------------------
### 1.1. Loading Packages ----
library(here)
library(tidyverse)
library(lmer)
library(lmtest)
library(reshape2)

### 1.2. Loading Data -----
regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/02_ln_height_all_locs_model_function.R")

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
ln_h_model_age_har <- list(ln_HeightModelAgeHarvest(regen_height))
ln_h_model_age_can <- list(ln_HeightModelAgeCover(regen_height))

###### 4.3 Climate Models ----
ln_h_model_1 <- ln_HeightClimate_1(regen_height)

###### 4.4 Harvest Models ----
ln_h_model_harvest_2 <- ln_HeightHarvest_2(regen_height)
ln_h_model_harvest_3 <- ln_HeightHarvest_3(regen_height)

###### 4.5 Cover Models ----
ln_h_model_cover_2 <- ln_HeightCover_2(regen_height)
ln_h_model_cover_3 <- ln_HeightCover_3(regen_height)


# 5. Grouping Models -----------------------------------------------------------
ln_height_harvest_models <- tibble("model_0" = ln_h_model_null,
                                   "model_h" = ln_h_model_harvest,
                                   "model_1" = ln_h_model_1,
                                   "model_2" = ln_h_model_harvest_2, 
                                   "model_3" = ln_h_model_harvest_3, 
                                   ClimaticVarList)

ln_height_harvest_models


ln_height_cover_models <- tibble("model_0" = ln_h_model_null,
                                "model_c" = ln_h_model_cover, 
                                "model_1" = ln_h_model_1, 
                                "model_2" = ln_h_model_cover_2,
                                "model_3" = ln_h_model_cover_3,
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

# 7. Calling RDS File  ------------------------------------------------------------

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                      "2024-03-11_ln_height_harvest_models.rds" ))

ln_height_harvest_models

ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                                    "2024-03-11_ln_height_cover_models.rds" ))

ln_height_cover_models


# 8. Testing Model Assumptions -----------------------------------------------------------

# importing functions
source("Script/03a_Height_Functions/05_grouped_loc_assumption_functions.R")

###### 8.1 Harvest -------

# 1. creating a new nested dataframe for inputing resids and fits
# "tree_number" is used as an key to map residuals to
harvest_tree_numbers <- subset(regen_height, select = c(tree_number))

# 2. nesting key to fit into the model dataframe 
ln_height_harvest_models$data <- rep(list(harvest_tree_numbers))

# 3. creating columns for residuals and fits 
ln_height_harvest_models <- diagnosticCols(ln_height_harvest_models)

# 4. extracting metrics to fill columns 
ln_height_harvest_models <- extractMetrics(ln_height_harvest_models)

# 5. melting dfs so they can be graphed 
ln_height_harvest_fits <- groupMeltDF(ln_height_harvest_models)

# removing models from dataset for tidyness
ln_height_harvest_fits_names <- names(ln_height_harvest_fits %>% select(contains("data")))

ln_height_harvest_fits_data <- subset(ln_height_harvest_fits,
                                        select = c("ClimaticVarList", ln_height_harvest_fits_names))

# adding a column for proper graphings names
ln_height_harvest_fits_data$names <- rep("ln_harvest")
ln_height_harvest_fits_data

# Graphing

# Residual Vs Fitted
graphingResidFits(ln_height_harvest_fits_data)


# QQ plot
QQGraphing(ln_height_harvest_fits_data)

###### 8.2 Cover ---------

# 1. creating a new nested dataframe for inputing resids and fits
# "tree_number" is used as an key to map residuals to
cover_tree_numbers <- subset(regen_height, select = c(tree_number))

# 2. nesting key to fit into the model dataframe 
ln_height_cover_models$data <- rep(list(cover_tree_numbers))

# 3. creating columns for residuals and fits 
ln_height_cover_models <- diagnosticCols(ln_height_cover_models)

# 4. extracting metrics to fill columns 
ln_height_cover_models <- extractMetrics(ln_height_cover_models)

# 5. melting dfs so they can be graphed 
ln_height_cover_fits <- groupMeltDF(ln_height_cover_models)

# removing models from dataset for tidyness
ln_height_cover_fits_names <- names(ln_height_cover_fits %>% select(contains("data")))

ln_height_cover_fits_data <- subset(ln_height_cover_fits,
                                      select = c("ClimaticVarList", ln_height_cover_fits_names))

# adding a column for proper graphings names
ln_height_cover_fits_data$names <- rep("ln_cover")
ln_height_cover_fits_data

# Graphing

# Residual Vs Fitted
graphingResidFits(ln_height_cover_fits_data)


# QQ plot
QQGraphing(ln_height_cover_fits_data)


# 9. Testing Harvest Models ----------------------------------------------------

ln_height_harvest_models

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 9.1 Test models ----

# Null vs 1 variable
ln_height_harvest_models$lr_test_0_h <- unlist(modelsTest(df = ln_height_harvest_models,
                                                             model_x = ln_height_harvest_models$model_0,
                                                             model_y = ln_height_harvest_models$model_h), 
                                                  recursive = FALSE)

ln_height_harvest_models$lr_test_0_1 <- unlist(modelsTest(df = ln_height_harvest_models,
                                                             model_x = ln_height_harvest_models$model_0,
                                                             model_y = ln_height_harvest_models$model_1), 
                                                  recursive = FALSE)

# Model 1 vs n + 1
ln_height_harvest_models$lr_test_1_2 <- unlist(modelsTest(df = ln_height_harvest_models,
                                                             model_x = ln_height_harvest_models$model_1,
                                                             model_y = ln_height_harvest_models$model_2), 
                                                  recursive = FALSE)

# Harvest model vs Havest + Climatic
ln_height_harvest_models$lr_test_h_2 <- unlist(modelsTest(df = ln_height_harvest_models,
                                                             model_x = ln_height_harvest_models$model_h,
                                                             model_y = ln_height_harvest_models$model_2), 
                                                  recursive = FALSE)

# Model 2 vs n + 1
ln_height_harvest_models$lr_test_2_3 <- unlist(modelsTest(df = ln_height_harvest_models,
                                                             model_x = ln_height_harvest_models$model_2,
                                                             model_y = ln_height_harvest_models$model_3), 
                                                  recursive = FALSE)
# Model 3 vs n + 1
ln_height_harvest_models


###### 9.2 Extracting p-values ----
HH_p_vals <- extractPVals(ln_height_harvest_models)

HH_p_vals

HH_p_vals <- subset(HH_p_vals, 
                          select = c("ClimaticVarList", "p_val_0_h", 
                                     "p_val_0_1",  "p_val_1_2", "p_val_h_2",
                                     "p_val_2_3"))
HH_p_vals

# Isolating Significant P-Values 
HH_sig_p_vals <- removeNonSigPVals(HH_p_vals)

HH_sig_p_vals

###### 9.3 Saving p-values ----
write.csv(HH_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                        "_ln_Height_Harvest_p_vals.csv")),
          row.names = FALSE)

write.csv(HH_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                            "_ln_Height_Harvest_sig_p_vals.csv")), 
                                           row.names = FALSE)


# 10. Testing Cover Models ----------------------------------------------------
ln_height_cover_models

###### 10.1 Test models ----

# Null vs 1 variable
ln_height_cover_models$lr_test_0_c <- unlist(modelsTest(df = ln_height_cover_models,
                                                                model_x = ln_height_cover_models$model_0,
                                                                model_y = ln_height_cover_models$model_c), 
                                                     recursive = FALSE)

ln_height_cover_models$lr_test_0_1 <- unlist(modelsTest(df = ln_height_cover_models,
                                                                model_x = ln_height_cover_models$model_0,
                                                                model_y = ln_height_cover_models$model_1), 
                                                     recursive = FALSE)

# Model 1 vs n + 1
ln_height_cover_models$lr_test_1_2 <- unlist(modelsTest(df = ln_height_cover_models,
                                                                model_x = ln_height_cover_models$model_1,
                                                                model_y = ln_height_cover_models$model_2), 
                                                     recursive = FALSE)

# Harvest model vs Havest + Climatic
ln_height_cover_models$lr_test_c_2 <- unlist(modelsTest(df = ln_height_cover_models,
                                                                model_x = ln_height_cover_models$model_c,
                                                                model_y = ln_height_cover_models$model_2), 
                                                     recursive = FALSE)

# Model 2 vs n + 1
ln_height_cover_models$lr_test_2_3 <- unlist(modelsTest(df = ln_height_cover_models,
                                                                model_x = ln_height_cover_models$model_2,
                                                                model_y = ln_height_cover_models$model_3), 
                                                     recursive = FALSE)
# Model 3 vs n + 1
ln_height_cover_models


###### 10.2 Extracting p-values ----
HC_p_vals <- extractPVals(ln_height_cover_models)

HC_p_vals

HC_p_vals <- subset(HC_p_vals, 
                          select = c("ClimaticVarList", "p_val_0_c",
                                     "p_val_0_1", "p_val_1_2", "p_val_c_2",
                                     "p_val_2_3"))
HC_p_vals

# Isolating Significant P-Values 
HC_sig_p_vals <- removeNonSigPVals(HC_p_vals)

HC_sig_p_vals

###### 10.3 Saving p-values ----

write.csv(HC_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                      "_ln_Height_Cover_p_vals.csv")),
          row.names = FALSE)

write.csv(HC_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(),
                                                          "_ln_Height_Cover_sig_p_vals.csv")), 
                                           row.names = FALSE)

