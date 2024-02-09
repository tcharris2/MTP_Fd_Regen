# HEIGHT ALL LOCATIONS ---------------------------------------------------------
#' @Content: Natural log Height all locations
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023


# *Steps  1-6  run as a background job* -----------------------------------------

#' Run @ln_height_all_locs_background_job.R

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))


# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/03a_ln_height_all_locs_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_height <-  subset(regen_prepped, !(is.na(height)))

regen_height <- subset(regen_height, !(is.na(tree_cover)))

regen_height <- subset(regen_height, !regen_height$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                                     "Peterhope future Fd", "Alex Fraser future Fd", 
                                                                     "Twobit B class Fd"))


str(regen_height)

## 4. Building out models ----------------------------------------------------------

###### 4.1 Null Model ----
ln_h_group_model_null <- list(ln_groupHeightModelNull(regen_height))

###### 4.2 Treatment Models ----
ln_h_group_model_harvest <- list(ln_groupHeightModelHarvest(regen_height))
ln_h_group_model_cover <- list(ln_groupHeightModelCover(regen_height))
ln_h_group_model_age <- list(ln_groupHeightModelAge(regen_height))
ln_h_group_model_age_har <- list(ln_groupHeightModelAgeHarvest(regen_height))
ln_h_group_model_age_can <- list(ln_groupHeightModelAgecover(regen_height))

###### 4.3 Harvest Models ----
ln_h_group_model_harvest_1 <- ln_groupHeightHarvest_1(regen_height)
ln_h_group_model_harvest_2 <- ln_groupHeightHarvest_2(regen_height)
ln_h_group_model_harvest_3 <- ln_groupHeightHarvest_3(regen_height)
ln_h_group_model_harvest_1a <- ln_groupHeightHarvest_1a(regen_height)
ln_h_group_model_harvest_2a <- ln_groupHeightHarvest_2a(regen_height)
ln_h_group_model_harvest_3a <- ln_groupHeightHarvest_3a(regen_height)

###### 4.4 cover Models ----
ln_h_group_model_cover_1 <- ln_groupHeightCover_1(regen_height)
ln_h_group_model_cover_2 <- ln_groupHeightCover_2(regen_height)
ln_h_group_model_cover_3 <- ln_groupHeightCover_3(regen_height)
ln_h_group_model_cover_1a <- ln_groupHeightCover_1a(regen_height)
ln_h_group_model_cover_2a <- ln_groupHeightCover_2a(regen_height)
ln_h_group_model_cover_3a <- ln_groupHeightCover_3a(regen_height)


# 5. Grouping Models -----------------------------------------------------------
ln_height_group_harvest_models <- tibble(ln_h_group_model_null, ln_h_group_model_harvest,
                                         ln_h_group_model_age, ln_h_group_model_age_har,
                                         ln_h_group_model_harvest_1, ln_h_group_model_harvest_1a, 
                                         ln_h_group_model_harvest_2, ln_h_group_model_harvest_2a, 
                                         ln_h_group_model_harvest_3, ln_h_group_model_harvest_3a)

ln_height_group_harvest_models



ln_height_group_cover_models <- tibble(ln_h_group_model_null, ln_h_group_model_cover,
                                        ln_h_group_model_age, ln_h_group_model_age_can, 
                                        ln_h_group_model_cover_1, ln_h_group_model_cover_1a, 
                                        ln_h_group_model_cover_2, ln_h_group_model_cover_2a,
                                        ln_h_group_model_cover_3, ln_h_group_model_cover_3a)

ln_height_group_cover_models




# Adding Climatic Variables
ln_height_group_harvest_models$climatic_var <- ClimaticVarList

ln_height_group_cover_models$climatic_var <- ClimaticVarList


# 6. Saving models as a RDS file --------------------------------------------------

# Harvest models
saveRDS(ln_height_group_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_group_harvest_models_NoFutures.rds" )))

# cover models
saveRDS(ln_height_group_cover_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_group_cover_models_NoFutures.rds" )))

# 7. Calling RDS File  ------------------------------------------------------------

ln_height_group_harvest_models <- readRDS(file = here("Data/04_Temp", "2024-02-07_ln_height_group_harvest_models_NoFutures.rds" ))

ln_height_group_harvest_models

ln_height_group_cover_models <- readRDS(file = here("Data/04_Temp", "2024-02-07_ln_height_group_cover_models_NoFutures.rds" ))

ln_height_group_cover_models


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

###### 8.2 cover ---------

# creating a new nested dataframe for inputing resids and fits
cover_tree_numbers <- subset(regen_cover_height, select = c(tree_number))

ln_height_group_cover_models$data <- rep(list(cover_tree_numbers))

# creating columns for residuals and fits 
ln_height_group_cover_models <- groupDiagnosticColsFunction(ln_height_group_cover_models)

# extracting metrics to fill columns 
ln_height_group_cover_models <- groupExtractMetrics(ln_height_group_cover_models)


# melting dfs so they can be graphed 
ln_height_group_cover_fits <- groupMeltDF(ln_height_group_cover_models)

# removing models from dataset for tidyness
ln_height_group_cover_fits_names <- names(ln_height_group_cover_fits %>% select(contains("data")))

ln_height_group_cover_fits_data <- subset(ln_height_group_cover_fits, select = c("ClimaticVarList", ln_height_group_cover_fits_names))

# adding a column for proper graphings names
ln_height_group_cover_fits_data$names <- rep("ln_cover")
ln_height_group_cover_fits_data


# Graphing

# Residual Vs Fitted
groupGraphingResidFitsFunction(ln_height_group_cover_fits_data)


# QQ plot
groupQQGraphingFunction(ln_height_group_cover_fits_data)


# 9. Testing Harvest Models ----------------------------------------------------
names(ln_height_group_harvest_models)


# rename columns
colnames(ln_height_group_harvest_models) <- c("model_0", "model_h", "model_a", "model_ah",
                                           "model_1", "model_1a", "model_2", "model_2a", 
                                           "model_3", "model_3a", 
                                           "ClimaticVarList")
ln_height_group_harvest_models

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 9.1 Test models ----

# Null vs 1 variable
ln_height_group_harvest_models$lr_test_0_h <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_0,
                                                             model_y = ln_height_group_harvest_models$model_h), 
                                                  recursive = FALSE)

ln_height_group_harvest_models$lr_test_0_a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_0,
                                                             model_y = ln_height_group_harvest_models$model_a), 
                                                  recursive = FALSE)

ln_height_group_harvest_models$lr_test_0_1 <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_0,
                                                             model_y = ln_height_group_harvest_models$model_1), 
                                                  recursive = FALSE)

# Model 1 vs n + 1
ln_height_group_harvest_models$lr_test_1_1a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                              model_x = ln_height_group_harvest_models$model_1,
                                                              model_y = ln_height_group_harvest_models$model_1a), 
                                                   recursive = FALSE)

ln_height_group_harvest_models$lr_test_1_2 <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_1,
                                                             model_y = ln_height_group_harvest_models$model_2), 
                                                  recursive = FALSE)

# Harvest model vs Havest + Climatic
ln_height_group_harvest_models$lr_test_h_2 <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_h,
                                                             model_y = ln_height_group_harvest_models$model_2), 
                                                  recursive = FALSE)

# Model 2 vs n + 1
ln_height_group_harvest_models$lr_test_2_2a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                              model_x = ln_height_group_harvest_models$model_2,
                                                              model_y = ln_height_group_harvest_models$model_2a), 
                                                   recursive = FALSE)

ln_height_group_harvest_models$lr_test_2_3 <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                             model_x = ln_height_group_harvest_models$model_2,
                                                             model_y = ln_height_group_harvest_models$model_3), 
                                                  recursive = FALSE)
# Model 3 vs n + 1
ln_height_group_harvest_models$lr_test_3_3a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                              model_x = ln_height_group_harvest_models$model_3,
                                                              model_y = ln_height_group_harvest_models$model_3a), 
                                                   recursive = FALSE)

# age models
ln_height_group_harvest_models$lr_test_a_1a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                                 model_x = ln_height_group_harvest_models$model_a,
                                                                 model_y = ln_height_group_harvest_models$model_1a), 
                                                      recursive = FALSE)


ln_height_group_harvest_models$lr_test_1a_2a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                                 model_x = ln_height_group_harvest_models$model_1a,
                                                                 model_y = ln_height_group_harvest_models$model_2a), 
                                                      recursive = FALSE)

ln_height_group_harvest_models$lr_test_ah_2a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                                  model_x = ln_height_group_harvest_models$model_ah,
                                                                  model_y = ln_height_group_harvest_models$model_2a), 
                                                       recursive = FALSE)

ln_height_group_harvest_models$lr_test_2a_3a <- unlist(modelsTest(df = ln_height_group_harvest_models,
                                                                 model_x = ln_height_group_harvest_models$model_2a,
                                                                 model_y = ln_height_group_harvest_models$model_3a), 
                                                      recursive = FALSE)
ln_height_group_harvest_models


###### 9.2 Extracting p-values ----
HH_group_p_vals <- extractPVals(ln_height_group_harvest_models)

HH_group_p_vals

HH_group_p_vals <- subset(HH_group_p_vals, 
                          select = c("ClimaticVarList", "p_val_0_h", "p_val_0_a", 
                                     "p_val_0_1", "p_val_1_1a", "p_val_1_2", "p_val_h_2",
                                     "p_val_2_2a", "p_val_2_3", "p_val_3_3a",
                                     "p_val_a_1a", "p_val_1a_2a", "p_val_ah_2a", "p_val_2a_3a"))
HH_group_p_vals

# Isolating Significant P-Values 
HH_group_sig_p_vals <- removeNonSigPVals(HH_group_p_vals)

HH_group_sig_p_vals

###### 9.3 Saving p-values ----
write.csv(HH_group_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                        "_ln_Height_Harvest_group_p_vals_NoFutures.csv")),
          row.names = FALSE)

write.csv(HH_group_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                            "_ln_Height_Harvest_group_sig_p_vals_NoFutures.csv")), 
                                           row.names = FALSE)


# 10. Testing Cover Models ----------------------------------------------------
names(ln_height_group_cover_models)


# rename columns
colnames(ln_height_group_cover_models) <- c("model_0", "model_c", "model_a", "model_ac",
                                              "model_1", "model_1a", "model_2", "model_2a", 
                                              "model_3", "model_3a",
                                              "ClimaticVarList")
ln_height_group_cover_models

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 10.1 Test models ----

# Null vs 1 variable
ln_height_group_cover_models$lr_test_0_c <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_0,
                                                                model_y = ln_height_group_cover_models$model_c), 
                                                     recursive = FALSE)

ln_height_group_cover_models$lr_test_0_a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_0,
                                                                model_y = ln_height_group_cover_models$model_a), 
                                                     recursive = FALSE)

ln_height_group_cover_models$lr_test_0_1 <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_0,
                                                                model_y = ln_height_group_cover_models$model_1), 
                                                     recursive = FALSE)

# Model 1 vs n + 1
ln_height_group_cover_models$lr_test_1_1a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                 model_x = ln_height_group_cover_models$model_1,
                                                                 model_y = ln_height_group_cover_models$model_1a), 
                                                      recursive = FALSE)

ln_height_group_cover_models$lr_test_1_2 <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_1,
                                                                model_y = ln_height_group_cover_models$model_2), 
                                                     recursive = FALSE)

# Harvest model vs Havest + Climatic
ln_height_group_cover_models$lr_test_c_2 <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_c,
                                                                model_y = ln_height_group_cover_models$model_2), 
                                                     recursive = FALSE)

# Model 2 vs n + 1
ln_height_group_cover_models$lr_test_2_2a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                 model_x = ln_height_group_cover_models$model_2,
                                                                 model_y = ln_height_group_cover_models$model_2a), 
                                                      recursive = FALSE)

ln_height_group_cover_models$lr_test_2_3 <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                model_x = ln_height_group_cover_models$model_2,
                                                                model_y = ln_height_group_cover_models$model_3), 
                                                     recursive = FALSE)
# Model 3 vs n + 1
ln_height_group_cover_models$lr_test_3_3a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                 model_x = ln_height_group_cover_models$model_3,
                                                                 model_y = ln_height_group_cover_models$model_3a), 
                                                      recursive = FALSE)

# age models
ln_height_group_cover_models$lr_test_a_1a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                 model_x = ln_height_group_cover_models$model_a,
                                                                 model_y = ln_height_group_cover_models$model_1a), 
                                                      recursive = FALSE)


ln_height_group_cover_models$lr_test_1a_2a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                  model_x = ln_height_group_cover_models$model_1a,
                                                                  model_y = ln_height_group_cover_models$model_2a), 
                                                       recursive = FALSE)

ln_height_group_cover_models$lr_test_ac_2a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                 model_x = ln_height_group_cover_models$model_ac,
                                                                 model_y = ln_height_group_cover_models$model_2a), 
                                                      recursive = FALSE)


ln_height_group_cover_models$lr_test_2a_3a <- unlist(modelsTest(df = ln_height_group_cover_models,
                                                                  model_x = ln_height_group_cover_models$model_2a,
                                                                  model_y = ln_height_group_cover_models$model_3a), 
                                                       recursive = FALSE)

ln_height_group_cover_models


###### 10.2 Extracting p-values ----
HC_group_p_vals <- extractPVals(ln_height_group_cover_models)

HC_group_p_vals

HC_group_p_vals <- subset(HC_group_p_vals, 
                          select = c("ClimaticVarList", "p_val_0_c", "p_val_0_a", 
                                     "p_val_0_1", "p_val_1_1a", "p_val_1_2", "p_val_c_2",
                                     "p_val_2_2a", "p_val_2_3", "p_val_3_3a",
                                     "p_val_a_1a", "p_val_1a_2a", "p_val_ac_2a", "p_val_2a_3a"))
HC_group_p_vals

# Isolating Significant P-Values 
HC_group_sig_p_vals <- removeNonSigPVals(HC_group_p_vals)

HC_group_sig_p_vals

###### 10.3 Saving p-values ----

write.csv(HC_group_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), 
                                                      "_ln_Height_Cover_group_p_vals_NoFutures.csv")),
          row.names = FALSE)

write.csv(HC_group_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(),
                                                          "_ln_Height_Cover_group_sig_p_vals_NoFutures.csv")), 
                                           row.names = FALSE)
