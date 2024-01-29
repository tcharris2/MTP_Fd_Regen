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
height_group_harvest_models <- tibble(h_group_model_null_harvest,
                                      h_group_model_harvest, h_group_model_age_har,
                                      h_group_model_harvest_1, h_group_model_harvest_1a, 
                                      h_group_model_harvest_2, h_group_model_harvest_2a, 
                                      h_group_model_harvest_3, h_group_model_harvest_3a)

height_group_harvest_models


height_group_canopy_models <- tibble(h_group_model_null_canopy,
                                     h_group_model_canopy, h_group_model_age_can, 
                                     h_group_model_canopy_1, h_group_model_canopy_1a, 
                                     h_group_model_canopy_2, h_group_model_canopy_2a,
                                     h_group_model_canopy_3, h_group_model_canopy_3a)

height_group_canopy_models


# Adding Climatic Variables
height_group_harvest_models$climatic_var <- ClimaticVarList

height_group_canopy_models$climatic_var <- ClimaticVarList


# 6. Saving models as a RDS file --------------------------------------------------


saveRDS(height_background_job_results, file = here("Data/04_Temp", "background_environ_height_group.rds"))

# Harvest models
saveRDS(height_group_harvest_models, file = here("Data/04_Temp", "height_group_harvest_models_df.rds"))

# Canopy models
saveRDS(height_group_canopy_models, file = here("Data/04_Temp", "height_group_canopy_models_df.rds"))

# 7. Calling RDS File  ------------------------------------------------------------

height_group_harvest_models <- readRDS(file = here("Data/04_Temp", "height_group_harvest_models.rds"))

height_group_harvest_models

height_group_canopy_models <- readRDS(file = here("Data/04_Temp", "height_group_canopy_models.rds"))

height_group_canopy_models


# 8. Testing Model Assumptions -------------------------------------------------------
names(height_group_harvest_models)

tree_numbers <- subset(regen_harvest_height, select = c(tree_number))


height_group_harvest_models$data <- rep(list(tree_numbers))

class(height_group_harvest_models$data[[1]][[1]])
height_group_harvest_models$data

height_group_harvest_models <- groupDiagnosticColsFunction(height_group_harvest_models)
names(height_group_harvest_models$data[[1]])

height_group_harvest_models <- groupExtractMetrics(height_group_harvest_models)

# melting dfs


full_melt_test <- groupMeltDF(height_group_harvest_models)

full_melt_data_names <- names(full_melt_test %>% select(contains("data")))

full_melt_data <- subset(full_melt_test, select = c("ClimaticVarList", full_melt_data_names))
full_melt_data

groupGraphingMeltFunction(full_melt_data)

###### 8.1 Importing Function ---------

source("Script/03a_Height_Functions/04_assumptions_diagnostic_functions.R")


####### 8.2 Running Functions ------ 
diagnostic_height_harvest <- diagnosticColsFunction(height_group_harvest_models)
diagnostic_height_harvest

diagnostic_height_harvest <- extractMetrics(diagnostic_height_harvest)
diagnostic_height_harvest



p <- ggplot()
i <- 1
while(i <= length(diagnostic_height_harvest)) {
  df <- diagnostic_height_harvest[[i]]
  p <- p + geom_line(data = df, aes(fitted_h_group_model_harvest_1, resid_h_group_model_harvest_1))  
  i <- i + 1
}
p



for (i in 1:nrow(diagnostic_height_harvest)) { 
  print(ggplot() +
          geom_point(aes(x = diagnostic_height_harvest$fitted_h_group_model_harvest_1[[i]],
                         y = diagnostic_height_harvest$resid_h_group_model_harvest_1[[i]]),
                     size = 0.25)) 
  Sys.sleep(2)
}

for (i in 1:nrow(diagnostic_height_harvest)) { 
  print(ggplot(aes(x = diagnostic_height_harvest$fitted_h_group_model_harvest_1[[i]],
                   y = diagnostic_height_harvest$resid_h_group_model_harvest_1[[i]])) +
          geom_point(size = 0.25)) 
}



p <- ggplot() + 
  geom_point(aes(x = diagnostic_height_harvest$fitted_h_group_model_harvest[[1]], 
                 y = diagnostic_height_harvest$resid_h_group_model_harvest[[1]]))

ggplot() + 
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest[[1]])))


diagnostic_height_harvest$resid_h_group_model_harvest[[1]]
diagnostic_height_harvest$fitted_h_group_model_harvest[[1]]

