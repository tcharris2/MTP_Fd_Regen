# SITE SPECIFIC HEIGHT - LN - HARVEST ---------------------------------------------
#' @Content: Height ------------------------------------------------------------

#' @Author Thomson Harris
#' @Date Nov 10th 2023


# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

#  2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/01a_ln_height_harvest_model_function.R")
source("Script/03a_Height_Functions/01.1a_ln_height_harvest_model_function_blocked.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3.  Correcting Variable types ----------------------------------------------------
regen_prepped <- universalDataPrepFunction(regen)

regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_height <-  subset(regen_prepped, !(is.na(height)))

regen_height <- subset(regen_height, !(is.na(tree_cover)))

str(regen_height)

# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Building Out Models ----------------------------------------------------

###### 4.1 Grouping Data ------

ln_height_loc_group_harvest <- regen_height %>% 
  group_by(location) %>% 
  nest()

###### 4.2 Storing models in a tibble ----

# Create a new tibble 
ln_height_harvest_blocked <- ln_height_loc_group_harvest

ln_height_harvest_blocked <- ln_height_harvest_blocked[ln_height_harvest_blocked$location != "Narrows" & 
                                                         ln_height_harvest_blocked$location != "Twobit", ]

ln_height_harvest_blocked
# Run the model functions with mutate to store them 

# Stores the null model as Model_0
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_0 = map(data, ln_heightHarvestNull))

# Stores Model_1
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1))

# Stores Model_2
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2))

# Stores Model_3
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3))

# Stores ModelHarvest 
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_h = map(data, ln_heightHarvest))



# Stores the null model as Model_0
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_0B = map(data, ln_heightHarvestNullB))

# Stores Model_1
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_1B = map(data, ln_heightHarvest_1B))

# Stores Model_2
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_2B = map(data, ln_heightHarvest_2B))

# Stores Model_3
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_3B = map(data, ln_heightHarvest_3B))

# Stores ModelHarvest 
ln_height_harvest_blocked <- ln_height_harvest_blocked %>% 
  mutate(model_hB = map(data, ln_heightHarvestB))


# looking at the models within the tibble 
ln_height_harvest_blocked



# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_harvest_blocked, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_harvest_models_Blocked.rds" )))


# 6. Calling model RDS files  -----------------------------------------------------

ln_height_harvest_blocked <- readRDS(file = here("Data/04_Temp", "2024-02-09_ln_height_harvest_models_Blocked.rds"))

ln_height_harvest_blocked

# 9. Testing Models ------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(ln_height_harvest_blocked$location)

HH_models_B <- subset(ln_height_harvest_blocked, select = -c(data, location))

HH_models_B <- HH_models_B %>%
  unnest(c(model_1, model_2, model_3, model_1B, model_2B, model_3B))

HH_models_B$climatic_var <- rep(ClimaticVarList, times = 4)

HH_models_B$location <- rep(loc_list, each = 15)

HH_models_B

# Testing models 

# Null vs 1 variable
HH_models_B$lr_test_0_0B <- unlist(modelsTest(df = HH_models_B,
                                           model_x = HH_models_B$model_0,
                                           model_y = HH_models_B$model_0B), 
                                recursive = FALSE)

HH_models_B$lr_test_1_1B <- unlist(modelsTest(df = HH_models_B,
                                           model_x = HH_models_B$model_1,
                                           model_y = HH_models_B$model_1B), 
                                recursive = FALSE)

HH_models_B$lr_test_2_2B <- unlist(modelsTest(df = HH_models_B,
                                           model_x = HH_models_B$model_2,
                                           model_y = HH_models_B$model_2B), 
                                recursive = FALSE)

HH_models_B$lr_test_3_3B <- unlist(modelsTest(df = HH_models_B,
                                           model_x = HH_models_B$model_3,
                                           model_y = HH_models_B$model_3B), 
                                recursive = FALSE)

HH_models_B$lr_test_h_hB <- unlist(modelsTest(df = HH_models_B,
                                           model_x = HH_models_B$model_h,
                                           model_y = HH_models_B$model_hB), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


HH_models_B

###### 9.1 Extracting P-Values ----------------------------------------------------------

HH_models_B_p_vals <- extractPVals(HH_models_B)

HH_models_B_p_vals

HH_B_p_vals <- subset(HH_models_B_p_vals, 
                    select = c("location", "climatic_var", "p_val_0_0B", "p_val_1_1B", 
                               "p_val_2_2B", "p_val_3_3B", "p_val_h_hB"))
HH_B_p_vals

# Isolating Significant P-Values 

HH_B_sig_p_vals <- removeNonSigPVals(HH_B_p_vals)

HH_B_sig_p_vals


write.csv(HH_B_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_pvals_Blocked.csv")), 
          row.names = FALSE)

write.csv(HH_B_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_sig_pvals_Blocked.csv")), 
          row.names = FALSE)

