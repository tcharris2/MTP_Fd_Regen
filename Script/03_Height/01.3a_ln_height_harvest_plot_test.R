# SITE SPECIFIC HEIGHT - LN - HARVEST ---------------------------------------------
#' @Content: Height ------------------------------------------------------------

#' @Author Thomson Harris
#' @Date Nov 10th 2023


# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

#  2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/01a_ln_height_harvest_model_function.R")
source("Script/03a_Height_Functions/01.2a_ln_height_harvest_model_function_plot.R")

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
ln_height_harvest_plot <- ln_height_loc_group_harvest

ln_height_harvest_plot
# Run the model functions with mutate to store them 

# Stores the null model as Model_0
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_0 = map(data, ln_heightHarvestNull))

# Stores Model_1
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1))

# Stores Model_2
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2))

# Stores Model_3
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3))

# Stores ModelHarvest 
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_h = map(data, ln_heightHarvest))



# Stores the null model as Model_0
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_0P = map(data, ln_heightHarvestNullP))

# Stores Model_1
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_1P = map(data, ln_heightHarvest_1P))

# Stores Model_2
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_2P = map(data, ln_heightHarvest_2P))

# Stores Model_3
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_3P = map(data, ln_heightHarvest_3P))

# Stores ModelHarvest 
ln_height_harvest_plot <- ln_height_harvest_plot %>% 
  mutate(model_hP = map(data, ln_heightHarvestP))


# looking at the models within the tibble 
ln_height_harvest_plot



# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_harvest_plot, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_harvest_models_Plot.rds" )))


# 6. Calling model RDS files  -----------------------------------------------------

ln_height_harvest_plot <- readRDS(file = here("Data/04_Temp", "2024-02-09_ln_height_harvest_models_Plot.rds"))

ln_height_harvest_plot

# 9. Testing Models ------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(ln_height_harvest_plot$location)

HH_models_P <- subset(ln_height_harvest_plot, select = -c(data, location))

HH_models_P <- HH_models_P %>%
  unnest(c(model_1, model_2, model_3, model_1P, model_2P, model_3P))

HH_models_P$climatic_var <- rep(ClimaticVarList, times = 6)

HH_models_P$location <- rep(loc_list, each = 15)

HH_models_P

# Testing models 

# Null vs 1 variable
HH_models_P$lr_test_0P_0 <- unlist(modelsTest(df = HH_models_P,
                                              model_x = HH_models_P$model_0P,
                                              model_y = HH_models_P$model_0), 
                                   recursive = FALSE)

HH_models_P$lr_test_1P_1 <- unlist(modelsTest(df = HH_models_P,
                                              model_x = HH_models_P$model_1P,
                                              model_y = HH_models_P$model_1), 
                                   recursive = FALSE)

HH_models_P$lr_test_2P_2 <- unlist(modelsTest(df = HH_models_P,
                                              model_x = HH_models_P$model_2P,
                                              model_y = HH_models_P$model_2), 
                                   recursive = FALSE)

HH_models_P$lr_test_3P_3 <- unlist(modelsTest(df = HH_models_P,
                                              model_x = HH_models_P$model_3P,
                                              model_y = HH_models_P$model_3), 
                                   recursive = FALSE)

HH_models_P$lr_test_hP_h <- unlist(modelsTest(df = HH_models_P,
                                              model_x = HH_models_P$model_hP,
                                              model_y = HH_models_P$model_h), 
                                   recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


HH_models_P

###### 9.1 Extracting P-Values ----------------------------------------------------------

HH_models_P_p_vals <- extractPVals(HH_models_P)

HH_models_P_p_vals

HH_P_p_vals <- subset(HH_models_P_p_vals, 
                      select = c("location", "climatic_var", "p_val_0P_0", "p_val_1P_1", 
                                 "p_val_2P_2", "p_val_3P_3", "p_val_hP_h"))
HH_P_p_vals

# Isolating Significant P-Values 

HH_P_sig_p_vals <- removeNonSigPVals(HH_P_p_vals)

HH_P_sig_p_vals


write.csv(HH_P_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_pvals_Plot.csv")), 
          row.names = FALSE)

write.csv(HH_P_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_sig_pvals_Plot.csv")), 
          row.names = FALSE)

