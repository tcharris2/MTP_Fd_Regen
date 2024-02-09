# Content: Blocking Survival Harvest ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/01_survival_harvest_model_function.R")

source("Script/02a_Survival_Functions/01.2_survival_harvest_model_function_plot.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)


# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# Grouping Data ----------------------------------------------------------------

loc_group_harvest <- regen_survival %>% 
  group_by(location) %>% 
  nest()

# Create a new tibble 
survival_harvest_plot <- loc_group_harvest

survival_harvest_plot


# Unblocked Models  ---------------------------------------------------

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_1 = map(data, survivalHarvest_1))

# Stores Model_2
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_2 = map(data, survivalHarvest_2))

# Stores Model_3
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_3 = map(data, survivalHarvest_3))

# Stores ModelHarvest 
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_h = map(data, survivalModelHarvest))


# Blocked Models --------------------------------------------------------------

# Stores the null model as Model_0
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_0P = map(data, survivalModelNullP))

# Stores Model_1
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_1P = map(data, survivalHarvest_1P))

# Stores Model_2
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_2P = map(data, survivalHarvest_2P))

# Stores Model_3
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_3P = map(data, survivalHarvest_3P))

# Stores ModelHarvest 
survival_harvest_plot <- survival_harvest_plot %>% 
  mutate(model_hP = map(data, survivalModelHarvestP))



survival_harvest_plot


# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_harvest_plot, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_harvest_models_plot.rds")))

# Calling model RDS files  -----------------------------------------------------

survival_harvest_plot <- readRDS(file = here("Data/04_Temp", "2024-02-08_survival_harvest_models_plot.rds"))

survival_harvest_plot


# Testing models ---------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(survival_harvest_plot$location)

SH_models_P <- subset(survival_harvest_plot, select = -c(data, location))

SH_models_P <- SH_models_P %>%
  unnest(c(model_1, model_2, model_3, model_1P, model_2P, model_3P))

SH_models_P$climatic_var <- rep(ClimaticVarList, times = 6)

SH_models_P$location <- rep(loc_list, each = 15)

SH_models_P

# Testing models 

# Null vs 1 variable
SH_models_P$lr_test_0P_0 <- unlist(modelsTest(df = SH_models_P,
                                           model_x = SH_models_P$model_0P,
                                           model_y = SH_models_P$model_0), 
                                recursive = FALSE)

SH_models_P$lr_test_1P_1 <- unlist(modelsTest(df = SH_models_P,
                                           model_x = SH_models_P$model_1P,
                                           model_y = SH_models_P$model_1), 
                                recursive = FALSE)

SH_models_P$lr_test_2P_2 <- unlist(modelsTest(df = SH_models_P,
                                           model_x = SH_models_P$model_2P,
                                           model_y = SH_models_P$model_2), 
                                recursive = FALSE)

SH_models_P$lr_test_3P_3 <- unlist(modelsTest(df = SH_models_P,
                                           model_x = SH_models_P$model_3P,
                                           model_y = SH_models_P$model_3), 
                                recursive = FALSE)

SH_models_P$lr_test_hP_h<- unlist(modelsTest(df = SH_models_P,
                                           model_x = SH_models_P$model_hP,
                                           model_y = SH_models_P$model_h), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


SH_models_P


# Extracting P-Values ----------------------------------------------------------

SH_models_P_p_vals <- extractPVals(SH_models_P)

SH_models_P_p_vals

SH_P_p_vals <- subset(SH_models_P_p_vals, 
                    select = c("location", "climatic_var", "p_val_0P_0", "p_val_1P_1", 
                               "p_val_2P_2", "p_val_3P_3", "p_val_hP_h"))
SH_P_p_vals

# Isolating Significant P-Values 

SH_P_sig_p_vals <- removeNonSigPVals(SH_P_p_vals)

SH_P_sig_p_vals


write.csv(SH_P_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_pvals_Plot.csv")), 
          row.names = FALSE)

write.csv(SH_P_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Harvest_sig_pvals_Plot.csv")), 
          row.names = FALSE)


