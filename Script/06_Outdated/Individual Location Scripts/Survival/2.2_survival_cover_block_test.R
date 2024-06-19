# Content: Blocking Survival cover ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/02_survival_cover_model_function.R")

source("Script/02a_Survival_Functions/02.1_survival_cover_model_function_blocked.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

# Correcting Variable types ----------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

str(regen_survival)


# This function converts survival, coverF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# Grouping Data ----------------------------------------------------------------

loc_group_cover <- regen_survival %>% 
  group_by(location) %>% 
  nest()

# Create a new tibble 
survival_cover_blocking <- loc_group_cover

survival_cover_blocking

# need to remove narrows and twobit

survival_cover_blocking <- survival_cover_blocking[survival_cover_blocking$location != "Narrows" & 
                                                         survival_cover_blocking$location != "Twobit", ]

survival_cover_blocking
# Unblocked Models  ---------------------------------------------------

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_1 = map(data, survivalCover_1))

# Stores Model_2
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_2 = map(data, survivalCover_2))

# Stores Model_3
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_3 = map(data, survivalCover_3))

# Stores Modelcover 
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_c = map(data, survivalModelCover))


# Blocked Models --------------------------------------------------------------

# Stores the null model as Model_0
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_0B = map(data, survivalModelNullB))

# Stores Model_1
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_1B = map(data, survivalCover_1B))

# Stores Model_2
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_2B = map(data, survivalCover_2B))

# Stores Model_3
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_3B = map(data, survivalCover_3B))

# Stores Modelcover 
survival_cover_blocking <- survival_cover_blocking %>% 
  mutate(model_cB = map(data, survivalModelCoverB))



survival_cover_blocking 

# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_cover_blocking, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_cover_models_blocking.rds")))

# Calling model RDS files  -----------------------------------------------------

survival_cover_blocking <- readRDS(file = here("Data/04_Temp", "2024-02-02_survival_cover_models_blocking.rds"))

survival_cover_blocking


# Testing models ---------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(survival_cover_blocking$location)

SC_models_B <- subset(survival_cover_blocking, select = -c(data, location))

SC_models_B <- SC_models_B %>%
  unnest(c(model_1, model_2, model_3, model_1B, model_2B, model_3B))

SC_models_B$climatic_var <- rep(ClimaticVarList, times = 4)

SC_models_B$location <- rep(loc_list, each = 15)

SC_models_B

# Testing models 

# Null vs 1 variable
SC_models_B$lr_test_0_0B <- unlist(modelsTest(df = SC_models_B,
                                              model_x = SC_models_B$model_0,
                                              model_y = SC_models_B$model_0B), 
                                   recursive = FALSE)

SC_models_B$lr_test_1_1B <- unlist(modelsTest(df = SC_models_B,
                                              model_x = SC_models_B$model_1,
                                              model_y = SC_models_B$model_1B), 
                                   recursive = FALSE)

SC_models_B$lr_test_2_2B <- unlist(modelsTest(df = SC_models_B,
                                              model_x = SC_models_B$model_2,
                                              model_y = SC_models_B$model_2B), 
                                   recursive = FALSE)

SC_models_B$lr_test_3_3B <- unlist(modelsTest(df = SC_models_B,
                                              model_x = SC_models_B$model_3,
                                              model_y = SC_models_B$model_3B), 
                                   recursive = FALSE)

SC_models_B$lr_test_c_cB<- unlist(modelsTest(df = SC_models_B,
                                             model_x = SC_models_B$model_c,
                                             model_y = SC_models_B$model_cB), 
                                  recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. cover of tree_cover)
# Look at the name  of the file to find the variable used. 


SC_models_B


# Extracting P-Values ----------------------------------------------------------

SC_models_B_p_vals <- extractPVals(SC_models_B)

SC_models_B_p_vals

SC_B_p_vals <- subset(SC_models_B_p_vals, 
                      select = c("location", "climatic_var", "p_val_0_0B", "p_val_1_1B", 
                                 "p_val_2_2B", "p_val_3_3B", "p_val_c_cB"))
SC_B_p_vals

# Isolating Significant P-Values 

SC_B_sig_p_vals <- removeNonSigPVals(SC_B_p_vals)

SC_B_sig_p_vals


write.csv(SC_B_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_pvals_Blocked.csv")), 
          row.names = FALSE)

write.csv(SC_B_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_sig_pvals_Blocked.csv")), 
          row.names = FALSE)


