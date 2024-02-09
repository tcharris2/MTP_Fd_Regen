# Content: Blocking Survival cover ------------------------------------------------------------

# Author: Thomson Harris
# Date: Oct 4th, 2023

# Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# Importing Functions ----------------------------------------------------------

source("Script/02a_Survival_Functions/02_survival_cover_model_function.R")

source("Script/02a_Survival_Functions/02.2_survival_cover_model_function_plot.R")

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
survival_cover_plot <- loc_group_cover

survival_cover_plot

# Unblocked Models  ---------------------------------------------------

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_0 = map(data, survivalModelNull))

# Stores Model_1
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_1 = map(data, survivalCover_1))

# Stores Model_2
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_2 = map(data, survivalCover_2))

# Stores Model_3
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_3 = map(data, survivalCover_3))

# Stores Modelcover 
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_c = map(data, survivalModelCover))


# Blocked Models --------------------------------------------------------------

# Stores the null model as Model_0
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_0P = map(data, survivalModelNullP))

# Stores Model_1
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_1P = map(data, survivalCover_1P))

# Stores Model_2
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_2P = map(data, survivalCover_2P))

# Stores Model_3
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_3P = map(data, survivalCover_3P))

# Stores Modelcover 
survival_cover_plot <- survival_cover_plot %>% 
  mutate(model_cP = map(data, survivalModelCoverP))



survival_cover_plot 

# Saving models as a RDS file --------------------------------------------------

saveRDS(survival_cover_plot, file = here("Data/04_Temp", paste0(Sys.Date(), "_survival_cover_models_plot.rds")))

# Calling model RDS files  -----------------------------------------------------

survival_cover_plot <- readRDS(file = here("Data/04_Temp", "2024-02-02_survival_cover_models_plot.rds"))

survival_cover_plot


# Testing models ---------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(survival_cover_plot$location)

SC_models_P <- subset(survival_cover_plot, select = -c(data, location))

SC_models_P <- SC_models_P %>%
  unnest(c(model_1, model_2, model_3, model_1P, model_2P, model_3P))

SC_models_P$climatic_var <- rep(ClimaticVarList, times = 6)

SC_models_P$location <- rep(loc_list, each = 15)

SC_models_P

# Testing models 

# Null vs 1 variable
SC_models_P$lr_test_0P_0 <- unlist(modelsTest(df = SC_models_P,
                                              model_x = SC_models_P$model_0P,
                                              model_y = SC_models_P$model_0), 
                                   recursive = FALSE)

SC_models_P$lr_test_1P_1 <- unlist(modelsTest(df = SC_models_P,
                                              model_x = SC_models_P$model_1P,
                                              model_y = SC_models_P$model_1), 
                                   recursive = FALSE)

SC_models_P$lr_test_2P_2 <- unlist(modelsTest(df = SC_models_P,
                                              model_x = SC_models_P$model_2P,
                                              model_y = SC_models_P$model_2), 
                                   recursive = FALSE)

SC_models_P$lr_test_3P_3 <- unlist(modelsTest(df = SC_models_P,
                                              model_x = SC_models_P$model_3P,
                                              model_y = SC_models_P$model_3), 
                                   recursive = FALSE)

SC_models_P$lr_test_cP_c<- unlist(modelsTest(df = SC_models_P,
                                             model_x = SC_models_P$model_cP,
                                             model_y = SC_models_P$model_c), 
                                  recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. cover of tree_cover)
# Look at the name  of the file to find the variable used. 


SC_models_P


# Extracting P-Values ----------------------------------------------------------

SC_models_P_p_vals <- extractPVals(SC_models_P)

SC_models_P_p_vals

SC_P_p_vals <- subset(SC_models_P_p_vals, 
                      select = c("location", "climatic_var", "p_val_0P_0", "p_val_1P_1", 
                                 "p_val_2P_2", "p_val_3P_3", "p_val_cP_c"))
SC_P_p_vals

# Isolating Significant P-Values 

SC_P_sig_p_vals <- removeNonSigPVals(SC_P_p_vals)

SC_P_sig_p_vals


write.csv(SC_P_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_pvals_Plot.csv")), 
          row.names = FALSE)

write.csv(SC_P_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_Survival_Cover_sig_pvals_Plot.csv")), 
          row.names = FALSE)


