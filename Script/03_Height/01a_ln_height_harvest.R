# SITE SPECIFIC HEIGHT - LN - HARVEST ---------------------------------------------
#' @Content: Height ------------------------------------------------------------
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

#  2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/01a_ln_height_harvest_model_function.R")

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
ln_height_harvest_models <- ln_height_loc_group_harvest

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
ln_height_harvest_models <- ln_height_harvest_models %>% 
  mutate(model_0 = map(data, ln_heightHarvestNull))

# Stores Model_1
ln_height_harvest_models <- ln_height_harvest_models %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1))

# Stores Model_2
ln_height_harvest_models <- ln_height_harvest_models %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2))

# Stores Model_3
ln_height_harvest_models <- ln_height_harvest_models %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3))

# Stores ModelHarvest 
ln_height_harvest_models <- ln_height_harvest_models %>% 
  mutate(model_h = map(data, ln_heightHarvest))


# looking at the models within the tibble 
ln_height_harvest_models

ln_height_harvest_models$model_0
ln_height_harvest_models$model_1
ln_height_harvest_models$model_2
ln_height_harvest_models$model_3
ln_height_harvest_models$model_h


# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_harvest_models_OutEdit_NoFutures.rds" )))


# 6. Calling model RDS files  -----------------------------------------------------

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", "2024-02-02_ln_height_harvest_models_OutEdit_Bv1.rds"))

ln_height_harvest_models

# 7. Extracting Diagnostic Values ---------------------------------------------

source("Script/03a_Height_Functions/04_individual_loc_assumptions_diagnostic_functions.R")

###### 7.1 Model 0 ----

# Keeping only nessecary models/information
ln_height_harvest_model_0_resid <- subset(ln_height_harvest_models, select = c("location", "data", "model_0"))

# Extracting resids/fits and storing them in $data
ln_height_harvest_model_0_resid <- model_ResidFits(ln_height_harvest_model_0_resid)

ln_height_harvest_model_0_resid

# removing model_x so that the dataframe can be unnested
ln_harvest_model_0_fits <- subset(ln_height_harvest_model_0_resid, select = c("location", "data"))

ln_harvest_model_0_fits <- unnest(ln_harvest_model_0_fits)
ln_harvest_model_0_fits

# Graphing
ggplot(data = ln_harvest_model_0_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2) +
  labs(title = "ln_harvest_model_0_resid_fitted")

ggplot(data = ln_harvest_model_0_fits) +
  geom_qq(aes(sample = resid)) +
  facet_wrap( ~ location, nrow = 2) +
  labs(title = "ln_harvest_model_0_QQplot")

###### 7.2 Model H -------

# Keeping only nessecary models/information
ln_height_harvest_model_h_resid <- subset(ln_height_harvest_models, select = c("location", "data", "model_h"))

# Extracting resids/fits and storing them in $data
ln_height_harvest_model_h_resid <- model_ResidFits(ln_height_harvest_model_h_resid)

ln_height_harvest_model_h_resid

# removing model_x so that the dataframe can be unnested
ln_harvest_model_h_fits <- subset(ln_height_harvest_model_h_resid, select = c("location", "data"))

ln_harvest_model_h_fits <- unnest(ln_harvest_model_h_fits)
ln_harvest_model_h_fits

# Graphing
ggplot(data = ln_harvest_model_h_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2)+
  labs(title = "ln_harvest_model_h_resid_fitted")

ggplot(data = ln_harvest_model_h_fits) +
  geom_qq(aes(sample = resid)) +
  facet_wrap( ~ location, nrow = 2) +
  labs(title = "ln_harvest_model_h_QQplot")

# 8. Diagnostic Values by Climatic Var -----------------

# This step is to change the format of the data 
# So it can be operated on

# Keeping nested models (model_1, model_2, model_3)
unnest_ln_height_harvest_models <- subset(ln_height_harvest_models, select = -c(model_0, model_h))

unnest_ln_height_harvest_models

# Unesting models
unnest_ln_height_harvest_models <- unnest_ln_height_harvest_models %>%
  unnest(c(model_1, model_2, model_3))

unnest_ln_height_harvest_models

# Adding climatic variables 
unnest_ln_height_harvest_models$climatic_var <- rep(ClimaticVarList, times = 6)

unnest_ln_height_harvest_models

# Regrouping by climatic variable
renest_ln_height_harvest_models <- unnest_ln_height_harvest_models %>% 
  group_by(climatic_var) %>% 
  nest()

renest_ln_height_harvest_models

###### 8.1 Creating a dataframe to graph from -----

ln_height_harvest_graphing_df <- graphingDfFunction(renest_ln_height_harvest_models)
ln_height_harvest_graphing_df

melt_ln_height_harvest_df <- meltGraphingDF(ln_height_harvest_graphing_df)
melt_ln_height_harvest_df

# This step is needed for saving the graphs with the correct names
names(melt_ln_height_harvest_df) <- paste0("ln_harvest_", ClimaticVarList)

###### 8.2 Graphing ----

# This will save outputs in the working directory when run

graphingMeltFunction(melt_ln_height_harvest_df)

graphingQQPlotFunction(melt_ln_height_harvest_df)


# graphingFunction()


# 9. Testing Models ------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(ln_height_harvest_models$location)

HH_models <- subset(ln_height_harvest_models, select = -c(data, location))

HH_models <- HH_models %>%
  unnest(c(model_1, model_2, model_3))

HH_models$climatic_var <- rep(ClimaticVarList, times = 6)

HH_models$location <- rep(loc_list, each = 15)

HH_models

# Testing models 

# Null vs 1 variable
HH_models$lr_test_0_1 <- unlist(modelsTest(df = HH_models,
                                           model_x = HH_models$model_0,
                                           model_y = HH_models$model_1), 
                                recursive = FALSE)

HH_models$lr_test_1_2 <- unlist(modelsTest(df = HH_models,
                                           model_x = HH_models$model_1,
                                           model_y = HH_models$model_2), 
                                recursive = FALSE)

HH_models$lr_test_2_3 <- unlist(modelsTest(df = HH_models,
                                           model_x = HH_models$model_2,
                                           model_y = HH_models$model_3), 
                                recursive = FALSE)

HH_models$lr_test_0_h <- unlist(modelsTest(df = HH_models,
                                           model_x = HH_models$model_0,
                                           model_y = HH_models$model_h), 
                                recursive = FALSE)

HH_models$lr_test_h_2 <- unlist(modelsTest(df = HH_models,
                                           model_x = HH_models$model_h,
                                           model_y = HH_models$model_2), 
                                recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. harvest of tree_cover)
# Look at the name  of the file to find the variable used. 


HH_models
HH_models$lr_test_0_1
HH_models$lr_test_1_2
HH_models$lr_test_2_3
HH_models$lr_test_0_h
HH_models$lr_test_h_2


###### 9.1 Extracting P-Values ----------------------------------------------------------

HH_models_p_vals <- extractPVals(HH_models)

HH_models_p_vals

HH_p_vals <- subset(HH_models_p_vals, 
                    select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                               "p_val_2_3", "p_val_0_h", "p_val_h_2"))
HH_p_vals

# Isolating Significant P-Values 

HH_sig_p_vals <- removeNonSigPVals(HH_p_vals)

HH_sig_p_vals


write.csv(HH_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_pvals_NoFutures.csv")), 
          row.names = FALSE)

write.csv(HH_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_sig_pvals_NoFutures.csv")), 
          row.names = FALSE)

