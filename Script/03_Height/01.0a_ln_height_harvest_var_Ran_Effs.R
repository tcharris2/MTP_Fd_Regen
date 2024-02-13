# SITE SPECIFIC HEIGHT - LN - HARVEST ---------------------------------------------
#' @Content: Height ------------------------------------------------------------
  
#' @Author Thomson Harris
#' @Date Feb 09th 2024

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

#  2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/01.1a_ln_height_harvest_model_function_splitplot.R")
source("Script/03a_Height_Functions/01.2a_ln_height_harvest_model_function_plot.R")
source("Script/03a_Height_Functions/01.3a_ln_height_harvest_model_function_block.R")

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

# 4. Grouping Data by Random Effects ---------------------------------------------

ln_height_loc_group_harvest <- regen_height %>% 
  group_by(location) %>% 
  nest()

###### 4.1 Block dataframe ----
ln_height_harvest_models_B <- ln_height_loc_group_harvest[ln_height_loc_group_harvest$location 
                                                          == "Jaffray", ]

###### 4.2 Plot dataframe ----
ln_height_harvest_models_P <- ln_height_loc_group_harvest[ln_height_loc_group_harvest$location 
                                                          %in% c("Narrows", "Redfish", "Twobit"), ]

###### 4.3 Splitplot dataframe ----
ln_height_harvest_models_S <- ln_height_loc_group_harvest[ln_height_loc_group_harvest$location 
                                                          %in% c("Alex Fraser", "John Prince"), ]


# 5. Storing Block Models ------------------------------------------------------

###### 5.1 Model_0B ----
ln_height_harvest_models_B <- ln_height_harvest_models_B %>% 
  mutate(model_0 = map(data, ln_heightHarvestNullB))

###### 5.2 Model_1B ----
ln_height_harvest_models_B <- ln_height_harvest_models_B %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1B))

###### 5.3 Model_2B ----
ln_height_harvest_models_B <- ln_height_harvest_models_B %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2B))

###### 5.4 Model_3B ----
ln_height_harvest_models_B <- ln_height_harvest_models_B %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3B))

###### 5.5 Model_hB ----
ln_height_harvest_models_B <- ln_height_harvest_models_B %>% 
  mutate(model_h = map(data, ln_heightHarvestB))


# 6. Storing Plot Models ------------------------------------------------------

###### 6.1 Model_0P ----
ln_height_harvest_models_P <- ln_height_harvest_models_P %>% 
  mutate(model_0 = map(data, ln_heightHarvestNullP))

###### 6.2 Model_1P ----
ln_height_harvest_models_P <- ln_height_harvest_models_P %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1P))

###### 6.3 Model_2P ----
ln_height_harvest_models_P <- ln_height_harvest_models_P %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2P))

###### 6.4 Model_3P ----
ln_height_harvest_models_P <- ln_height_harvest_models_P %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3P))

###### 6.6 Model_hP ----
ln_height_harvest_models_P <- ln_height_harvest_models_P %>% 
  mutate(model_h = map(data, ln_heightHarvestP))


# 7. Storing Splitplot Models ------------------------------------------------------

###### 7.1 Model_0S ----
ln_height_harvest_models_S <- ln_height_harvest_models_S %>% 
  mutate(model_0 = map(data, ln_heightHarvestNullS))

###### 7.2 Model_1S ----
ln_height_harvest_models_S <- ln_height_harvest_models_S %>% 
  mutate(model_1 = map(data, ln_heightHarvest_1S))

###### 7.3 Model_2S ----
ln_height_harvest_models_S <- ln_height_harvest_models_S %>% 
  mutate(model_2 = map(data, ln_heightHarvest_2S))

###### 7.4 Model_3S ----
ln_height_harvest_models_S <- ln_height_harvest_models_S %>% 
  mutate(model_3 = map(data, ln_heightHarvest_3S))

###### 7.7 Model_hS ----
ln_height_harvest_models_S <- ln_height_harvest_models_S %>% 
  mutate(model_h = map(data, ln_heightHarvestS))



# 8. Merging Model Dataframes -----------------------------------------------

ln_height_harvest_models <- rbind(ln_height_harvest_models_B, ln_height_harvest_models_P, ln_height_harvest_models_S) 

ln_height_harvest_models


# 9. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_harvest_models, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_harvest_models_var_Ran_Effs.rds" )))


# 10. Calling model RDS files  -----------------------------------------------------

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", "2024-02-09_ln_height_harvest_models_var_Ran_Effs.rds"))

ln_height_harvest_models

# 11. Extracting Diagnostic Values ---------------------------------------------

source("Script/03a_Height_Functions/04_individual_loc_assumptions_diagnostic_functions.R")

###### 11.1 Model 0 ----

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

###### 11.2 Model H -------

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

# 12. Diagnostic Values by Climatic Var -----------------

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

###### 12.1 Creating a dataframe to graph from -----

ln_height_harvest_graphing_df <- graphingDfFunction(renest_ln_height_harvest_models)
ln_height_harvest_graphing_df

melt_ln_height_harvest_df <- meltGraphingDF(ln_height_harvest_graphing_df)
melt_ln_height_harvest_df

# This step is needed for saving the graphs with the correct names
names(melt_ln_height_harvest_df) <- paste0("ln_harvest_", ClimaticVarList)

###### 12.2 Graphing ----

# This will save outputs in the working directory when run

graphingMeltFunction(melt_ln_height_harvest_df)

graphingQQPlotFunction(melt_ln_height_harvest_df)



# 13. Testing Models ------------------------------------------------------------

###### 13.1 Importing Functions -----
source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

###### 13.2 Un-nesting Models ----- 

loc_list <- unique(ln_height_harvest_models$location)

HH_models <- subset(ln_height_harvest_models, select = -c(data, location))

HH_models <- HH_models %>%
  unnest(c(model_1, model_2, model_3))

HH_models$climatic_var <- rep(ClimaticVarList, times = 6)

HH_models$location <- rep(loc_list, each = 15)

HH_models

###### 13.3 Running Tests ----- 

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


###### 13.4 Extracting P-Values ----------------------------------------------------------

HH_models_p_vals <- extractPVals(HH_models)

HH_models_p_vals

HH_p_vals <- subset(HH_models_p_vals, 
                    select = c("location", "climatic_var", "p_val_0_1", "p_val_1_2", 
                               "p_val_2_3", "p_val_0_h", "p_val_h_2"))
HH_p_vals

###### 13.5 Isolating Significant P-Values ------

HH_sig_p_vals <- removeNonSigPVals(HH_p_vals)

HH_sig_p_vals

# 14. Saving P-Values as .CSV --------------------------------------------------

write.csv(HH_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_pvals_var_Ran_Effs.csv")), 
          row.names = FALSE)

write.csv(HH_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Harvest_sig_pvals_var_Ran_Effs.csv")), 
          row.names = FALSE)

