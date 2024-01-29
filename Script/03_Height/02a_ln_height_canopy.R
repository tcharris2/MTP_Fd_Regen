# SITE SPECIFIC HEIGHT - LN - CANOPY ---------------------------------------------
#' @Content: Height ------------------------------------------------------------
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/02a_ln_height_canopy_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")


# 3.  Correcting Variable types ----------------------------------------------------
regen_prepped <- universalDataPrepFunction(regen)

# Removing NAs from height
regen_height <- subset(regen_prepped, !(is.na(height)))

# Removing NAs from tree cover 
regen_canopy_height <- subset(regen_height, !(is.na(tree_cover)))

regen_canopy_ln_height <- regen_canopy_height

regen_canopy_ln_height$ln_height <- log(regen_canopy_height$height)


str(regen_canopy_ln_height)

# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Building Out Models ----------------------------------------------------

###### 4.1 Grouping Data ------

ln_height_loc_group_canopy <- regen_canopy_ln_height %>% 
  group_by(location) %>% 
  nest()

###### 4.2 Storing models in a tibble ----

# Create a new tibble 
ln_height_canopy_models <- ln_height_loc_group_canopy

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
ln_height_canopy_models <- ln_height_canopy_models %>% 
  mutate(model_0 = map(data, ln_heightCanopyNull))

# Stores Model_1
ln_height_canopy_models <- ln_height_canopy_models %>% 
  mutate(model_1 = map(data, ln_heightCanopy_1))

# Stores Model_2
ln_height_canopy_models <- ln_height_canopy_models %>% 
  mutate(model_2 = map(data, ln_heightCanopy_2))

# Stores Model_3
ln_height_canopy_models <- ln_height_canopy_models %>% 
  mutate(model_3 = map(data, ln_heightCanopy_3))

# Stores ModelHarvest 
ln_height_canopy_models <- ln_height_canopy_models %>% 
  mutate(model_c = map(data, ln_heightCanopy))


# looking at the models within the tibble 
ln_height_canopy_models

ln_height_canopy_models$model_0
ln_height_canopy_models$model_1
ln_height_canopy_models$model_2
ln_height_canopy_models$model_3
ln_height_canopy_models$model_h


# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_canopy_models, file = here("Data/04_Temp", "ln_height_canopy_models.rds"))


# 6. Calling model RDS files  -----------------------------------------------------

ln_height_canopy_models <- readRDS(file = here("Data/04_Temp", "ln_height_canopy_models.rds"))

ln_height_canopy_models

# 7. Extracting Diagnostic Values ---------------------------------------------

source("Script/03a_ln_height_Functions/04_individual_loc_assumptions_diagnostic_functions.R")

###### 7.1 Model 0 ----

# Keeping only nessecary models/information
ln_height_canopy_model_0_resid <- subset(ln_height_canopy_models, select = c("location", "data", "model_0"))

# Extracting resids/fits and storing them in $data
ln_height_canopy_model_0_resid <- model_ResidFits(ln_height_canopy_model_0_resid)

ln_height_canopy_model_0_resid

# removing model_x so that the dataframe can be unnested
ln_height_canopy_model_0_fits <- subset(ln_height_canopy_model_0_resid, select = c("location", "data"))

ln_height_canopy_model_0_fits <- unnest(ln_height_canopy_model_0_fits)
ln_height_canopy_model_0_fits

# Graphing
ggplot(data = ln_height_canopy_model_0_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2)

###### 7.2 Model C -------

# Keeping only nessecary models/information
ln_height_canopy_model_c_resid <- subset(ln_height_canopy_models, select = c("location", "data", "model_c"))

# Extracting resids/fits and storing them in $data
ln_height_canopy_model_c_resid <- model_ResidFits(ln_height_canopy_model_c_resid)

ln_height_canopy_model_c_resid

# removing model_x so that the dataframe can be unnested
ln_canopy_model_c_fits <- subset(ln_height_canopy_model_c_resid, select = c("location", "data"))

ln_canopy_model_c_fits <- unnest(canopy_model_c_fits)
ln_canopy_model_c_fits

# Graphing
ggplot(data = canopy_model_c_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2)

# 8. Diagnostic Values by Climatic Var -----------------

# This step is to change the format of the data 
# So it can be operated on

# Keeping nested models (model_1, model_2, model_3)
unnest_ln_height_canopy_models <- subset(ln_height_canopy_models, select = -c(model_0, model_c))

unnest_ln_height_canopy_models

# Unesting models
unnest_ln_height_canopy_models <- unnest_ln_height_canopy_models %>%
  unnest(c(model_1, model_2, model_3))

unnest_ln_height_canopy_models

# Adding climatic variables 
unnest_ln_height_canopy_models$climatic_var <- rep(ClimaticVarList, times = 6)

unnest_ln_height_canopy_models

# Regrouping by climatic variable
renest_ln_height_canopy_models <- unnest_ln_height_canopy_models %>% 
  group_by(climatic_var) %>% 
  nest()

renest_ln_height_canopy_models

###### 8.1 Creating a dataframe to graph from -----

ln_height_canopy_graphing_df <- graphingDfFunction(renest_ln_height_canopy_models)
ln_height_canopy_graphing_df

melt_ln_height_canopy_df <- meltGraphingDF(ln_height_canopy_graphing_df)
melt_ln_height_canopy_df

# This step is needed for saving the graphs with the correct names
names(melt_ln_height_canopy_df) <- paste0("ln_canopy_", ClimaticVarList)

###### 8.2 Graphing ----

# This will save outputs in the working directory when run

graphingMeltFunction(melt_ln_height_canopy_df)

# graphingFunction()






