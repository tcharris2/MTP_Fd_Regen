# SITE SPECIFIC HEIGHT - COVER ---------------------------------------------
#' @Content: Height ------------------------------------------------------------
  
#' @Author Thomson Harris
#' @Date Nov 10th 2023

  
# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

# 2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/02_height_cover_model_function.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")


# 3.  Correcting Variable types ----------------------------------------------------
regen_prepped <- universalDataPrepFunction(regen)

# Removing NAs from height
regen_height <- subset(regen_prepped, !(is.na(height)))

# Removing NAs from tree cover 
regen_cover_height <- subset(regen_height, !(is.na(tree_cover)))


str(regen_cover_height)

# This function converts survival, harvestF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Building Out Models ----------------------------------------------------

###### 4.1 Grouping Data ------

height_loc_group_cover <- regen_cover_height %>% 
  group_by(location) %>% 
  nest()

###### 4.2 Storing models in a tibble ----

# Create a new tibble 
height_cover_models <- height_loc_group_cover

# Run the model functions with mutate to store them 

# Stores the null model as Model_0
height_cover_models <- height_cover_models %>% 
  mutate(model_0 = map(data, heightCoverNull))

# Stores Model_1
height_cover_models <- height_cover_models %>% 
  mutate(model_1 = map(data, heightCover_1))

# Stores Model_2
height_cover_models <- height_cover_models %>% 
  mutate(model_2 = map(data, heightCover_2))

# Stores Model_3
height_cover_models <- height_cover_models %>% 
  mutate(model_3 = map(data, heightCover_3))

# Stores ModelHarvest 
height_cover_models <- height_cover_models %>% 
  mutate(model_c = map(data, heightCover))


# looking at the models within the tibble 
height_cover_models

height_cover_models$model_0
height_cover_models$model_1
height_cover_models$model_2
height_cover_models$model_3
height_cover_models$model_h


# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(height_cover_models, file = here("Data/04_Temp", "height_cover_models.rds"))


# 6. Calling model RDS files  -----------------------------------------------------

height_cover_models <- readRDS(file = here("Data/04_Temp", "height_cover_models.rds"))

height_cover_models

# 7. Extracting Diagnostic Values ---------------------------------------------

source("Script/03a_Height_Functions/04_individual_loc_assumptions_diagnostic_functions.R")

###### 7.1 Model 0 ----

# Keeping only nessecary models/information
height_cover_model_0_resid <- subset(height_cover_models, select = c("location", "data", "model_0"))

# Extracting resids/fits and storing them in $data
height_cover_model_0_resid <- model_ResidFits(height_cover_model_0_resid)

height_cover_model_0_resid

# removing model_x so that the dataframe can be unnested
height_cover_model_0_fits <- subset(height_cover_model_0_resid, select = c("location", "data"))

height_cover_model_0_fits <- unnest(height_cover_model_0_fits)
height_cover_model_0_fits

# Graphing
ggplot(data = height_cover_model_0_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2)

ggplot(data = height_cover_model_0_fits) +
  geom_qq(aes(sample = resid)) +
  facet_wrap( ~ location, nrow = 2) +
  labs(title = "cover_model_0_QQplot")


###### 7.2 Model C -------

# Keeping only nessecary models/information
height_cover_model_c_resid <- subset(height_cover_models, select = c("location", "data", "model_c"))

# Extracting resids/fits and storing them in $data
height_cover_model_c_resid <- model_ResidFits(height_cover_model_c_resid)

height_cover_model_c_resid

# removing model_x so that the dataframe can be unnested
cover_model_c_fits <- subset(height_cover_model_c_resid, select = c("location", "data"))

cover_model_c_fits <- unnest(cover_model_c_fits)
cover_model_c_fits

# Graphing
ggplot(data = cover_model_c_fits) +
  geom_point(aes(x = fitted,
                 y = resid)) +
  facet_wrap( ~ location, nrow = 2)

ggplot(data = cover_model_c_fits) +
  geom_qq(aes(sample = resid)) +
  facet_wrap( ~ location, nrow = 2) +
  labs(title = "cover_model_c_QQplot")

# 8. Diagnostic Values by Climatic Var -----------------

# This step is to change the format of the data 
# So it can be operated on

# Keeping nested models (model_1, model_2, model_3)
unnest_height_cover_models <- subset(height_cover_models, select = -c(model_0, model_c))

unnest_height_cover_models

# Unesting models
unnest_height_cover_models <- unnest_height_cover_models %>%
  unnest(c(model_1, model_2, model_3))

unnest_height_cover_models

# Adding climatic variables 
unnest_height_cover_models$climatic_var <- rep(ClimaticVarList, times = 6)

unnest_height_cover_models

# Regrouping by climatic variable
renest_height_cover_models <- unnest_height_cover_models %>% 
  group_by(climatic_var) %>% 
  nest()

renest_height_cover_models

###### 8.1 Creating a dataframe to graph from -----

height_cover_graphing_df <- graphingDfFunction(renest_height_cover_models)
height_cover_graphing_df

melt_height_cover_df <- meltGraphingDF(height_cover_graphing_df)
melt_height_cover_df

# This step is needed for saving the graphs with the correct names
names(melt_height_cover_df) <- paste0("cover_", ClimaticVarList)

###### 8.2 Graphing ----

# This will save outputs in the working directory when run

graphingMeltFunction(melt_height_cover_df)

graphingQQPlotFunction(melt_height_cover_df)


# graphingFunction()






