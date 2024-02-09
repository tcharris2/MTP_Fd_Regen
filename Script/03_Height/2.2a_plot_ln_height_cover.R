# SITE SPECIFIC HEIGHT - LN - cover ---------------------------------------------
#' @Content: Height ------------------------------------------------------------

#' @Author Thomson Harris
#' @Date Nov 10th 2023


# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

#  2. Importing Functions ----------------------------------------------------------

source("Script/03a_Height_Functions/02a_ln_height_cover_model_function.R")
source("Script/03a_Height_Functions/02.2a_ln_height_cover_model_function_plot.R")

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3.  Correcting Variable types ----------------------------------------------------
regen_prepped <- universalDataPrepFunction(regen)

regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_height <-  subset(regen_prepped, !(is.na(height)))

regen_height <- subset(regen_height, !(is.na(tree_cover)))

str(regen_height)

# This function converts survival, coverF, provenanceF, and all the random 
# effects into factors. 

# It also normalizes all the climatic distance variables. 

# 4. Building Out Models ----------------------------------------------------

###### 4.1 Grouping Data ------

ln_height_loc_group_cover <- regen_height %>% 
  group_by(location) %>% 
  nest()

###### 4.2 Storing models in a tibble ----

# Create a new tibble 
ln_height_cover_plot <- ln_height_loc_group_cover

ln_height_cover_plot
# Run the model functions with mutate to store them 

# Stores the null model as Model_0
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_0 = map(data, ln_heightCoverNull))

# Stores Model_1
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_1 = map(data, ln_heightCover_1))

# Stores Model_2
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_2 = map(data, ln_heightCover_2))

# Stores Model_3
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_3 = map(data, ln_heightCover_3))

# Stores Modelcover 
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_c = map(data, ln_heightCover))



# Stores the null model as Model_0
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_0P = map(data, ln_heightCoverNullP))

# Stores Model_1
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_1P = map(data, ln_heightCover_1P))

# Stores Model_2
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_2P = map(data, ln_heightCover_2P))

# Stores Model_3
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_3P = map(data, ln_heightCover_3P))

# Stores Modelcover 
ln_height_cover_plot <- ln_height_cover_plot %>% 
  mutate(model_cP = map(data, ln_heightCoverP))


# looking at the models within the tibble 
ln_height_cover_plot



# 5. Saving models as a RDS file --------------------------------------------------

saveRDS(ln_height_cover_plot, file = here("Data/04_Temp", paste0(Sys.Date(), "_ln_height_cover_models_Plot.rds" )))


# 6. Calling model RDS files  -----------------------------------------------------

ln_height_cover_plot <- readRDS(file = here("Data/04_Temp", "2024-02-09_ln_height_cover_models_Plot.rds"))

ln_height_cover_plot

# 9. Testing Models ------------------------------------------------------------

source("Script/01_Universal_Functions/01_lrtest_function_updated.R")

# Expanding the lists 

loc_list <- unique(ln_height_cover_plot$location)

HC_models_P <- subset(ln_height_cover_plot, select = -c(data, location))

HC_models_P <- HC_models_P %>%
  unnest(c(model_1, model_2, model_3, model_1P, model_2P, model_3P))

HC_models_P$climatic_var <- rep(ClimaticVarList, times = 6)

HC_models_P$location <- rep(loc_list, each = 15)

HC_models_P

# Testing models 

# Null vs 1 variable
HC_models_P$lr_test_0P_0 <- unlist(modelsTest(df = HC_models_P,
                                              model_x = HC_models_P$model_0P,
                                              model_y = HC_models_P$model_0), 
                                   recursive = FALSE)

HC_models_P$lr_test_1P_1 <- unlist(modelsTest(df = HC_models_P,
                                              model_x = HC_models_P$model_1P,
                                              model_y = HC_models_P$model_1), 
                                   recursive = FALSE)

HC_models_P$lr_test_2P_2 <- unlist(modelsTest(df = HC_models_P,
                                              model_x = HC_models_P$model_2P,
                                              model_y = HC_models_P$model_2), 
                                   recursive = FALSE)

HC_models_P$lr_test_3P_3 <- unlist(modelsTest(df = HC_models_P,
                                              model_x = HC_models_P$model_3P,
                                              model_y = HC_models_P$model_3), 
                                   recursive = FALSE)

HC_models_P$lr_test_cP_c <- unlist(modelsTest(df = HC_models_P,
                                              model_x = HC_models_P$model_cP,
                                              model_y = HC_models_P$model_c), 
                                   recursive = FALSE)



# note the change from "_h" to "_trt" this is to keep functions consistent across 
# multiple uses 
# "_trt" denotes the "treatment" or non-climatic variable analysed (ie. cover of tree_cover)
# Look at the name  of the file to find the variable used. 


HC_models_P

###### 9.1 Extracting P-Values ----------------------------------------------------------

HC_models_P_p_vals <- extractPVals(HC_models_P)

HC_models_P_p_vals

HC_P_p_vals <- subset(HC_models_P_p_vals, 
                      select = c("location", "climatic_var", "p_val_0P_0", "p_val_1P_1", 
                                 "p_val_2P_2", "p_val_3P_3", "p_val_cP_c"))
HC_P_p_vals

# Isolating Significant P-Values 

HC_P_sig_p_vals <- removeNonSigPVals(HC_P_p_vals)

HC_P_sig_p_vals


write.csv(HC_P_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Cover_pvals_Plot.csv")), 
          row.names = FALSE)

write.csv(HC_P_sig_p_vals, file = here("Data/05_Output", paste0(Sys.Date(), "_ln_Height_Cover_sig_pvals_Plot.csv")), 
          row.names = FALSE)

