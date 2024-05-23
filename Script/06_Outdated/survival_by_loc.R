# Survival -------------------------------------

# Importing Data -------------------------------

regen <- read.csv(here("Data/Processed", "survival_fd_b.csv"), header = TRUE)

# Importing Functions -------------------------

source("Script/Functions/Model_Functions.R")


# Correcting Variable types -----------------------------

# Variables as a factor
regen$survival    <- as.factor(regen$survival)
regen$locationF   <- as.factor(regen$locationF)
regen$blockF      <- as.factor(regen$blockF)
regen$plotF       <- as.factor(regen$plotF)
regen$splitplotF  <- as.factor(regen$splitplotF)
regen$harvestF    <- as.factor(regen$harvestF)
regen$provenanceF <- as.factor(regen$provenanceF)

# normalizing climate variables
regen <- regen %>%
  mutate(across(starts_with("d_"), scale))

# blockF is not a significant variables - re-valuing splitplotF <- need to figure out if this a valud method 
# Until then keep old splitplot variable (Thomson - 19/09/2023)

regen$splitplotNo <- (regen$locationNo * 10000) + (regen$block * 100) +
  (regen$harvestNo * 10) + (regen$provenanceNo)

regen$splitplotF <- as.factor(regen$splitplotNo)

# Grouping Data -------------------------------

loc_group <- regen %>% 
  group_by(location) %>% 
  nest()

# Null model ---------------------------
model.0 <- function(df) {
  glmer(survival ~ 1 + (1|plotF/splitplotF), data = df,
        family = binomial)
}

models.0 <- loc_group %>% 
  mutate(model = map(data, model.0))

models.0$model


# Models ---------------------------------
model.1 <- function(df) {
  glmer(survival ~ d_AHM + (1|plotF/splitplotF), data = df,
        family = binomial, 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}

models.1 <- loc_group %>% 
  mutate(model = map(data, model.1))

models.1$model


# + d_MAT + harvestF
model.2 <- function(df) {
  glmer(survival ~ d_AHM + harvestF  + (1|plotF/splitplotF), data = df,
        family = binomial,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}

models.2 <- loc_group %>% 
  mutate(model = map(data, model.2))
models.2$model


# + d_MAT * harvestF
model.3 <- function(df) {
  glmer(survival ~ d_AHM + harvestF + d_AHM*harvestF + (1|plotF/splitplotF), data = df,
        family = binomial, 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
}

models.3 <- loc_group %>% 
  mutate(model = map(data, model.3))
models.3$model



# Functions -----------------------------------------
# function to test models
lrtest_function <- function(test_models) {
  lrtest(test_models$model.x[[1]], test_models$model.y[[1]]) %>%
    nest()
}

# function to iterate test across all models
models_test <- function (){
  
  models_df <- model_join()
  
  for (i in 1:nrow(models_df)) {
    
    test_models <- c(models_df[i, 3], models_df[i, 5])
    models_df$lr_test[i] <- lrtest_function(test_models)
  
  }

  models_df$lr_test

}

# function to join model dataframes
# needs to be modified for each run 
# model_join <- function() {
  
  # models_df <- left_join(models.0, models.1, by ='location')
  # models_df$lr_test <- NA
  
  # models_df
# } 


# Testing Models ------------------------------------


# joining and testing models of increasing complexity
model_join <- function() {
  
      models_df <- left_join(models.0, models.1, by ='location')
      models_df$lr_test <- NA
  
    models_df
  }

  test_0_1 <- unlist(models_test(), recursive = FALSE)

model_join <- function() {
  
       models_df <- left_join(models.1, models.2, by ='location')
       models_df$lr_test <- NA
  
   models_df
  }

 test_1_2 <- unlist(models_test(), recursive = FALSE)

model_join <- function() {
  
       models_df <- left_join(models.2, models.3, by ='location')
       models_df$lr_test <- NA
  
     models_df
  }

 test_2_3 <- unlist(models_test(), recursive = FALSE)
 
model_join <- function() {
   
   models_df <- left_join(models.1, models.3, by ='location')
   models_df$lr_test <- NA
   
   models_df
 }
 
 test_1_3 <- unlist(models_test(), recursive = FALSE)

# storing test outputs as a tibble
lrtest_tibble <- tibble(locations = c("Alex Fraser", "Jaffray", "John Prince", 
                                 "Narrows", "Redfish", "Twobit"), test_0_1, test_1_2, test_2_3, test_1_3)
lrtest_tibble


# Extracting p-values from models -----------------------

# extract values for test one
p_val_extract <- function() {
  
  # create empty data frame
  p_val_df <- data.frame(matrix(NA,   
                                nrow = 6,
                                ncol = 5))
  
  # Name empty data frame
  names(p_val_df) <- c("location", "p_0_1", "p_1_2", "p_2_3", "p_1_3")
  
  p_val_df$location <- c("Alex Fraser", "Jaffray", "John Prince", 
                         "Narrows", "Redfish", "Twobit")
  
  # Populate the data frame
  for (i in 1:nrow(lrtest_tibble)) {
    p_values <- c(lrtest_tibble[[2]][[i]][["Pr(>Chisq)"]][[2]])
    p_val_df$p_0_1[i] <- p_values
  }
  
  for (i in 1:nrow(lrtest_tibble)) {
    p_values <- c(lrtest_tibble[[3]][[i]][["Pr(>Chisq)"]][[2]])
    p_val_df$p_1_2[i] <- p_values
  }
  
  for (i in 1:nrow(lrtest_tibble)) {
    p_values <- c(lrtest_tibble[[4]][[i]][["Pr(>Chisq)"]][[2]])
    p_val_df$p_2_3[i] <- p_values
  }
  
  for (i in 1:nrow(lrtest_tibble)) {
    p_values <- c(lrtest_tibble[[5]][[i]][["Pr(>Chisq)"]][[2]])
    p_val_df$p_1_3[i] <- p_values
  }
  
  p_val_df
}


DD1040_p_vals <- p_val_extract()

DD1040_p_vals


# Saving p-values --------------------------------------------- 

data_list <- list(AHM_p_vals, CMD_p_vals, CMI_p_vals, DD_0_p_vals, DD_18_p_vals, DD1040_p_vals, DD18_p_vals, DD5_p_vals, 
                  EMT_p_vals, Eref_p_vals, EXT_p_vals, FFP_p_vals, MAP_p_vals, MAT_p_vals, MCMT_p_vals, MSP_p_vals, 
                MWMT_p_vals, NFFD_p_vals, PAS_p_vals, RH_p_vals, SHM_p_vals, TD_p_vals) 

data_names <- c("AHM_p_vals", "CMD_p_vals", "CMI_p_vals", "DD_0_p_vals", "DD_18_p_vals", "DD1040_p_vals", "DD18_p_vals", "DD5_p_vals", 
                  "EMT_p_vals", "Eref_p_vals", "EXT_p_vals", "FFP_p_vals", "MAP_p_vals", "MAT_p_vals", "MCMT_p_vals", "MSP_p_vals", 
                  "MWMT_p_vals", "NFFD_p_vals", "PAS_p_vals", "RH_p_vals", "SHM_p_vals", "TD_p_vals") 
data_names

names(data_list) <- data_names # naming data fram list

data_list # check for correct naming 

for(i in names(data_list)){
  write.csv(data_list[[i]], paste0(i,".csv"))
}
