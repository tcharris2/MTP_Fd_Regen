# Survival -------------------------------------

rm(list=ls(all=TRUE)) #remove any objects left from previous R runs

# Importing Data -------------------------------

regen <- read.csv("survival_fd_b.csv", header = TRUE)

# Getting Packages --------------------------

library(modelr)
library(tidyverse)
library(lmtest)
library(lme4)
library(broom.mixed)
library(dplyr)

# Cleaning Data Set -----------------------------

# need all levels to have >1 sample
# issue for blocks (twobit)

# survival as a factor
regen$survival <- as.factor(regen$survival)

# Other variables as factors 
regen$locationF <- as.factor(regen$locationF)
regen$blockF <- as.factor(regen$blockF)
regen$plotF <- as.factor(regen$plotF)
regen$splitplotF <- as.factor(regen$splitplotF)
regen$harvestF <- as.factor(regen$harvestF)
regen$provenanceF <- as.factor(regen$provenanceF)

# remove provenance and site climate data
regen <- regen %>% select(-contains(c("p_", "s_")))

# normalizing climate variables
regen <- regen %>%
  mutate(across(starts_with("d_"), scale))

# which locations have 1 block

unique(loc_group[[2]][[1]][["blockF"]])
unique(loc_group[[2]][[2]][["blockF"]])
unique(loc_group[[2]][[3]][["blockF"]])
unique(loc_group[[2]][[4]][["blockF"]]) # has 1 block
unique(loc_group[[2]][[5]][["blockF"]])
unique(loc_group[[2]][[6]][["blockF"]]) # has 1 block 

# Grouping Data -------------------------------

loc_group <- regen %>% 
  group_by(location) %>% 
  nest()

loc_group.1 <- subset(loc_group, location != "Twobit" & location != "Narrows")

loc_group.2 <- subset(loc_group, location %in% c("Twobit", "Narrows"))

# Two datafarmes containing the locations with and without multiple blocks

# Testing if the use of blockF is appropriate ---------------------------
# full model
model.1 <- function(df) {
  glmer(survival ~ 1 + (1|blockF/plotF/splitplotF), data = df,
        family = binomial)
}

models.1 <- loc_group.1 %>% 
  mutate(model = map(data, model.1))
models.1

# reduced model
model.0 <- function(df) {
  glmer(survival ~ 1 + (1|plotF/splitplotF), data = df,
        family = binomial)
}

models.0 <- loc_group.1 %>% 
  mutate(model = map(data, model.0))
models.0

# + harvestF 
model.ha <- function(df) {
  glmer(survival ~ harvestF + (1|blockF/plotF/splitplotF), data = df,
        family = binomial)
}

models.ha <- loc_group %>% 
  mutate(model = map(data, model.ha))
models.ha$model

# 3 functions that are used for testing models
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
  
  names(models_df$lr_test) <- c("Alex Fraser", "Jaffray", "John Prince", 
                                "Redfish")
  models_df$lr_test
  
}

# Save lrtest outputs ------------------------------


# joining and testing models of increasing complexity
model_join <- function() {
  
  models_df <- left_join(models.0, models.1, by ='location')
  models_df$lr_test <- NA
  
  models_df
}

test_0_1 <- unlist(models_test(), recursive = FALSE)

# storing test outputs as a tibble

lrtest_tibble <- tibble(locations = c("Alex Fraser", "Jaffray", "John Prince", 
                                      "Redfish"), test_0_1)
lrtest_tibble

# Extracting p-values from models -----------------------
p_value <- lrtest_tibble[[2]][[1]][["Pr(>Chisq)"]][[2]]
p_value
# works to extract the p value from the tibble

# extracting p-values for the test 
 
p_val_extract <- function() {
  
  # create empty data frame
  p_val_df <- data.frame(matrix(NA,   
                                nrow = 4,
                                ncol = 2))
  
  # Name empty data frame
  names(p_val_df) <- c("location", "p_0_1")
  
  p_val_df$location <- c("Alex Fraser", "Jaffray", "John Prince", 
                         "Redfish")
  
  # Populate the data frame
  for (i in 1:nrow(lrtest_tibble)) {
    p_values <- c(lrtest_tibble[[2]][[i]][["Pr(>Chisq)"]][[2]])
    p_val_df$p_0_1[i] <- p_values
  }
  
  p_val_df
}
block_p_vals <- p_val_extract()
block_p_vals
# all p-values are > 0.05 showing blockF is not a significant random effect 
# and therefor can be dropped. 
