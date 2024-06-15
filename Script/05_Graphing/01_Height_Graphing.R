# CONTENT: Graphing - Height ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

### 1.1 Importing Functions -----

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

### 1.2 Loading Packages ----
library(ggeffects)
library(ggpubr)
library(emmeans)
library(performance)

### 1.4 Height specific prep ----

# Universal prep
regen_prepped <- universalDataPrepFunction(regen)

# This function converts survival, harvestF, provenanceF, and 
# all the random effects into factors. 
# It also normalizes all the climatic distance variables. 

# Remove outliers
regen_height <- subset(regen_prepped, !regen_prepped$tree_number %in% 
                         c(3904, 9861, 8248, 12846, 13432, 14752))

# Remove NAs
regen_height <-  subset(regen_height, !(is.na(height)))


str(regen_height)

### 1.5 Calling RDS files -----

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                      "2024-03-11_ln_height_group_harvest_models.rds"))

ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                                "2024-03-11_ln_height_group_cover_models.rds"))


names(ln_height_harvest_models)

names(ln_height_cover_models)


# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models


harvest_2a_models <- ln_height_harvest_models[c(2, 4:6, 8:11, 14:15), c("ClimaticVarList", "model_2a")]

harvest_2a_models <- harvest_2a_models[c(2:5, 7:8, 10), ]

cover_2a_models <- ln_height_cover_models[c(5:6, 10, 14), c("ClimaticVarList", "model_2a")]

cover_3a_models <- ln_height_cover_models[c(2, 4, 8, 11, 15), c("ClimaticVarList", "model_3a")]

cover_3a_models_weak <- ln_height_cover_models[c(1, 3, 7, 9, 12:13), c("ClimaticVarList", "model_3a")]

# Cleaning up 
rm(ln_height_cover_models, ln_height_harvest_models, regen_prepped, universalDataPrepFunction)

# 3. Adding Predicted Values -------------------------------------------------

##### 3.1 Adding data -----

harvest_2a_models$data <- list(regen_height)

harvest_2a_models

cover_2a_models$data <- list(regen_height)

cover_2a_models


cover_3a_models$data <- list(regen_height)

cover_3a_models

cover_3a_models_weak$data <- list(regen_height)

cover_3a_models_weak


### 3.2 Adding predictions -----

heightPredictions <- function (df, model) {
  
  for (i in 1:nrow(df)) {
    
    VEC <- predict(model[[i]])
    
    df[["data"]][[i]]$predict_val <- VEC
    
  }
  
  return(df)
  
}


harvest_2a_models <- heightPredictions(harvest_2a_models, harvest_2a_models$model_2a)

cover_2a_models <- heightPredictions(cover_2a_models, cover_2a_models$model_2a)

cover_3a_models <- heightPredictions(cover_3a_models, cover_3a_models$model_3a)

cover_3a_models_weak <- heightPredictions(cover_3a_models_weak, cover_3a_models_weak$model_3a)


# 4. Cover Models -------------------------------------------------------

# Predictions 
cover_3a_models



##### 4.1.2 MAP Plot ----
MAP_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[2]],
                                   type = "pred", 
                                   terms = c("d_MAP [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)", 
                                  alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3a_models$data[[2]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 3,
              size = 0.1, colour = "gray30", alpha = 0.5) +
  
  labs(x = "MAP Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MAP_3C_plot

##### 4.1.3 NFFD Plot ----
NFFD_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[3]],
                                   type = "pred", 
                                   terms = c("d_NFFD [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)",
                                   alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3a_models$data[[3]],
              mapping = aes(x = d_NFFD, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.3,
              size = 0.1, colour = "gray30", alpha = 0.5) +
  
  labs(x = "NFFD Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

NFFD_3C_plot

##### 4.1.4 EMT Plot ----
EMT_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[4]],
                                   type = "pred", 
                                   terms = c("d_EMT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models$data[[4]],
              mapping = aes(x = d_EMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.1,
              size = 0.1, colour = "gray20") +
  
  labs(x = "EMT Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

EMT_3C_plot

##### 4.1.5 RH Plot ----
RH_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[5]],
                                   type = "pred", 
                                   terms = c("d_RH [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)",
                                 alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3a_models$data[[5]],
              mapping = aes(x = d_RH, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.05,
              size = 0.1, colour = "gray30", alpha = 0.5) +
  
  labs(x = "RH Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

RH_3C_plot


##### 4.2 Composites ----
ggarrange(MWMT_3C_plot, MAP_3C_plot, NFFD_3C_plot,
          EMT_3C_plot, RH_3C_plot,
          labels = c("A", "B", "C",
                     "D", "E"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


ggarrange(MAP_3C_plot, NFFD_3C_plot, RH_3C_plot,
          labels = c("A", "B", "C"),
          hjust = -1, 
          ncol = 1,
          common.legend = TRUE, legend = "top")

### 4.3 Cover 3a Weak ------

# Predictions 
cover_3a_models_weak

####### 4.3.1 MAT Plot ----
MAT_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[1]],
                                   type = "pred", 
                                   terms = c("d_MAT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[1]],
              mapping = aes(x = d_MAT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.03,
              size = 0.1, colour = "gray20") +
  
  labs(x = "MAT Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MAT_3C_plot



####### 4.3.2 FFP Plot ----
FFP_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[4]],
                                   type = "pred", 
                                   terms = c("d_FFP [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[4]],
              mapping = aes(x = d_FFP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.35,
              size = 0.1, colour = "gray20") +
  
  labs(x = "FFP Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

FFP_3C_plot

####### 4.3.3 EXT Plot ----
EXT_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[5]],
                                   type = "pred", 
                                   terms = c("d_EXT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[5]],
              mapping = aes(x = d_EXT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.03,
              size = 0.1, colour = "gray20") +
  
  labs(x = "EXT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

EXT_3C_plot


##### 4.4 Composites ----
ggarrange(MAT_3C_plot,
          FFP_3C_plot, EXT_3C_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")

# 5. Harvest Models ------------------------------------------------------------
# ggeffects ploting
# Paired with "6. emmeans" to create composites 

###### 5.1 MAP plot ----
df_MAP <- ggpredict(harvest_2a_models[["model_2a"]][[1]], terms = c("d_MAP [all]", "harvestF"))

ggplot(df_MAP, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[1]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
                  alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "MAP Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

###### 5.2 MSP plot ----
df_MSP <- ggpredict(harvest_2a_models[["model_2a"]][[2]], terms = c("d_MSP [all]", "harvestF"))

ggplot(df_MSP, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[2]],
              MSPping = aes(x = d_MSP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "MSP Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

###### 5.3 AHM plot ----
df_AHM <- ggpredict(harvest_2a_models[["model_2a"]][[3]], terms = c("d_AHM [all]", "harvestF"))

ggplot(df_AHM, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[4]],
              mapping = aes(x = d_AHM, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "AHM Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times"))

###### 5.4 NFFD plot ----
df_NFFD <- ggpredict(harvest_2a_models[["model_2a"]][[4]], terms = c("d_NFFD [all]", "harvestF"))

ggplot(df_NFFD, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[4]],
              NFFDping = aes(x = d_NFFD, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "NFFD Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

###### 5.5 PAS plot----
PAS_mod <- harvest_2a_models$model_2a[[5]]
PAS_mod

performance::r2_nakagawa(PAS_mod, tolerance = 1e-1000)


tab_model(PAS_mod)

df_PAS <- ggpredict(harvest_2a_models[["model_2a"]][[5]], terms = c("d_PAS [all]", "harvestF"))

PAS_graph <- ggplot(df_PAS, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[5]],
              mapping = aes(x = d_PAS, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.8,
              size = 0.1, colour = "gray30") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Precipitation as Snow Transfer Distance (mm)", 
       y = "Predicted Height (cm)",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "green4", "blue", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times"))

PAS_graph

###### 5.6 EMT plot ----
df_EMT <- ggpredict(harvest_2a_models[["model_2a"]][[6]], terms = c("d_EMT [all]", "harvestF"))

ggplot(df_EMT, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[6]],
              EMTping = aes(x = d_EMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "EMT Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

###### 5.7 RH plot ----
df_RH <- ggpredict(harvest_2a_models[["model_2a"]][[7]], terms = c("d_RH [all]", "harvestF"))

ggplot(df_RH, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[7]],
              mapping = aes(x = d_RH, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray50") +
  
  geom_point(aes(colour = group), size = 2, shape = "triangle") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "RH Climatic Distance", 
       y = "Predicted Height",
       title = NULL,
       fill = "Harvest",
       colour = "Harvest") +
  
  scale_color_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                     values = c("red", "blue", "green4", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"), 
                    values = c("red", "blue", "green4", "black")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())


# 6. emmeans --------------------------------------------------------------------

# Labels
harvest_labels <- c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")

###### 6.1 PAS ----

# Labels
PAS_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
PAS_mod <- #path to PAS model

# Regrid emmeans
PAS_regird <- regrid(emmeans(PAS_mod, "harvestF",
                             lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                     transform = "log")
confint(PAS_regird, type = "response")
pairs(PAS_regird,  adjust="bonferroni", side="two-sided", type = "response")

PAS_means <- plot(regrid(PAS_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = PAS_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(PAS_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

PAS_means

### 6.2 Composites -----
ggarrange(PAS_graph, PAS_means, nrow = 2, heights = c(2.5, 1))



# 7. Marginal/Conditional R2 --------------------------------------------

# using an extremely low tolerance due to model singularity
# values are the same for model without the singularity.

performance::r2_nakagawa(ln_height_cover_models$model_3a[[1]], tolerance = 1e-1000)
