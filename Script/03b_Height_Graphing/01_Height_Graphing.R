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

library(ggpubr)

### 1.2. Correcting Variable types -----

# Universal prep
regen_prepped <- universalDataPrepFunction(regen)


# Height specific prep
regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_height <-  subset(regen_prepped, !(is.na(height)))

regen_height <- subset(regen_height, !(is.na(tree_cover)))

regen_height$sqrt_tree_cover <- sqrt(regen_height$tree_cover)

# Removing Futures
regen_height <- subset(regen_height, !regen_height$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                                     "Peterhope future Fd", "Alex Fraser future Fd", 
                                                                     "Twobit B class Fd"))

# for the models to read
df <- regen_height

str(regen_height)


### 1.3. Calling RDS files -----

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                      "2024-03-11_ln_height_group_harvest_models.rds"))

ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                                "2024-03-11_ln_height_group_cover_models.rds"))


names(ln_height_harvest_models)


names(ln_height_cover_models)


# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(ln_height_harvest_models) <- c("model_0", "model_h", "model_a", "model_ah",
                                        "model_1", "model_1a", "model_2", "model_2a", 
                                        "model_3", "model_3a", 
                                        "ClimaticVarList")

colnames(ln_height_cover_models) <- c("model_0", "model_c", "model_a", "model_ac",
                                      "model_1", "model_1a", "model_2", "model_2a", 
                                      "model_3", "model_3a", 
                                      "ClimaticVarList")


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

### 4.1 Cover 3a -----

# Predictions 
cover_3a_models

# MWMT Plot
MWMT_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[1]],
                                   type = "pred", 
                                   terms = c("d_MWMT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models$data[[1]],
              mapping = aes(x = d_MWMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.03,
              size = 0.1, colour = "gray20") +
  
  labs(x = "MWMT Climatic Distance", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MWMT_3C_plot

# MAP Plot
MAP_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[2]],
                                   type = "pred", 
                                   terms = c("d_MAP [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models$data[[2]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 3,
              size = 0.1, colour = "gray20") +
  
  labs(x = "MAP Climatic Distance", 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MAP_3C_plot

# NFFD Plot
NFFD_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[3]],
                                   type = "pred", 
                                   terms = c("d_NFFD [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models$data[[3]],
              mapping = aes(x = d_NFFD, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.7,
              size = 0.1, colour = "gray20") +
  
  labs(x = "NFFD Climatic Distance", 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

NFFD_3C_plot

# EMT Plot
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

# RH Plot
RH_3C_plot <- sjPlot::plot_model(cover_3a_models[["model_3a"]][[5]],
                                   type = "pred", 
                                   terms = c("d_RH [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models$data[[5]],
              mapping = aes(x = d_RH, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.1,
              size = 0.1, colour = "gray20") +
  
  labs(x = "RH Climatic Distance", 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

RH_3C_plot


##### 4.1.2 Composite ----
ggarrange(MWMT_3C_plot, MAP_3C_plot, NFFD_3C_plot,
          EMT_3C_plot, RH_3C_plot,
          labels = c("A", "B", "C",
                     "D", "E"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


ggarrange(MAP_3C_plot,EMT_3C_plot, RH_3C_plot,
          labels = c("A", "B", "C"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")

### 4.2 Cover 3a Weak ------

# Predictions 
cover_3a_models_weak

# MAT Plot
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


# MCMT Plot
MCMT_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[2]],
                                   type = "pred", 
                                   terms = c("d_MCMT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[2]],
              mapping = aes(x = d_MCMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.05,
              size = 0.1, colour = "gray20") +
  
  labs(x = "MCMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MCMT_3C_plot

# SHM Plot
SHM_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[3]],
                                   type = "pred", 
                                   terms = c("d_SHM [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[3]],
              mapping = aes(x = d_SHM, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.17,
              size = 0.1, colour = "gray20") +
  
  labs(x = "SHM Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

SHM_3C_plot

# FFP Plot
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

# EXT Plot
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

# Eref Plot
Eref_3C_plot <- sjPlot::plot_model(cover_3a_models_weak[["model_3a"]][[6]],
                                   type = "pred", 
                                   terms = c("d_Eref [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent \nTree Cover (%)") +
  
  geom_jitter(data = cover_3a_models_weak$data[[6]],
              mapping = aes(x = d_Eref, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 1,
              size = 0.1, colour = "gray20") +
  
  labs(x = "Eref Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

Eref_3C_plot


##### 4.2.2 Composite ----
ggarrange(MAT_3C_plot, MCMT_3C_plot, SHM_3C_plot,
          FFP_3C_plot, EXT_3C_plot, Eref_3C_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")

# 5. Harvest Models ------------------------------------------------------------

# MAP Plot
MAP_2H_plot <- sjPlot::plot_model(harvest_2a_models[["model_2a"]][[1]],
                                   type = "pred", 
                                   terms = c("d_MAP [all]", "harvestF"),
                                   legend.title = "Harvest",
                                   line.size = 1) +
  
  geom_jitter(data = harvest_2a_models$data[[1]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 1,
              size = 0.1, colour = "gray60") +
  
  labs(x = "MAP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

MAP_2H_plot


library(ggeffects)

# ggeffects ploting
# d_MAP
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

# d_MSP
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

# d_AHM
df_AHM <- ggpredict(harvest_2a_models[["model_2a"]][[3]], terms = c("d_AHM [all]", "harvestF"))

ggplot(df_AHM, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[3]],
              AHMping = aes(x = d_AHM, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
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
        legend.title = element_blank())

# d_NFFD
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

# d_PAS
df_PAS <- ggpredict(harvest_2a_models[["model_2a"]][[5]], terms = c("d_PAS [all]", "harvestF"))

PAS_graph <- ggplot(df_PAS, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2a_models$data[[5]],
              mapping = aes(x = d_PAS, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.8,
              size = 0.1, colour = "gray50") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.1) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Precipitation as Snow Distance (mm)", 
       y = "Predicted Height (cm)",
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

# d_EMT
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

# d_RH
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





# 6. emmeans ---------------------------------------------------------------------

library(emmeans)


df <- regen_height


mod <- ln_height_harvest_models$model_ah[[1]]

mod <- harvest_2a_models$model_2a[[5]]

# Regrid emmeans

logemm.src <- regrid(emmeans(mod, "harvestF",
                             lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                     transform = "log")
confint(logemm.src, type = "response")
pairs(logemm.src,  adjust="bonferroni", side="two-sided", type = "response")


harvest_labels <- c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")
sig_labels <- c("AA", "AA", "AB", "BB")

PAS_means <- plot(regrid(logemm.src), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = sig_labels), hjust = 1.5, vjust = -4, size = 3) +
  geom_text(aes(label = round(exp(logemm.src@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


ggarrange(PAS_graph, PAS_means, nrow = 2, heights = c(2.5, 1))


# 7. Beta Coefficients ---------------------------------------------------------

model_0 <- lmer(log(height) ~ scale(d_RH) + harvestF + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_1 <- lmer(log(height) ~ d_RH + harvestF + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_0
model_1

sjPlot::plot_model(model_0, terms = c("d_RH [all]", "harvestF"), type = "pred")
sjPlot::plot_model(model_1, terms = c("d_RH [all]", "harvestF"), type = "pred")


# 8. Three way interaction ------------------------------------------------------
# Variation too high due to not a large enough breadth of climatic distances 

library(ggeffects)
library(emmeans)

model_0 <- lmer(log(height) ~ scale(d_RH) * harvestF + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))



model_1 <- lmer(log(height) ~ scale(d_RH) * harvestF + locationF + (1|blockF/plotF/splitplotF), 
              data = regen_height, REML = FALSE, 
              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))



model_2 <- lmer(log(height) ~ scale(d_RH) * harvestF * locationF + (1|blockF/plotF/splitplotF), 
              data = regen_height, REML = FALSE, 
              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


model_3 <- lmer(log(height) ~ scale(d_RH) * locationF + (1|blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


model_0
model_1
model_2
model_3

lrtest(model_0, model_1)
lrtest(model_1, model_2)
lrtest(model_2, model_3)

df <- ggpredict(model_0, terms = c("d_RH [all]", "harvestF"), terms_to_colnames = TRUE)
df


ggplot(df, aes(x, predicted)) +
  geom_line(aes(color = group)) 


df_0 <- ggpredict(model_0, terms = c("d_RH [all]", "harvestF", "locationF"), terms_to_colnames = TRUE, type = "random")
df_0


ggplot(df_0, aes(x, predicted)) +
  geom_line(aes(color = group)) +
  facet_wrap(~ facet)


df_1 <- ggpredict(model_1, terms = c("d_RH [all]", "harvestF", "locationF"), terms_to_colnames = TRUE, type = "random")
df_1


ggplot(df_1, aes(x, predicted)) +
  geom_line(aes(color = group)) +
  facet_wrap(~ facet)


df_2 <- ggpredict(model_2, terms = c("d_RH [all]", "harvestF", "locationF"), terms_to_colnames = TRUE, type = "random")
df_2

plot(df_2, show_ci = FALSE)
plot(df_2)


ggplot(df_2, aes(x, predicted)) +
  geom_line(aes(color = group)) +
  facet_wrap(~ facet)



df_3 <- ggpredict(model_3, terms = c("d_RH [all]", "locationF"), terms_to_colnames = TRUE, type = "random")

plot(df_3, show_ci = FALSE) +
  geom_point(data = regen_height, 
             aes(x = d_RH, y = height, colour = locationF), inherit.aes = FALSE)
plot(df_3)

emmip(model_2, harvestF ~ d_RH | locationF, mult.name = "variety", cov.reduce = FALSE,
      glmerTest.limit = 5809, pbkrtest.limit = 5809)

emtrends(model_3, pairwise ~ locationF, var = "d_RH", mult.name = "variety")
