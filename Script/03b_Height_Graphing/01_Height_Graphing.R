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



# 5. emmeans ---------------------------------------------------------------------

library(emmeans)

mod <- harvest_2a_models$model_2a[[1]]

mod_2 <- ln_height_harvest_models$model_h[[1]]

mod


plots.emm.RH <- emmeans(mod, ~ harvestF, 
                     lmerTest.limit = 5809, pbkrtest.limit = 5809)
pairs(plots.emm.RH, adjust="bonferroni", side="two-sided")


plots.emm <- emmeans(mod_2, ~ harvestF)
pairs(plots.emm, adjust="bonferroni", side="two-sided")
