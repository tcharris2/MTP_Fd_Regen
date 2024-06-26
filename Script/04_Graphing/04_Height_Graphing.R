# CONTENT: Graphing - Height ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), header = TRUE) 
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

df <- regen_height

### 1.5 Calling RDS files -----

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                      "2024-06-24_ln_height_harvest_models.rds"))

ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                                "2024-06-24_ln_height_cover_models.rds"))


names(ln_height_harvest_models)

names(ln_height_cover_models)


# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models


harvest_2_models <- ln_height_harvest_models[c(2:7, 9), c("ClimaticVarList", "model_2")]

cover_2_models <- ln_height_cover_models[c(4:5, 6), c("ClimaticVarList", "model_2")]

cover_3_models <- ln_height_cover_models[c(1:2, 5, 7:9), c("ClimaticVarList", "model_3")]


# Cleaning up 
rm(ln_height_cover_models, ln_height_harvest_models, regen_prepped, universalDataPrepFunction)

# 3. Adding Predicted Values -------------------------------------------------

##### 3.1 Adding data -----

harvest_2_models$data <- list(regen_height)

harvest_2_models

cover_2_models$data <- list(regen_height)

cover_2_models

cover_3_models$data <- list(regen_height)

cover_3_models


### 3.2 Adding predictions -----

heightPredictions <- function (df, model) {
  
  for (i in 1:nrow(df)) {
    
    VEC <- predict(model[[i]])
    
    df[["data"]][[i]]$predict_val <- VEC
    
  }
  
  return(df)
  
}


harvest_2_models <- heightPredictions(harvest_2_models, harvest_2_models$model_2)

cover_2_models <- heightPredictions(cover_2_models, cover_2_models$model_2)

cover_3_models <- heightPredictions(cover_3_models, cover_3_models$model_3)


# 4. Cover Models -------------------------------------------------------
# save as "A4" .pdf
# Predictions 
cover_3_models

##### 4.1.1 MAT Plot ----
MAT_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[1]],
                                  type = "pred", 
                                  terms = c("d_MAT [all]", "tree_cover [0, 10, 30, 60]"),
                                  legend.title = "   Percent Tree Cover (%)", 
                                  alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[1]],
              mapping = aes(x = d_MAT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.03,
              size = 0.1, colour = "gray20", alpha = 0.5) +
  
  labs(x = bquote(bold("Mean Annual Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

MAT_3C_plot


##### 4.1.2 MAP Plot ----
MAP_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[2]],
                                   type = "pred", 
                                   terms = c("d_MAP [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent Tree Cover (%)", 
                                  alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[2]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 3,
              size = 0.1, colour = "gray20", alpha = 0.5) +
  
  labs(x = "Mean Annual Precipitation Transfer Distance (mm)", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

MAP_3C_plot

##### 4.1.3 NFFD Plot ----
NFFD_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[3]],
                                   type = "pred", 
                                   terms = c("d_NFFD [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent Tree Cover (%)",
                                   alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[3]],
              mapping = aes(x = d_NFFD, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.3,
              size = 0.1, colour = "gray20", alpha = 0.5) +
  
  labs(x = "Number of Frost Free Days Transfer Distance (days)", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

NFFD_3C_plot

##### 4.1.4 EMT Plot ----
EMT_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[4]],
                                   type = "pred", 
                                   terms = c("d_EMT [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent Tree Cover (%)",
                                  alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[4]],
              mapping = aes(x = d_EMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.1,
              size = 0.1, colour = "gray20") +
  
  labs(x =  bquote(bold("Extreme Minimum Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

EMT_3C_plot

##### 4.1.5 EXT Plot ----
EXT_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[5]],
                                  type = "pred", 
                                  terms = c("d_EXT [all]", "tree_cover [0, 10, 30, 60]"),
                                  legend.title = "   Percent Tree Cover (%)", 
                                  alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[5]],
              mapping = aes(x = d_EXT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.03,
              size = 0.1, colour = "gray20", alpha = 0.5) +
  
  labs(x =  bquote(bold("Extreme Maximum Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

EXT_3C_plot


##### 4.1.6 RH Plot ----
RH_3C_plot <- sjPlot::plot_model(cover_3_models[["model_3"]][[6]],
                                   type = "pred", 
                                   terms = c("d_RH [all]", "tree_cover [0, 10, 30, 60]"),
                                   legend.title = "   Percent Tree Cover (%)",
                                 alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = cover_3_models$data[[6]],
              mapping = aes(x = d_RH, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.05,
              size = 0.1, colour = "gray20", alpha = 0.5) +
  
  labs(x = "Mean Annual Relative Humidity Transfer Distance (%)", 
       y = "Predicted Height (cm)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

RH_3C_plot


##### 4.2 Composites ----

ggarrange(MAP_3C_plot, NFFD_3C_plot, RH_3C_plot,
          labels = c("A", "B", "C"),
          hjust = -1, 
          ncol = 1,
          common.legend = TRUE, legend = "top")

# save as "US Legal" .pdf

# 5. Harvest Models ------------------------------------------------------------
# ggeffects ploting
# Paired with "6. emmeans" to create composites 
harvest_2_models

###### 5.1 MAP plot ----
df_MAP <- ggpredict(harvest_2_models[["model_2"]][[1]], terms = c("d_MAP [all]", "harvestF"))

MAP_graph <- ggplot(df_MAP, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[1]],
              mapping = aes(x = d_MAP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 2,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
                  alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Mean Annual Precipitation Transfer Distance (mm)", 
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
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times", size = 17))

MAP_graph

###### 5.2 MSP plot ----
df_MSP <- ggpredict(harvest_2_models[["model_2"]][[2]], terms = c("d_MSP [all]", "harvestF"))

MSP_graph <- ggplot(df_MSP, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[2]],
              mapping = aes(x = d_MSP, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.3,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Mean Summer Precipitation Transfer Distance (mm)", 
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
        text = element_text(family = "Times", size = 17))

MSP_graph

###### 5.3 AHM plot ----
df_AHM <- ggpredict(harvest_2_models[["model_2"]][[3]], terms = c("d_AHM [all]", "harvestF"))

AHM_graph <- ggplot(df_AHM, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[3]],
              mapping = aes(x = d_AHM, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.07,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Annual Heat Moisture Index Transfer Distance", 
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
        text = element_text(family = "Times", size = 17))

AHM_graph

###### 5.4 NFFD plot ----
df_NFFD <- ggpredict(harvest_2_models[["model_2"]][[4]], terms = c("d_NFFD [all]", "harvestF"))

NFFD_graph <- ggplot(df_NFFD, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[4]],
              mapping = aes(x = d_NFFD, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.3,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Number of Frost Free Days Transfer Distance (days)", 
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
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times", size = 17))

NFFD_graph

###### 5.5 PAS plot----

df_PAS <- ggpredict(harvest_2_models[["model_2"]][[5]], terms = c("d_PAS [all]", "harvestF"))

PAS_graph <- ggplot(df_PAS, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[5]],
              mapping = aes(x = d_PAS, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.8,
              size = 0.1, colour = "gray20") +
  
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
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times", size = 17))

PAS_graph

###### 5.6 EMT plot ----
df_EMT <- ggpredict(harvest_2_models[["model_2"]][[6]], terms = c("d_EMT [all]", "harvestF"))

EMT_graph <- ggplot(df_EMT, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[6]],
              mapping = aes(x = d_EMT, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.05,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = bquote(bold("Extreme Minimum Temperature Transfer Distance (" ^"o" * "C)")), 
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
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times", size = 17))

EMT_graph

###### 5.7 RH plot ----
df_RH <- ggpredict(harvest_2_models[["model_2"]][[7]], terms = c("d_RH [all]", "harvestF"))

RH_graph <- ggplot(df_RH, aes(x, predicted)) +
  
  geom_jitter(data = harvest_2_models$data[[7]],
              mapping = aes(x = d_RH, y = exp(predict_val)),
              inherit.aes = FALSE,
              height = 0.5,
              width = 0.05,
              size = 0.1, colour = "gray20") +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.05) +
  
  geom_line(aes(color = group), linewidth = 1) +
  
  labs(x = "Mean Annual Relative Humidity Transfer Distance (%)", 
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
        text = element_text(family = "Times", size = 17))

RH_graph

# 6. emmeans --------------------------------------------------------------------

# Labels
harvest_labels <- c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")

###### 6.1 MAP ----

# Labels
MAP_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
MAP_mod <- #path to MAP model

# Regrid emmeans
MAP_regird <- regrid(emmeans(MAP_mod, "harvestF",
                             lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                     transform = "log")
confint(MAP_regird, type = "response")
pairs(MAP_regird,  adjust="bonferroni", side="two-sided", type = "response")

MAP_means <- plot(regrid(MAP_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = MAP_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(MAP_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

MAP_means

###### 6.2 MSP ----

# Labels
MSP_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
MSP_mod <- #path to MSP model
  
  # Regrid emmeans
  MSP_regird <- regrid(emmeans(MSP_mod, "harvestF",
                               lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                       transform = "log")
confint(MSP_regird, type = "response")
pairs(MSP_regird,  adjust="bonferroni", side="two-sided", type = "response")

MSP_means <- plot(regrid(MSP_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = MSP_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(MSP_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

MSP_means

###### 6.3 AHM ----

# Labels
AHM_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
AHM_mod <- #path to AHM model
  
  # Regrid emmeans
  AHM_regird <- regrid(emmeans(AHM_mod, "harvestF",
                               lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                       transform = "log")
confint(AHM_regird, type = "response")
pairs(AHM_regird,  adjust="bonferroni", side="two-sided", type = "response")

AHM_means <- plot(regrid(AHM_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = AHM_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(AHM_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

AHM_means

###### 6.4 NFFD ----

# Labels
NFFD_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
NFFD_mod <- #path to NFFD model
  
  # Regrid emmeans
  NFFD_regird <- regrid(emmeans(NFFD_mod, "harvestF",
                               lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                       transform = "log")
confint(NFFD_regird, type = "response")
pairs(NFFD_regird,  adjust="bonferroni", side="two-sided", type = "response")

NFFD_means <- plot(regrid(NFFD_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = NFFD_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(NFFD_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

NFFD_means

###### 6.5 PAS ----

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

###### 6.6 EMT ----

# Labels
EMT_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
EMT_mod <- #path to EMT model
  
  # Regrid emmeans
  EMT_regird <- regrid(emmeans(EMT_mod, "harvestF",
                               lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                       transform = "log")
confint(EMT_regird, type = "response")
pairs(EMT_regird,  adjust="bonferroni", side="two-sided", type = "response")

EMT_means <- plot(regrid(EMT_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = EMT_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(EMT_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

EMT_means

###### 6.7 RH ----

# Labels
RH_sig_labels <- c("AA", "AA", "AB", "BB")

# Models
RH_mod <- #path to RH model
  
  # Regrid emmeans
  RH_regird <- regrid(emmeans(RH_mod, "harvestF",
                               lmerTest.limit = 5809, pbkrtest.limit = 5809), 
                       transform = "log")
confint(RH_regird, type = "response")
pairs(RH_regird,  adjust="bonferroni", side="two-sided", type = "response")

RH_means <- plot(regrid(RH_regird), transform = "log") +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = RH_sig_labels), hjust = 1.5, vjust = -5, size = 3) +
  geom_text(aes(label = round(exp(RH_regird@bhat), 1)), hjust = -0.5) +
  
  labs(x = "Height (cm)", 
       y = "Harvest Type",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))

RH_means


### 7. Harvest Composites -----

# MAP
ggarrange(MAP_graph, MAP_means, nrow = 2, heights = c(2.5, 1))

# MSP
ggarrange(MSP_graph, MSP_means, nrow = 2, heights = c(2.5, 1))

# AHM
ggarrange(AHM_graph, AHM_means, nrow = 2, heights = c(2.5, 1))

# NFFD
ggarrange(NFFD_graph, NFFD_means, nrow = 2, heights = c(2.5, 1))

# PAS
ggarrange(PAS_graph, PAS_means, nrow = 2, heights = c(2.5, 1))

# EMT
ggarrange(EMT_graph, EMT_means, nrow = 2, heights = c(2.5, 1))

# RH
ggarrange(RH_graph, RH_means, nrow = 2, heights = c(2.5, 1))



# 8. Marginal/Conditional R2 --------------------------------------------

# using an extremely low tolerance due to model singularity
# values are the same for model without the singularity.

### 8.1 Cover values ----

# MAT
performance::r2_nakagawa(ln_height_cover_models$model_3[[1]], tolerance = 1e-1000)

# MAP
performance::r2_nakagawa(ln_height_cover_models$model_3[[2]], tolerance = 1e-1000)

# NFFD
performance::r2_nakagawa(ln_height_cover_models$model_3[[3]], tolerance = 1e-1000)

# EMT
performance::r2_nakagawa(ln_height_cover_models$model_3[[4]], tolerance = 1e-1000)

# EXT
performance::r2_nakagawa(ln_height_cover_models$model_3[[5]], tolerance = 1e-1000)

# RH
performance::r2_nakagawa(ln_height_cover_models$model_3[[6]], tolerance = 1e-1000)


### 8.2 harvest values ----
# MAP
performance::r2_nakagawa(ln_height_cover_models$model_2[[1]], tolerance = 1e-1000)

# MSP
performance::r2_nakagawa(ln_height_cover_models$model_2[[2]], tolerance = 1e-1000)

# AHM
performance::r2_nakagawa(ln_height_cover_models$model_2[[3]], tolerance = 1e-1000)

# NFFD
performance::r2_nakagawa(ln_height_cover_models$model_2[[4]], tolerance = 1e-1000)

# PAS
performance::r2_nakagawa(ln_height_cover_models$model_2[[5]], tolerance = 1e-1000)

# EMT
performance::r2_nakagawa(ln_height_cover_models$model_2[[6]], tolerance = 1e-1000)

# RH
performance::r2_nakagawa(ln_height_cover_models$model_2[[7]], tolerance = 1e-1000)
