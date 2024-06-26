# CONTENT: Graphing - Survival  ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), 
                  header = TRUE)
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

### 1.1. Importing Functions ----------------------------------------------------------

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")
source("Script/04_Graphing/03a_Survival_graphing_Functions.R")

### 1.2 Loading Packages --------
library(emmeans)
library(ggpubr)
library(ggeffects)

### 1.3. Correcting Variable types ---------------------------------------------------

regen_prepped <- universalDataPrepFunction(regen)

# This function converts survival, harvestF, provenanceF, and 
# all the random effects into factors. 
# It also normalizes all the climatic distance variables. 

regen_survival <- regen_prepped
str(regen_prepped)


### 1.4 Calling RDS file  ------

survival_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-06-24_survival_harvest_models.rds"))

survival_harvest_models


survival_cover_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-06-24_survival_cover_models.rds"))

survival_cover_models


# 2. Selecting Best Models ---------------------------------------------------

names(survival_harvest_models)
names(survival_cover_models)

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models


model_3_H <- survival_harvest_models[c(5, 7, 9), c("ClimaticVarList", "model_3")]


model_3_C <- survival_cover_models[c(1:2, 5, 7:9), c("ClimaticVarList", "model_3")]

# 3. Clean up Global env. ------------------------------------------------------

rm(regen, regen_prepped, universalDataPrepFunction)


# 4. Estimated Probability of Survival -----------------------------------------

##### 4.1 Adding data -----

model_3_H$data <- list(regen_survival)

model_3_H


model_3_C$data <- list(regen_survival)

model_3_C

### 4.2 Adding EPS ----

model_3_H <- survivalProbs(model_3_H, model_3_H$model_3)

model_3_C <- survivalProbs(model_3_C, model_3_C$model_3)



# 5. Harvest Interaction Plots --------------------------------------------------
  
# models
NFFD_mod <- model_3_H$model_3[[1]]
EMT_mod <- model_3_H$model_3[[2]]
RH_mod <- model_3_H$model_3[[3]]


tab_model(NFFD_mod, EMT_mod, RH_mod, transform = NULL)

### 5.1. NFFD plot -----

NFFD_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[1]], type = "pred", 
                   terms = c("d_NFFD [all]", "harvestF"),
                   legend.title = "",
                   alpha = 0.05) + 
  
  scale_colour_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                      values = c("red", "green4", "blue", "black")) +
  
          
  geom_point(data = model_3_H$data[[1]], mapping = aes(x = d_NFFD, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Number of Frost Free Days Transfer Distance (days)", 
       y = "Predicted Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 17),
        text = element_text(family = "Times"))

NFFD_plot

### 5.2. EMT plot -----

EMT_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[2]], type = "pred", 
                                terms = c("d_EMT [all]", "harvestF"),
                                legend.title = "",
                                alpha = 0.05) + 
  
  scale_colour_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                    values = c("red", "green4", "blue", "black")) +
  
  
  geom_point(data = model_3_H$data[[2]], mapping = aes(x = d_EMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = bquote(bold("Extreme Minimum Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 17),
        text = element_text(family = "Times"))

EMT_plot
### 5.3. RH plot -----

RH_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[3]], type = "pred", 
                                terms = c("d_RH [all]", "harvestF"),
                                legend.title = "",
                              alpha = 0.05) + 
  
  scale_colour_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                    values = c("red", "green4", "blue", "black")) +
  
  
  geom_point(data = model_3_H$data[[3]], mapping = aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Mean Annual Relative Humidity Transfer Distance (%)", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 17),
        text = element_text(family = "Times"))

RH_plot 

### 5.4. Composites ------ 

ggarrange(NFFD_plot, EMT_plot, RH_plot, 
          labels = c("A", "B", "C", "D"), 
          vjust = 0.5, 
          common.legend = TRUE, legend = "top")

ggarrange(NFFD_plot, RH_plot, 
          labels = c("A", "B"), 
          vjust = 0.5, 
          common.legend = TRUE, legend = "top")


# 6.  Cover Interaction Plots --------------------------------------------------

# Save at "A4" .pdf

### 6.1. MAT plot ----
MAT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[1]], type = "pred", 
                                terms = c("d_MAT [all]", "tree_cover [0, 10, 30, 60]"),
                                legend.title = "Percent Crown Closure (%)",
                                alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[1]], mapping = aes(x = d_MAT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 0.02, alpha = 0.5) +
  
  labs(x = bquote(bold("Mean Annual Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

MAT_cov_plot


### 6.2. MAP plot -----
MAP_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[2]], type = "pred", 
                                    terms = c("d_MAP [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                   alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[2]], mapping = aes(x = d_MAP, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 2.5, alpha = 0.5) +
  
  labs(x = "Mean Annual Precipitation Transfer Distance (mm)", 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

MAP_cov_plot

### 6.3. NFFD plot -----
NFFD_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[3]], type = "pred", 
                                    terms = c("d_NFFD [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                    alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[3]], mapping = aes(x = d_NFFD, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 0.4, alpha = 0.5) +
  
  labs(x = "Number of Frost Free Days Transfer Distance (days)", 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

NFFD_cov_plot


### 6.4. EMT plot -----
EMT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[4]], type = "pred", 
                                    terms = c("d_EMT [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                   alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[4]], mapping = aes(x = d_EMT, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 0.05, alpha = 0.5) +
  
  labs(x = bquote(bold("Extreme Minimum Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

EMT_cov_plot

### 6.5. EXT plot ----
EXT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[5]], type = "pred", 
                                    terms = c("d_EXT [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)",
                                    alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[5]], mapping = aes(x = d_EXT, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 0.02, alpha = 0.5) +
  
  labs(x = bquote(bold("Extreme Maximum Temperature Transfer Distance (" ^"o" * "C)")), 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

EXT_cov_plot

### 6.6. RH plot ----
RH_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[6]], type = "pred", 
                                    terms = c("d_RH [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                  alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[6]], mapping = aes(x = d_RH, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray20", width = 0.05, alpha = 0.5) +
  
  labs(x = "Mean Annual Relative Humidity Transfer Distance (%)", 
       y = "Predicted Probability of Survival (%)",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times", size = 17))

RH_cov_plot

### 6.7 Composites ----

ggarrange(MAT_cov_plot,
          MAP_cov_plot, NFFD_cov_plot, FFP_cov_plot,
          EMT_cov_plot, EXT_cov_plot,
          RH_cov_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F",
                     "G"),
          hjust = -1.5, 
          common.legend = TRUE, legend = "top")


ggarrange(MAT_cov_plot,MAP_cov_plot,
          NFFD_cov_plot, RH_cov_plot,
          labels = c("A", "B", "C",
                     "D"),
          vjust = 0.2, 
          hjust = -2,
          common.legend = TRUE, legend = "top")


# 7. Climatic Models Composite ------------------------------------------------

### 7.1. MAT plot ----
MAT_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[1]], type = "pred", 
                                 terms = c("d_MAT [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "MAT Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))



### 7.2. MAP plot -----
MAP_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[4]], type = "pred", 
                                 terms = c("d_MAP [all]")) +
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "MAP Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


### 7.3. MSP plot ----
MSP_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[5]], type = "pred", 
                                 terms = c("d_MSP [all]")) +
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "MSP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))

### 7.4. AHM plot ----
AHM_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[6]], type = "pred", 
                                 terms = c("d_AHM [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "AHM Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))

### 7.5. NFFD plot -----
NFFD_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[7]], type = "pred", 
                                  terms = c("d_NFFD [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "NFFD Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))



### 7.6. PAS plot ----
PAS_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[9]], type = "pred", 
                                 terms = c("d_PAS [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "PAS Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))

### 7.7. EMT plot ----
EMT_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[10]], type = "pred", 
                                 terms = c("d_EMT [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "EMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


### 7.8. EXT plot ----
EXT_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[11]], type = "pred", 
                                 terms = c("d_EXT [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "EXT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


### 7.9. RH plot ----
RH_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[13]], type = "pred", 
                                terms = c("d_RH [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "RH Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


### 7.10. Composites -----

ggarrange(MAT_1_plot,
          MAP_1_plot, MSP_1_plot, AHM_1_plot,
          PAS_1_plot, EXT_1_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


# 8. Harveset emtrends  --------------------------------------------------------------

df <- regen_survival

harvest_labels <- c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")

### 8.1. RH emtrend ----

# Get model 
RH_mod <- model_3_H$model_3[[4]]
RH_mod

# Look at trend significance 
RH_emtrends <- emtrends(RH_mod, pairwise ~ harvestF, var = "d_RH", adjust = "bonferroni")
RH_emtrends

# Plotting

# Lables
RH_sig_labels <- c("AA", "AB", "AB", "BB")
RH_trend_vals <- c(0.2281, 0.1515, 0.1379, 0.0718)

RH_trend <- plot(emtrends(RH_mod, pairwise ~ harvestF, var = "d_RH", adjust = "bonferroni")) +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = RH_sig_labels), hjust = 1.5, 
                                     vjust = -3,
                                     size = 5) +
  geom_text(aes(label = round(RH_trend_vals, 2), hjust = -0.3),
            size = 5) +
  
  
  labs(x = bquote(bold(RH[td] * "  - Harvest Interaction Slope")), 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"),
        text = element_text(family = "Times"))


RH_trend

### 8.2. EMT emtrend ----


# Get model 
EMT_mod <- model_3_H$model_3[[2]]
EMT_mod

# Look at trend significance 
EMT_emtrends <- emtrends(EMT_mod, pairwise ~ harvestF, var = "d_EMT", adjust = "bonferroni")
EMT_emtrends

# Plotting

# Lables
EMT_sig_labels <- c("AA", "AB", "AB", "BB")
EMT_trend_vals <- c(0.1714, 0.1301, 0.1062, 0.0562)


EMT_trend <- plot(emtrends(EMT_mod, pairwise ~ harvestF, var = "d_EMT", adjust = "bonferroni")) +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = EMT_sig_labels), hjust = 1.5, 
            vjust = -3,
            size = 5) +
  geom_text(aes(label = round(EMT_trend_vals, 2), hjust = -0.3), size = 5) +
  
  
  labs(x = bquote(bold(EMT[td] * "  - Harvest Interaction Slope")), 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"),
        text = element_text(family = "Times"))


EMT_trend


### 8.2. NFFD emtrend ----

# Get model
NFFD_mod <- model_3_H$model_3[[1]]
NFFD_mod

# Look at trend significance 
NFFD_emtrends <- emtrends(NFFD_mod, pairwise ~ harvestF, var = "d_NFFD", adjust = "bonferroni")
NFFD_emtrends

# Labels
NFFD_sig_labels <- c("AA", "AB", "AB", "BB")
NFFD_trend_vals <- c(0.0350, 0.0256, 0.0217, 0.0108)


NFFD_trend <- plot(emtrends(NFFD_mod, pairwise ~ harvestF, var = "d_NFFD", adjust = "bonferroni")) +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = NFFD_sig_labels), hjust = 1.5, 
                                     vjust = -3,
                                     size = 5) +
  geom_text(aes(label = round(NFFD_trend_vals, 3), hjust = -0.3),
            size = 5) +
  
  
  labs(x = bquote(bold(NFFD[td] * "  - Harvest Interaction Slope")), 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"),
        text = element_text(family = "Times"))


NFFD_trend


### 8.3 Composites -----------------

inter_plot <- ggarrange(NFFD_plot, RH_plot,
          vjust = 0.5, 
          ncol = 2,
          heights = c(10, 1),
          common.legend = TRUE, legend = "top")

trend_plot <- ggarrange(NFFD_trend, RH_trend)

ggarrange(inter_plot, trend_plot, nrow = 2, heights = c(2, 1))

# save at 1600 x 900 .png or "US legal" .pdf 


# EMT Composite 

ggarrange(EMT_plot, EMT_trend, ncol = 1, heights = c(2.5, 1))

# save graph as 1400 x 1000 .png or 11.50" x 12.00" .pdf