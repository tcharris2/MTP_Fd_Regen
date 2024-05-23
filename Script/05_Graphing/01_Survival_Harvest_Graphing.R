# CONTENT: Graphing - Survival  ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data -------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)
# check for most recent CSV file

#### 1.1 Importing Functions --------

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")

#### 1.2 Correcting Variable types -------

regen_prepped <- universalDataPrepFunction(regen)

regen_survival <- subset(regen_prepped, !(is.na(tree_cover)))

regen_survival <- subset(regen_survival, 
                         !regen_survival$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                          "Peterhope future Fd", "Alex Fraser future Fd", 
                                                          "Twobit B class Fd"))
regen_survival$sqrt_tree_cover <- sqrt(regen_survival$tree_cover)

str(regen_survival)

#### 1.3 Calling RDS file  ------

survival_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-03-11_survival_group_harvest_models.rds"))

survival_harvest_models


survival_cover_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-03-11_survival_group_cover_models.rds"))

survival_cover_models


# 2. Selecting Best Models ---------------------------------------------------

names(survival_harvest_models)
names(survival_cover_models)

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(survival_harvest_models) <- c("model_0", "model_h", "model_a", 
                                       "model_1", "model_1a", "model_2", "model_2a", 
                                       "model_3", "model_3a", "ClimaticVarList")

colnames(survival_cover_models) <- c("model_0", "model_c", "model_a", 
                                     "model_1", "model_1a", "model_2", "model_2a", 
                                     "model_3", "model_3a", "ClimaticVarList")


model_1 <- survival_harvest_models[c(1:6, 8:13, 15), c("ClimaticVarList", "model_1")]

model_1 <- survival_harvest_models[c(1:6, 10, 12:13), c("ClimaticVarList", "model_1")]

model_3_H <- survival_harvest_models[c(8:9, 11, 15), c("ClimaticVarList", "model_3")]


model_2_C <- survival_cover_models[c(5:6, 10), c("ClimaticVarList", "model_2")]

model_3_C <- survival_cover_models[c(1:4, 8:9, 11:13, 15), c("ClimaticVarList", "model_3")]

# 3. Clean up Global env. ------------------------------------------------------

rm(regen, regen_prepped, universalDataPrepFunction)


# 4. Estimated Probability of Survival -----------------------------------------

source("Script/02b_Survival_Graphing/graphing_Functions.R")

##### 4.1 Adding data -----

model_1$data <- list(regen_survival)

model_1

model_3_H$data <- list(regen_survival)

model_3_H


model_2_C$data <- list(regen_survival)

model_2_C

model_3_C$data <- list(regen_survival)

model_3_C

# Adding estimated probability of survival 
model_1 <- survivalProbs(model_1, model_1$model_1)

model_3_H <- survivalProbs(model_3_H, model_3_H$model_3)

model_2_C <- survivalProbs(model_2_C, model_2_C$model_2)

model_3_C <- survivalProbs(model_3_C, model_3_C$model_3)



# 5. Graphing models -----------------------------------------------------------

graphESTSurvivalProb(climatic_models)

graphESTSurvivalProb_2(inter_models)

graphESTSurvivalProb_3(inter_cover_models)

graphESTSurvivalProb_3(climatic_cover_models)

inter_harvest_models

s_mod_1

sjPlot::plot_model(s_mod_1, type = "pred", terms = c("d_MAT [all]", "harvestF"), se = FALSE)

sjPlot::plot_model(inter_harvest_models[["model_3"]][[4]],
                   type = "pred", 
                   terms = c("d_RH [all]", "harvestF"),
                   legend.title = "Harvest",
                   legend_style(pos= "Top Right")) +

        geom_point(data = inter_harvest_models$data[[4]], 
                   aes(x = d_RH, y = survival_probs), 
                   inherit.aes = FALSE, size = 0.5) +
  
        theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
              panel.grid.major = element_line(color = "gray30", linewidth = .15),
              panel.grid.minor = element_blank()) +

        labs(x = paste( "d_RH", "Climatic Distance"), 
             y = "Estimated Probability of Survival",
             title = NULL)
  
b <- sjPlot::plot_model(inter_harvest_models[["model_3"]][[3]],
                        type = "pred", 
                        terms = c("d_EMT [all]", "harvestF"),
                        legend.title = "Harvest",
                        legend.position = "top",
                        palette = "jco") +
  
  geom_point(data = inter_harvest_models$data[[4]], 
             aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray30", linewidth = .15),
        panel.grid.minor = element_blank()) +
  
  labs(x = paste( "d_EMT", "Climatic Distance"), 
       y = "Estimated Probability of Survival",
       title = NULL)

b
ggarrange(a, b, common.legend = TRUE, label.y = 0) 


inter_harvest_models
scale_fill_discrete(labels = c("Clearcut", 'Seed Tree', "30Ret", "60Ret"))


ggdensity(data = inter_harvest_models$data[[4]], "survival_probs", fill = "harvestF", 
          palette = "jco")


ReMSP.log <- 1.3193627 + 0.3536144 * climatic_models[[3]][[1]][["d_MSP"]]

ReMSP.prob <- (exp((ReMSP.log))) / (1+exp((ReMSP.log)))


plot(climatic_models[[3]][[1]][["d_MSP"]], ReMSP.prob, ylim = c(0,1), 
     main = "Survival vs MSP", xlab = "MSP Climatic Distance", ylab = "Survival") ## probabilities



# 6. Editing selected graphs --------------------------------------------------


  
# NFFD plot
NFFD_mod <- model_3_H$model_3[[1]]
RH_mod <- model_3_H$model_3[[4]]

tab_model(NFFD_mod, RH_mod, transform = NULL)

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
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        text = element_text(family = "Times"))

NFFD_plot

# FFP Plot
FFP_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[2]], type = "pred", 
                                terms = c("d_FFP [all]", "harvestF"),
                                legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
  
  geom_point(data = model_3_H$data[[2]], mapping = aes(x = d_FFP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "FFP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# EMT Plot
EMT_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[3]], type = "pred", 
                                terms = c("d_EMT [all]", "harvestF"),
                                legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
  
  geom_point(data = model_3_H$data[[3]], mapping = aes(x = d_EMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "EMT Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# RH Plot 
RH_plot <- sjPlot::plot_model(model_3_H[["model_3"]][[4]], type = "pred", 
                                terms = c("d_RH [all]", "harvestF"),
                                legend.title = "",
                              alpha = 0.05) + 
  
  scale_colour_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention"),
                    values = c("red", "green4", "blue", "black")) +
  
  
  geom_point(data = model_3_H$data[[4]], mapping = aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Mean Annual Relative Humidity Transfer Distance (%)", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        text = element_text(family = "Times"))

RH_plot 

# Composite plots 

library(ggpubr)


ggarrange(NFFD_plot, FFP_plot, EMT_plot, RH_plot, 
          labels = c("A", "B", "C", "D"), 
          vjust = 0.5, 
          common.legend = TRUE, legend = "top")

ggarrange(NFFD_plot, RH_plot, 
          labels = c("A", "B"), 
          vjust = 0.5, 
          common.legend = TRUE, legend = "top")


# 7. Climatic Models Composite ------------------------------------------------

model_1

# MAT plot
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


# MWMT plot
MWMT_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[2]], type = "pred", 
                                 terms = c("d_MWMT [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "MWMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# MCMT plot
MCMT_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[3]], type = "pred", 
                                  terms = c("d_MCMT [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "MCMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# MAP plot
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


# MSP plot
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

# AHM plot
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

# NFFD plot
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


# FFP plot
FFP_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[8]], type = "pred", 
                                 terms = c("d_FFP [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "FFP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# PAS plot
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

# EMT plot
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


# EXT plot
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

# Eref plot
Eref_1_plot <- sjPlot::plot_model(model_1[["model_1"]][[12]], type = "pred", 
                                  terms = c("d_Eref [all]")) + 
  
  scale_y_continuous(limits = c(0.2, 1), 
                     labels = c("20%", "40%", "60%", "80%", "100%")) +
  
  labs(x = "Eref Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# RH plot
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



ggarrange(MAT_1_plot, MWMT_1_plot, MCMT_1_plot,
          MAP_1_plot, MSP_1_plot, AHM_1_plot,
          PAS_1_plot, EXT_1_plot, Eref_1_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F", 
                     "G", "H", "I"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


# 8. Inter Cover Models Composite --------------------------------------------------
model_3_C

# MAT plot
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
       y = "Predicted Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"),
        text = element_text(family = "Times"))

MAT_cov_plot


# MWMT plot
MWMT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[2]], type = "pred", 
                                   terms = c("d_MWMT [all]", "tree_cover [0, 25, 50]"),
                                   legend.title = "Percent Tree Cover (%)") + 
  
  
  geom_point(data = model_3_C$data[[2]], mapping = aes(x = d_MWMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "MWMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# MCMT plot
MCMT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[3]], type = "pred", 
                                    terms = c("d_MCMT [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_3_C$data[[3]], mapping = aes(x = d_MCMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "MCMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# MAP plot
MAP_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[4]], type = "pred", 
                                    terms = c("d_MAP [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                   alpha = 0.05) +
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[4]], mapping = aes(x = d_MAP, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray40", width = 2.5, alpha = 0.5) +
  
  labs(x = "MAP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"))

MAP_cov_plot

# NFFD plot
NFFD_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[5]], type = "pred", 
                                    terms = c("d_NFFD [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                    alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[5]], mapping = aes(x = d_NFFD, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray40", width = 0.4, alpha = 0.5) +
  
  labs(x = "NFFD Climatic Distance", 
       y = "Predicted Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"))

NFFD_cov_plot


# FFP plot
FFP_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[6]], type = "pred", 
                                    terms = c("d_FFP [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_3_C$data[[6]], mapping = aes(x = d_FFP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "FFP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# EMT plot
EMT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[7]], type = "pred", 
                                    terms = c("d_EMT [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_3_C$data[[7]], mapping = aes(x = d_EMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "EMT Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# EXT plot
EXT_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[8]], type = "pred", 
                                    terms = c("d_EXT [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_3_C$data[[8]], mapping = aes(x = d_EXT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "EXT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# Eref plot
Eref_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[9]], type = "pred", 
                                    terms = c("d_Eref [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 

  geom_point(data = model_3_C$data[[9]], mapping = aes(x = d_Eref, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Eref Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# RH plot
RH_cov_plot <- sjPlot::plot_model(model_3_C[["model_3"]][[10]], type = "pred", 
                                    terms = c("d_RH [all]", "tree_cover [0, 10, 30, 60]"),
                                    legend.title = "Percent Tree Cover (%)",
                                  alpha = 0.05) + 
  
  scale_colour_manual(labels = c("0", "10", "30", "60"),
                      values = c("red", "green4", "blue", "black")) +
  scale_fill_manual(labels = c("0", "10", "30", "60"),
                    values = c("red", "green4", "blue", "black")) +
  
  geom_jitter(data = model_3_C$data[[10]], mapping = aes(x = d_RH, y = survival_probs), 
              inherit.aes = FALSE, size = 0.2, colour = "gray40", width = 0.07, alpha = 0.5) +
  
  labs(x = "RH Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.spacing.y = unit(1, "cm"))

RH_cov_plot

library(ggpubr)

ggarrange(MAT_cov_plot, MWMT_cov_plot, MCMT_cov_plot,
          MAP_cov_plot, NFFD_cov_plot, FFP_cov_plot,
          EMT_cov_plot, EXT_cov_plot, Eref_cov_plot,
          RH_cov_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F",
                     "G", "H", "I", "J"),
          hjust = -1.5, 
          common.legend = TRUE, legend = "top")


ggarrange(MAT_cov_plot,MAP_cov_plot,
          NFFD_cov_plot, RH_cov_plot,
          labels = c("A", "B", "C",
                     "D"),
          vjust = 0.2, 
          hjust = -2,
          common.legend = TRUE, legend = "top")

# 9. Climatic Cover Models -------------------------------------------------
model_2_C

# MSP Plot
MSP_cov2_plot <- sjPlot::plot_model(model_2_C[["model_2"]][[1]], type = "pred", 
                                  terms = c("d_MSP [all]", "tree_cover [0, 25, 50]"),
                                  legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_2_C$data[[1]], mapping = aes(x = d_MSP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "MSP Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# AHM plot
AHM_cov2_plot <- sjPlot::plot_model(model_2_C[["model_2"]][[2]], type = "pred", 
                                    terms = c("d_AHM [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_2_C$data[[2]], mapping = aes(x = d_AHM, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "AHM Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# PAS Plot
PAS_cov2_plot <- sjPlot::plot_model(model_2_C[["model_2"]][[3]], type = "pred", 
                                    terms = c("d_PAS [all]", "tree_cover [0, 25, 50]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  geom_point(data = model_2_C$data[[3]], mapping = aes(x = d_PAS, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "PAS Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# Composite Plot
ggarrange(MSP_cov2_plot, AHM_cov2_plot, PAS_cov2_plot,
          labels = c("A", "B", "C"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


# 10. emmeans --------------------------------------------------------------

library(emmeans)

df <- regen_survival

model_3_H

NFFD_mod <- model_3_H$model_3[[1]]
RH_mod <- model_3_H$model_3[[4]]

NFFD_mod
RH_mod

  
# Useful 
joint_tests(mod, by = "d_RH")
joint_tests(mod, by = "harvestF")
joint_tests(mod)

RH_emtrends <- emtrends(RH_mod, pairwise ~ harvestF, var = "d_RH", adjust = "bonferroni")
RH_emtrends
# Plotting

harvest_labels <- c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")
sig_labels <- c("AA", "AB", "AB", "BB")
sig_labels_1 <- c("AA", "", "", "")
RH_trend_vals <- c(0.2281, 0.1515, 0.1379, 0.0718)


RH_trend <- plot(emtrends(RH_mod, pairwise ~ harvestF, var = "d_RH", adjust = "bonferroni")) +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = sig_labels), hjust = 1.5, 
                                     vjust = -3,
                                     size = 3) +
  geom_text(aes(label = round(RH_trend_vals, 2), hjust = -0.3)) +
  
  
  labs(x = "Relative Humidity - Harvest \n Interaction Slope", 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))


RH_trend

NFFD_emtrends <- emtrends(NFFD_mod, pairwise ~ harvestF, var = "d_NFFD", adjust = "bonferroni")
NFFD_emtrends

NFFD_trend_vals <- c(0.0350, 0.0256, 0.0217, 0.0108)


NFFD_trend <- plot(emtrends(NFFD_mod, pairwise ~ harvestF, var = "d_NFFD", adjust = "bonferroni")) +
  
  coord_flip() +
  
  scale_y_discrete(labels = harvest_labels) +
  
  geom_text(aes(label = sig_labels), hjust = 1.5, 
                                     vjust = -3,
                                     size = 3) +
  geom_text(aes(label = round(NFFD_trend_vals, 3), hjust = -0.3)) +
  
  
  labs(x = "Number of Frost Free Days - Harvest \n Interaction Slope", 
       y = "",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times"))


NFFD_trend

NFFD_mod
RH_mod

# Grouping -----------------

inter_plot <- ggarrange(NFFD_plot, RH_plot,
          vjust = 0.5, 
          ncol = 2,
          heights = c(10, 1),
          common.legend = TRUE, legend = "top")

trend_plot <- ggarrange(NFFD_trend, RH_trend)

ggarrange(inter_plot, trend_plot, nrow = 2, heights = c(2, 1))


ggarrange(NFFD_plot, RH_plot,
          ggarrange(NFFD_trend, RH_trend, nrow = 1, labels = c("C", "D"), align = "hv"),
          labels = c("A", "B"), widths = c(1, 10), align = "v", common.legend = TRUE)

# Crown Closure means ---------------------------------------
library(ggeffects)

MAT_mod <- model_3_C[["model_3"]][[1]]

tab_model(MAT_mod)

df_2 <- ggpredict(MAT_mod, terms = c("d_MAT [all]", "tree_cover [0, 10, 30, 60]"), terms_to_colnames = TRUE, type = "random")
df_2

emtrends(MAT_mod, pairwise ~ tree_cover, var = "d_MAT", adjust = "bonferroni", 
         at=list(d_MAT = c(-2.5, 0, 2.5, 5), tree_cover =c(0, 0.000002, 30, 60)))

emtrends(mod, pairwise ~ temp, var="nitro", at=list(variety="A", temp=c(20,40)))

joint_tests(MAT_mod, by = "d_MAT")

emmip(MAT_mod, tree_cover ~ d_MAT, mult.name = "variety", cov.reduce = FALSE)

emtrends(MAT_mod, pairwise ~ d_MAT, var = "tree_cover", mult.name = "d_MAT")




trend_plots <- ggarrange(NFFD_trend, RH_trend, nrow = 1, labels = c("C", "D"), align = "hv")
# Testing 
fiber.lm <- lm(strength ~ diameter*machine, data = fiber)

fiber.lm
emtrends(fiber.lm, pairwise ~ machine, var = "diameter")


emmip(RH_mod, harvestF ~ d_RH, cov.reduce = range)


df_1 <- ggpredict(mod, terms = c("d_RH [all]", "harvestF", "locationF"), terms_to_colnames = TRUE, type = "random")
df_1


ggplot(df_1, aes(x, predicted)) +
  geom_line(aes(color = group)) +
  facet_wrap(~ facet)


mod_2 <- ln_height_cover_models$model_3[[15]]
mod_2


df_2 <- ggpredict(mod_2, terms = c("d_RH [all]", "tree_cover [0, 10, 25, 50]", "locationF"), terms_to_colnames = TRUE, type = "random")
df_2


ggplot(df_2, aes(x, predicted)) +
  geom_line(aes(color = group)) +
  facet_wrap(~ facet)


# Less Useful

plots.emm.RH <- emmeans(mod, ~ d_RH * harvestF,  
                        glmerTest.limit = 8040, pbkrtest.limit = 8040)
pairs(plots.emm.RH, adjust="bonferroni", side="two-sided")


contrast(plots.emm.RH, "consec", simple = "each", combine = TRUE, adjust = "mvt")
plots.emm.RH




# For cover 

model_3_C
mod.c <- model_3_C$model_3[[1]]
mod.c

emtrend.cb <- emtrends(mod, pairwise ~ tree_cover, var = "d_MAT", adjust = "bonferroni")
joint_tests(mod.c, by = "d_MAT")

emmeans(mod.c, pairwise ~ tree_cover)


# Testing 

sjPlot::plot_model(model_3_H[["model_3"]][[4]], type = "pred", 
                              terms = c("d_RH [all]", "harvestF"),
                              legend.title = "", show.intercept = TRUE) + 
  
  scale_colour_discrete(labels = c("Clearcut *", "Seed Tree", "30% Retention", "60% Retention *")) +
  
  
  geom_point(data = model_3_H$data[[4]], mapping = aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "RH Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")



sjPlot::plot_model(model_3_H[["model_3"]][[4]], type = "pred", 
                   terms = c("d_RH [all]", "harvestF"),
                   legend.title = "", show.zeroinf = TRUE)


# 11. Beta coeffs ------------------------

install.packages("mpae")
library(mpae)

survival_harvest_models

s_mod <- survival_harvest_models$s_group_model_harvest_1[[4]]

h_mod <- ln_height_cover_models$ln_h_group_model_cover_1[[4]]

scaled.coef(h_mod)
scaled.coef
UseMethod

tab_model(s_mod)
tab_model(h_mod)


# 12. P-Values --------------------------

NFFD_mod_2 <- survival_harvest_models$model_2[[8]]

RH_mod_2 <- survival_harvest_models$model_2[[15]]

lrtest(NFFD_mod_2, NFFD_mod)
lrtest(RH_mod_2, RH_mod)
