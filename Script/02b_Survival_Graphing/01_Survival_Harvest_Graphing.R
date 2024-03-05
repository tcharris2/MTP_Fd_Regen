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
                                               "2024-02-05_survival_group_harvest_models_NoFutures.rds"))

survival_harvest_models


survival_cover_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-02-20_survival_group_cover_models_NoFutures_sqrt.rds"))

survival_cover_models


# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(survival_harvest_models) <- c("ClimaticVarList", "model_0", "model_h", "model_a", 
                                         "model_1", "model_1a", "model_2", "model_2a", 
                                         "model_3", "model_3a")

colnames(survival_cover_models) <- c("ClimaticVarList", "model_0", "model_c", "model_a", 
                                       "model_1", "model_1a", "model_2", "model_2a", 
                                       "model_3", "model_3a")


climatic_models <- survival_harvest_models[c(1:6, 10, 12:13), c("ClimaticVarList", "model_1")]

inter_harvest_models <- survival_harvest_models[c(8:9, 11, 15), c("ClimaticVarList", "model_3")]


climatic_cover_models <- survival_cover_models[c(5:6, 10), c("ClimaticVarList", "model_2")]

inter_cover_models <- survival_cover_models[c(1:4, 8:9, 11:13, 15), c("ClimaticVarList", "model_3")]

# 3. Clean up Global env. ------------------------------------------------------

rm(regen, regen_prepped, universalDataPrepFunction)


# 4. Estimated Probability of Survival -----------------------------------------

source("Script/02b_Survival_Graphing/graphing_Functions.R")

##### 4.1 Adding data -----

climatic_models$data <- list(regen_survival)

climatic_models

inter_harvest_models$data <- list(regen_survival)

inter_harvest_models


climatic_cover_models$data <- list(regen_survival)

climatic_cover_models

inter_cover_models$data <- list(regen_survival)

inter_cover_models

# Adding esitmated probability of survival 
climatic_models <- survivalProbs(climatic_models, climatic_models$model_1)

inter_harvest_models <- survivalProbs(inter_harvest_models, inter_harvest_models$model_3)

climatic_cover_models <- survivalProbs(climatic_cover_models, climatic_cover_models$model_2)

inter_cover_models <- survivalProbs(inter_cover_models, inter_cover_models$model_3)



# 5. Graphing models -----------------------------------------------------------

graphESTSurvivalProb(climatic_models)

graphESTSurvivalProb_2(inter_models)

graphESTSurvivalProb_3(inter_cover_models)

graphESTSurvivalProb_3(climatic_cover_models)

inter_harvest_models


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
NFFD_plot <- sjPlot::plot_model(inter_harvest_models[["model_3"]][[1]], type = "pred", 
                   terms = c("d_NFFD [all]", "harvestF"),
                   legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
          
  geom_point(data = inter_harvest_models$data[[1]], mapping = aes(x = d_NFFD, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Normalized NFFD Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# FFP Plot
FFP_plot <- sjPlot::plot_model(inter_harvest_models[["model_3"]][[2]], type = "pred", 
                                terms = c("d_FFP [all]", "harvestF"),
                                legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
  
  geom_point(data = inter_harvest_models$data[[2]], mapping = aes(x = d_FFP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Normalized FFP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# EMT Plot
EMT_plot <- sjPlot::plot_model(inter_harvest_models[["model_3"]][[3]], type = "pred", 
                                terms = c("d_EMT [all]", "harvestF"),
                                legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
  
  geom_point(data = inter_harvest_models$data[[3]], mapping = aes(x = d_EMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Normalized EMT Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# RH Plot 
RH_plot <- sjPlot::plot_model(inter_harvest_models[["model_3"]][[4]], type = "pred", 
                                terms = c("d_RH [all]", "harvestF"),
                                legend.title = "") + 
  
  scale_colour_discrete(labels = c("Clearcut", "Seed Tree", "30% Retention", "60% Retention")) +
  
  
  geom_point(data = inter_harvest_models$data[[4]], mapping = aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.5) +
  
  labs(x = "Normalized RH Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

  

# Composite plots 

library(ggpubr)


ggarrange(NFFD_plot, FFP_plot, EMT_plot, RH_plot, labels = c("A", "B", "C", "D"), vjust = 0.5, 
          common.legend = TRUE, legend = "top")

class(regen_survival$plotF)


# 7. Climatic Models Composite ------------------------------------------------

climatic_models

# MAT plot
MAT_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[1]], type = "pred", 
                              terms = c("d_MAT [all]"),
                              legend.title = "") + 
  
  labs(x = "Normalized MAT Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# MWMT plot
MWMT_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[2]], type = "pred", 
                                 terms = c("d_MWMT [all]"),
                                 legend.title = "") + 
  
  labs(x = "Normalized MWMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# MCMT plot
MCMT_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[3]], type = "pred", 
                                  terms = c("d_MCMT [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized MCMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# MAP plot
MAP_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[4]], type = "pred", 
                                  terms = c("d_MAP [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized MAP Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# MSP plot
MSP_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[5]], type = "pred", 
                                  terms = c("d_MSP [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized MSP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# AHM plot
AHM_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[6]], type = "pred", 
                                  terms = c("d_AHM [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized AHM Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# PAS plot
PAS_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[7]], type = "pred", 
                                  terms = c("d_PAS [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized PAS Climatic Distance", 
       y = "Esitmated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# EXT plot
EXT_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[8]], type = "pred", 
                                  terms = c("d_EXT [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized EXT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# Eref plot
Eref_1_plot <- sjPlot::plot_model(climatic_models[["model_1"]][[9]], type = "pred", 
                                  terms = c("d_Eref [all]"),
                                  legend.title = "") + 
  
  labs(x = "Normalized Eref Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")



ggarrange(MAT_1_plot, MWMT_1_plot, MCMT_1_plot,
          MAP_1_plot, MSP_1_plot, AHM_1_plot,
          PAS_1_plot, EXT_1_plot, Eref_1_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F", 
                     "G", "H", "I"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")


# 8. Inter Cover Models Composite --------------------------------------------------
inter_cover_models

# MAT plot
MAT_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[1]], type = "pred", 
                                terms = c("d_MAT [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_MAT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, position = "jitter", colour = "gray20") +
  
  labs(x = "Normalized MAT Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# MWMT plot
MWMT_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[2]], type = "pred", 
                                   terms = c("d_MWMT [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                   legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_MWMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized MWMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# MCMT plot
MCMT_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[3]], type = "pred", 
                                    terms = c("d_MCMT [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_MCMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized MCMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# MAP plot
MAP_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[4]], type = "pred", 
                                    terms = c("d_MAP [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_MAP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized MAP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# NFFD plot
NFFD_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[5]], type = "pred", 
                                    terms = c("d_NFFD [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_NFFD, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized NFFD Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# FFP plot
FFP_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[6]], type = "pred", 
                                    terms = c("d_FFP [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_FFP, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized FFP Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# EMT plot
EMT_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[7]], type = "pred", 
                                    terms = c("d_EMT [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_EMT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized EMT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# EXT plot
EXT_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[8]], type = "pred", 
                                    terms = c("d_EXT [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_EXT, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized EXT Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


# Eref plot
Eref_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[9]], type = "pred", 
                                    terms = c("d_Eref [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_Eref, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized Eref Climatic Distance", 
       y = "Estimated Probability of Survival",
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")

# RH plot
RH_cov_plot <- sjPlot::plot_model(inter_cover_models[["model_3"]][[10]], type = "pred", 
                                    terms = c("d_RH [all]", "sqrt_tree_cover [0, 5, 7.07]"),
                                    legend.title = "Percent Tree Cover (%)") + 
  
  scale_colour_discrete(labels = c("0", "25", "50")) +
  
  
  geom_point(data = inter_cover_models$data[[1]], mapping = aes(x = d_RH, y = survival_probs), 
             inherit.aes = FALSE, size = 0.2, colour = "gray20") +
  
  labs(x = "Normalized RH Climatic Distance", 
       y = NULL,
       title = NULL) + 
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top")


ggarrange(MAT_cov_plot, MWMT_cov_plot, MCMT_cov_plot,
          MAP_cov_plot, EXT_cov_plot, RH_cov_plot,
          labels = c("A", "B", "C",
                     "D", "E", "F"),
          hjust = -1, 
          common.legend = TRUE, legend = "top")
