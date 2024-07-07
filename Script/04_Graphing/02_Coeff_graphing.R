####### MODEL COEFFICIENTS ################
#' @Content: Graphing Model Coefficients ---------------------------------------------

#' @Author: Thomson Harris
#' @Date: July 7th, 2024

# 1. Loading Packages -----------
library(here)
library(tidyverse)
library(sjPlot)
library(ggpubr)


# 2. Importing Data ---------------

ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                              "2024-03-11_ln_height_cover_models.rds" ))

survival_harvest_mods <- readRDS(file = here("Data/04_Temp", 
                                             "2024-03-11_survival_harvest_models.rds"))



ln_height_cover_models

height_models <- ln_height_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_3a")]

survival_models <- survival_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_3")]


height_models
survival_models

# 3. Height -----------------

plot_model(height_models$model_1[[2]])

sjPlot::plot_models(height_models$model_3[[2]], height_models$model_3[[3]], height_models$model_3[[6]],
                    height_models$model_3[[1]], height_models$model_3[[8]], height_models$model_3[[7]],
                    height_models$model_3[[5]], height_models$model_3[[4]], height_models$model_3[[9]],
                    std.est = "std2") + ylim(-0.1, 0.2) 

height_est <-sjPlot::plot_models(height_models$model_3a[[2]], height_models$model_3a[[3]], height_models$model_3a[[6]],
                    height_models$model_3a[[1]], height_models$model_3a[[8]], height_models$model_3a[[7]],
                    height_models$model_3a[[5]], height_models$model_3a[[4]], height_models$model_3a[[9]],
                    show.legend = FALSE,
                    show.p = TRUE,
                    show.values = TRUE,
                    rm.terms = c("sqrt(tree_cover)", "age",
                                 "scale(d_MAP):sqrt(tree_cover)", "scale(d_MSP):sqrt(tree_cover)", "scale(d_PAS):sqrt(tree_cover)",
                                 "scale(d_MAT):sqrt(tree_cover)", "scale(d_EXT):sqrt(tree_cover)", "scale(d_EMT):sqrt(tree_cover)",
                                 "scale(d_NFFD):sqrt(tree_cover)", "scale(d_AHM):sqrt(tree_cover)", "scale(d_RH):sqrt(tree_cover)")) +
  
  ylim(-0.10, 0.20) +
  
  labs(y = expression(bold("Scaled " * beta * " Estimate")),
       title = bquote(bold("Height Models"^"b"))) +
  
  scale_x_discrete(labels = c(bquote(RH[td]), bquote(AHM[td]), bquote(NFFD[td]), 
                              bquote(EMT[td]), bquote(EXT[td]), bquote(MAT[td]), 
                              bquote(PAS[td]), bquote(MSP[td]), bquote(MAP[td]))) +
  
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) + 
  
  scale_colour_manual(values = c("black", "black", "red", "red", "red", "red", "blue", "blue", "blue")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        title = element_text(face = "bold"),
        text = element_text(family = "Times"))
  
height_est

# 4. Survival ---------------------

survival_est<- sjPlot::plot_models(survival_models$model_3[[2]], survival_models$model_3[[3]], survival_models$model_3[[6]],
                    survival_models$model_3[[1]], survival_models$model_3[[8]], survival_models$model_3[[7]],
                    survival_models$model_3[[5]], survival_models$model_3[[4]], survival_models$model_3[[9]],
                    show.legend = FALSE,
                    axis.labels=c(bquote(RH[tds]), bquote(AHM[tds]), bquote(NFFD[tds]), 
                                  bquote(EMT[tds]), bquote(EXT[tds]), bquote(MAT[tds]), 
                                  bquote(PAS[tds]), bquote(MSP[tds]), bquote(MAP[tds])),
                    show.p = TRUE,
                    show.values = TRUE,
                    transform = "plogis",
                    rm.terms = c("sqrt(tree_cover)",
                                 "scale(d_MAP):sqrt(tree_cover)", "scale(d_MSP):sqrt(tree_cover)", "scale(d_PAS):sqrt(tree_cover)",
                                 "scale(d_MAT):sqrt(tree_cover)", "scale(d_EXT):sqrt(tree_cover)", "scale(d_EMT):sqrt(tree_cover)",
                                 "scale(d_NFFD):sqrt(tree_cover)", "scale(d_AHM):sqrt(tree_cover)", "scale(d_RH):sqrt(tree_cover)")) +
  
  ylim(0, 1) +
  
  labs(y = expression(bold("Scaled Probabilities")),
       title = bquote(bold("Survival Models"^"a"))) +
  
  scale_x_discrete(labels = c(bquote(RH[td]), bquote(AHM[td]), bquote(NFFD[td]), 
                              bquote(EMT[td]), bquote(EXT[td]), bquote(MAT[td]), 
                              bquote(PAS[td]), bquote(MSP[td]), bquote(MAP[td]))) +
  
  geom_hline(yintercept = 0.5, colour = "black", linewidth = 0.4) + 
  
  scale_colour_manual(values = c("black", "black", "red", "red", "red", "red", "blue", "blue", "blue")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Times"))

survival_est

# 5. Composites ----------------------

ggarrange(survival_est, height_est, align = "hv")


