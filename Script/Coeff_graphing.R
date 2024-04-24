library(ggpubr)

ln_height_cover_models

height_1_models <- ln_height_harvest_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_1")]

survival_1_models <- survival_harvest_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_1")]


height_1_models
survival_1_models


myfun <- function(x){
  (exp(x) - 1) * 100
}
# Height

plot_model(height_1_models$model_1[[2]])

sjPlot::plot_models(height_1_models$model_1[[2]], height_1_models$model_1[[3]], height_1_models$model_1[[6]],
                    height_1_models$model_1[[1]], height_1_models$model_1[[8]], height_1_models$model_1[[7]],
                    height_1_models$model_1[[5]], height_1_models$model_1[[4]], height_1_models$model_1[[9]],
                    std.est = "std2") + ylim(-0.1, 0.2) 

height_est <-sjPlot::plot_models(height_1_models$model_1[[2]], height_1_models$model_1[[3]], height_1_models$model_1[[6]],
                    height_1_models$model_1[[1]], height_1_models$model_1[[8]], height_1_models$model_1[[7]],
                    height_1_models$model_1[[5]], height_1_models$model_1[[4]], height_1_models$model_1[[9]],
                    show.legend = FALSE,
                    show.p = TRUE,
                    show.values = TRUE) +
  
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

# Survival 

survival_est<- sjPlot::plot_models(survival_1_models$model_1[[2]], survival_1_models$model_1[[3]], survival_1_models$model_1[[6]],
                    survival_1_models$model_1[[1]], survival_1_models$model_1[[8]], survival_1_models$model_1[[7]],
                    survival_1_models$model_1[[5]], survival_1_models$model_1[[4]], survival_1_models$model_1[[9]],
                    show.legend = FALSE,
                    axis.labels=c(bquote(RH[tds]), bquote(AHM[tds]), bquote(NFFD[tds]), 
                                  bquote(EMT[tds]), bquote(EXT[tds]), bquote(MAT[tds]), 
                                  bquote(PAS[tds]), bquote(MSP[tds]), bquote(MAP[tds])),
                    show.p = TRUE,
                    show.values = TRUE,
                    transform = "plogis") +
  
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
# Grouping 

ggarrange(survival_est, height_est, align = "hv")
