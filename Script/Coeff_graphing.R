library(ggpubr)

ln_height_cover_models

height_1_models <- ln_height_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_1")]

survival_1_models <- survival_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_1")]


height_1_models
survival_1_models



# Height

height_est <-sjPlot::plot_models(height_1_models$model_1[[2]], height_1_models$model_1[[3]], height_1_models$model_1[[6]],
                    height_1_models$model_1[[1]], height_1_models$model_1[[8]], height_1_models$model_1[[7]],
                    height_1_models$model_1[[5]], height_1_models$model_1[[4]], height_1_models$model_1[[9]],
                    show.legend = FALSE,
                    axis.labels=c("RH", "AHM", "NFFD", "EMT", "EXT", "MAT", "PAS", "MSP", "MAP"),
                    show.p = TRUE,
                    show.values = TRUE,
                    title = "Height") +
  
  ylim(-0.1, 0.2) +
  
  labs(y = "Model Beta Estimates") +
  
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) + 
  
  scale_colour_manual(values = c("black", "black", "red", "red", "red", "red", "blue", "blue", "blue")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        title = element_text(face = "bold"))
  


# Survival 

survival_est<- sjPlot::plot_models(survival_1_models$model_1[[2]], survival_1_models$model_1[[3]], survival_1_models$model_1[[6]],
                    survival_1_models$model_1[[1]], survival_1_models$model_1[[8]], survival_1_models$model_1[[7]],
                    survival_1_models$model_1[[5]], survival_1_models$model_1[[4]], survival_1_models$model_1[[9]],
                    show.legend = FALSE,
                    axis.labels=c("RH", "AHM", "NFFD", "EMT", "EXT", "MAT", "PAS", "MSP", "MAP"),
                    show.p = TRUE,
                    show.values = TRUE,
                    title = "Survival") +
  
  ylim(0, 3) +
  
  labs(y = "Model Odds Ratio") +
  
  geom_hline(yintercept = 1, colour = "black", linewidth = 0.4) + 
  
  scale_colour_manual(values = c("black", "black", "red", "red", "red", "red", "blue", "blue", "blue")) +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        title = element_text(face = "bold"))


# Grouping 

ggarrange(survival_est, height_est)
