library(ggpubr)

ln_height_cover_models

height_1_models <- ln_height_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_3a")]

height_1_models_h <- ln_height_harvest_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_2a")]

survival_1_models <- survival_cover_models[c(1, 4:6, 8, 10:12, 15), c("ClimaticVarList", "model_3")]


height_1_models_c
survival_1_models


myfun <- function(x){
  (exp(x) - 1) * 100
}
# Height -----------------

plot_model(height_1_models$model_1[[2]])

sjPlot::plot_models(height_1_models$model_3[[2]], height_1_models$model_3[[3]], height_1_models$model_3[[6]],
                    height_1_models$model_3[[1]], height_1_models$model_3[[8]], height_1_models$model_3[[7]],
                    height_1_models$model_3[[5]], height_1_models$model_3[[4]], height_1_models$model_3[[9]],
                    std.est = "std2") + ylim(-0.1, 0.2) 

height_est <-sjPlot::plot_models(height_1_models$model_3a[[2]], height_1_models$model_3a[[3]], height_1_models$model_3a[[6]],
                    height_1_models$model_3a[[1]], height_1_models$model_3a[[8]], height_1_models$model_3a[[7]],
                    height_1_models$model_3a[[5]], height_1_models$model_3a[[4]], height_1_models$model_3a[[9]],
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

# Survival ---------------------

survival_est<- sjPlot::plot_models(survival_1_models$model_3[[2]], survival_1_models$model_3[[3]], survival_1_models$model_3[[6]],
                    survival_1_models$model_3[[1]], survival_1_models$model_3[[8]], survival_1_models$model_3[[7]],
                    survival_1_models$model_3[[5]], survival_1_models$model_3[[4]], survival_1_models$model_3[[9]],
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

# Grouping ----------------------

library(ggpubr)

ggarrange(survival_est, height_est, align = "hv")



# Height

plot_model(height_1_models$model_1[[2]])

age_mod <- ln_height_cover_models$model_a[[1]]
cover_mod <- ln_height_cover_models$model_c[[1]]

plot_model(age_mod)
plot_model(cover_mod)
plot_model(age_mod, type = "pred", terms = "age")
plot_model(cover_mod, type = "pred", terms = "tree_cover")

# Interaction Height 

sjPlot::plot_models(height_1_models_c$model_3a[[2]], height_1_models_c$model_3a[[3]], height_1_models_c$model_3a[[6]],
                    height_1_models_c$model_3a[[1]], height_1_models_c$model_3a[[8]], height_1_models_c$model_3a[[7]],
                    height_1_models_c$model_3a[[5]], height_1_models_c$model_3a[[4]], height_1_models_c$model_3a[[9]], 
                    rm.terms = c("age", "sqrt(tree_cover)"),
                    show.p = TRUE,
                    show.values = TRUE,
                    show.legend = FALSE) +
  
  ylim(-0.1, 0.15)  +
  
  labs(y = expression(bold("Scaled " * beta * " Estimate")),
     title = bquote(bold("Height Models"^"b"))) +
  
  scale_x_discrete(labels = c(bquote("RH"[td]:"Crown Closure"), bquote(RH[td]), 
                              bquote("AHM"[td]:"Crown Closure"), bquote(AHM[td]),
                              bquote("NFFD"[td]:"Crown Closure"), bquote(NFFD[td]), 
                              bquote("EMT"[td]:"Crown Closure"), bquote(EMT[td]), 
                              bquote("EXT"[td]:"Crown Closure"), bquote(EXT[td]),
                              bquote("MAT"[td]:"Crown Closure"), bquote(MAT[td]), 
                              bquote("PAS"[td]:"Crown Closure"), bquote(PAS[td]),
                              bquote("MSP"[td]:"Crown Closure"), bquote(MSP[td]), 
                              bquote("MAP"[td]:"Crown Closure"), bquote(MAP[td]))) +
  
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) + 
  
  scale_fill_manual(values = c("black", "black",
                                 "grey", "grey",
                                 "red", "red", "red", "red",
                                 "pink", "pink", "pink", "pink",
                                 "blue", "blue", "blue",
                                 "lightblue", "lightblue", "lightblue"))  +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray60", linewidth = .05),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        title = element_text(face = "bold"),
        text = element_text(family = "Times"))



sjPlot::plot_models(height_1_models_h$model_2a[[2]], height_1_models_h$model_2a[[3]], height_1_models_h$model_2a[[6]],
                    height_1_models_h$model_2a[[1]], height_1_models_h$model_2a[[8]], height_1_models_h$model_2a[[7]],
                    height_1_models_h$model_2a[[5]], height_1_models_h$model_2a[[4]], height_1_models_h$model_2a[[9]], 
                    rm.terms = c("age", "harvestF [1, 2, 3, 4]"),
                    show.p = TRUE,
                    show.values = TRUE) + ylim(-0.1, 0.15)



mod_test <- lmer(log(height) ~ scale(d_MAT) + tree_cover + harvestF + age + tree_cover*scale(d_MAT) + tree_cover*age + (1|blockF/plotF/splitplotF), 
                  data = regen_height, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

mod <- lmer(log(height) ~ scale(d_MAT) + tree_cover + harvestF + age + (1|blockF/plotF/splitplotF), 
                 data = regen_height, REML = FALSE,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

lrtest(mod, mod_test)

tab_model(mod_test)
r2_nakagawa(mod_test, tolerance = 1e-1000)

mod_test
mod

mod_mega <- lmer(log(height) ~ scale(d_MAT) + tree_cover + harvestF + age + 
                              scale(d_MAT)*tree_cover + 
                 + tree_cover*age + (1|blockF/plotF/splitplotF), 
                 data = regen_height, REML = FALSE,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
mod_mega
tab_model(mod_mega)



scale_colour_manual(labels = c(1:18),
  values = c("black", "black",
                               "grey", "grey",
                               "red", "red", "red", "red",
                               "pink", "pink", "pink", "pink",
                               "blue", "blue", "blue",
                               "lightblue", "lightblue", "lightblue")) 