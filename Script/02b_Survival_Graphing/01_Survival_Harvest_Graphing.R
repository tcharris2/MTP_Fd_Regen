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
