# CONTENT: Graphing - Survival (Harvest) ----------------------------------------------------
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

str(regen_survival)

#### 1.3 Calling RDS file  ------

survival_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                               "2024-02-05_survival_group_harvest_models_NoFutures.rds"))

survival_harvest_models

# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(survival_harvest_models) <- c("ClimaticVarList", "model_0", "model_c", "model_a", 
                                         "model_1", "model_1a", "model_2", "model_2a", 
                                         "model_3", "model_3a")


climatic_models <- survival_harvest_models[c(1:6, 10, 12:13), c("ClimaticVarList", "model_1")]

inter_models <- survival_harvest_models[c(8:9, 11, 15), c("ClimaticVarList", "model_3")]


# 3. Clean up Global env. ------------------------------------------------------

rm(regen, regen_prepped, universalDataPrepFunction)


# 4. Estimated Probability of Survival -----------------------------------------

##### 4.1 Adding data -----

climatic_models$data <- list(regen_survival)

climatic_models

inter_models$data <- list(regen_survival)

# Adding esitmated probability of survival 
climatic_models <- survivalProbs(climatic_models, climatic_models$model_1)

inter_models <- survivalProbs(inter_models, inter_models$model_3)


# 5. Graphing models -----------------------------------------------------------

graphingESTSurvivalProb(climatic_models)



# ggPlots - similar too above function. 

ggplot(data = climatic_models$data[[1]], mapping = aes(x = d_MAT, y = survival_probs)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  labs(title = "d_MAT", x = paste("Climatic Distance"), y = "Estimated Probability of Survival") +
  theme(legend.title=element_blank())

ggplot() + 
  geom_point(data = climatic_models$data[[6]], mapping = aes(x = d_MSP, y = survival_probs, colour = location)) +
  geom_smooth(data = climatic_models$data[[6]], mapping = aes(x = d_MSP, y = survival_probs),
              method = "glm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_smooth(data = climatic_models$data[[6]], mapping = aes(x = d_MSP, y = ReMSP.prob), colour = "red", se = FALSE)+
  labs(title = "d_MSP", x = paste("Climatic Distance"), y = "Estimated Probability of Survival") +
  theme(legend.title=element_blank())





# Manually calculate by hand for model_1 
fixef(climatic_models[[2]][[5]])
ReMSP.log <- 1.3193627 + 0.3536144 * climatic_models[[3]][[1]][["d_MSP"]]
ReMSP.prob <- (exp((ReMSP.log))) / (1+exp((ReMSP.log)))
plot(climatic_models[[3]][[1]][["d_MAT"]], ReMAT.prob, ylim = c(0,1), 
     main = "Survival vs MAT", xlab = "MAP Climatic Distance", ylab = "Survival") ## probabilities

A<-fitted(climatic_models[[2]][[1]]) # These are the probabilities already calculated.
B<-predict(climatic_models[[2]][[1]])
C<-exp(B)/(1+exp(B)) # Using xbeta from predict( ), we can calculate




# using sjPlots ----------------------------------------------------------------


inter_models
climatic_models



sjPlot::plot_model(climatic_models$model_1[[1]], type = "pred", terms = c("d_MAT [all]")) + 
  geom_point(data = climatic_models$data[[1]], mapping = aes(x = d_MAT, y = survival_probs)) +
  labs(x = "MAT Climatic Distance", y = "Estimated Probability of Survival", title = NULL) +
  geom_smooth(data = climatic_models$data[[1]], mapping = aes(x = d_MAT, y = ReMAT.prob), colour = "blue")




sjPlot::plot_model(inter_models$model_3[[3]], type = "pred", terms = c("d_EMT [all]", "harvestF")) + 
      geom_point(data = inter_models$data[[3]], mapping = aes(x = d_EMT, y = survival_probs), 
                 inherit.aes = FALSE, size = 0.5)  

