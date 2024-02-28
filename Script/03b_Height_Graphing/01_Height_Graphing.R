# CONTENT: Graphing - Height ----------------------------------------------------
# ALL LOCATIONS -------------------------------------------------------

# Author: Thomson Harris
# Date Started: Feb 13th, 2024

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 
# check for most recent CSV file

ClimaticVarList <- names(regen %>% select(starts_with("d_")))

ClimaticVarList[[1]] <- "d_MAT_2"
ClimaticVarList[[2]] <- "d_MWMT_2"
ClimaticVarList[[3]] <- "d_MCMT_2"
ClimaticVarList[[4]] <- "d_MAP_2"
ClimaticVarList[[8]] <- "d_NFFD_2"
ClimaticVarList[[9]] <- "d_FFP_2"
ClimaticVarList[[10]] <- "d_PAS_2"
ClimaticVarList[[11]] <- "d_EMT_2"
ClimaticVarList[[12]] <- "d_EXT_2"
ClimaticVarList[[13]] <- "d_Eref_2"
ClimaticVarList[[14]] <- "d_CMD_2"




# 2. Importing Functions ----------------------------------------------------------

source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


# 3. Correcting Variable types ----------------------------------------------------

# Adding Squared Climatic Term
regen$d_MAT_2 <- (regen$s_MAT)^2 - (regen$p_MAT)^2
regen$d_MWMT_2 <- (regen$s_MWMT)^2 - (regen$p_MWMT)^2
regen$d_MCMT_2 <- (regen$s_MCMT)^2 - (regen$p_MCMT)^2
regen$d_MAP_2 <- (regen$s_MAP)^2 - (regen$p_MAP)^2
#regen$d_MSP_2 <- (regen$s_MSP)^2 - (regen$p_MSP)^2
#regen$d_AHM_2 <- (regen$s_AHM)^2 - (regen$p_AHM)^2
#regen$d_SHM_2 <- (regen$s_SHM)^2 - (regen$p_SHM)^2
regen$d_NFFD_2 <- (regen$s_NFFD)^2 - (regen$p_NFFD)^2
regen$d_FFP_2 <- (regen$s_FFP)^2 - (regen$p_FFP)^2
regen$d_PAS_2 <- (regen$s_PAS)^2 - (regen$p_PAS)^2
regen$d_EMT_2 <- (regen$s_EMT)^2 - (regen$p_EMT)^2
regen$d_EXT_2 <- (regen$s_EXT)^2 - (regen$p_EXT)^2
regen$d_Eref_2 <- (regen$s_Eref)^2 - (regen$p_Eref)^2
regen$d_CMD_2 <- (regen$s_CMD)^2 - (regen$p_CMD)^2
#regen$d_RH_2 <- (regen$s_RH)^2 - (regen$p_RH)^2

# Prepping Data

# Universal prep
regen_prepped <- universalDataPrepFunction(regen)


# Height specific prep
regen_prepped <- subset(regen_prepped, !regen_prepped$tree_number %in% c(3904, 9861, 8248, 12846, 13432, 14752))

regen_prepped$ln_height <- log(regen_prepped$height)

regen_height <-  subset(regen_prepped, !(is.na(height)))

regen_height <- subset(regen_height, !(is.na(tree_cover)))

regen_height$sqrt_tree_cover <- sqrt(regen_height$tree_cover)

# Removing Futures
regen_height <- subset(regen_height, !regen_height$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                                     "Peterhope future Fd", "Alex Fraser future Fd", 
                                                                     "Twobit B class Fd"))




str(regen_height)


# 2. Calling RDS files ------------------------------------------------



ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                      "2024-02-27_ln_height_group_harvest_models_sqrd_NoFutures.rds"))

ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                "2024-02-27_ln_height_group_harvest_models_NoFutures.rds"))


names(ln_height_harvest_models)


names(ln_height_cover_models)


# 2. Selecting Best Models ---------------------------------------------------

# See 2024-02-06_MODEL_SLECTION_survival_Harvest_group_sig_p_vals_NoFutures.csv for list of selected models

colnames(ln_height_harvest_models) <- c("model_0", "model_h", "model_a", "model_ah",
                                              "model_1", "model_1a", "model_2", "model_2a", 
                                              "model_3", "model_3a", 
                                              "ClimaticVarList")

colnames(ln_height_cover_models) <- c("model_0", "model_c", "model_a", "model_ac",
                                              "model_1", "model_1a", "model_2", "model_2a", 
                                              "model_3", "model_3a", 
                                              "ClimaticVarList")


harvest_2a_models <- ln_height_harvest_models[c(2, 4:6, 8, 10:11, 15), c("ClimaticVarList", "model_2a")]

cover_2a_models <- ln_height_cover_models[c(5:6, 10), c("ClimaticVarList", "model_2a")]

cover_3a_models <- ln_height_cover_models[c(1:4, 7:9, 11:13, 15), c("ClimaticVarList", "model_3a")]


# Graphing -----------------------------------
graphHeighBeta(cover_3a_models)


# Testing graph design ----------------
sjPlot::plot_model(harvest_2a_models[["model_2a"]][[1]],
                   type = "std", show.p = TRUE, show.values = TRUE,
                   axis.labels=c("Age", "60Ret", "30Ret", "SeedTree", "MWMT")) +
  
  ylim(-0.911, 0.911) +
  geom_hline(yintercept = 0, colour = "green4") +
  
  labs(y = "Standarized Beta Coefficents",
       title = "MWMT Beta Estimates") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray40", linewidth = .05),
        panel.grid.minor = element_blank()) 

graphHeighBeta(cover_3a_models)



