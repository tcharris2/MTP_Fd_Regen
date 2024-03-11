# VISUALIZING REGEN DATA -----------------------------------------------------


# 1. Loading Data -----------------------------------------------------
regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE) 

ClimaticVarList <- names(regen %>% select(starts_with("d_")))


source("Script/01_Universal_Functions/00_universal_data_prep_function.R")


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



str(regen)

library(dplyr)
library(ggplot2)

# 2. Checking Linear relationship ------------------------------------------------

climaticRelationFunc <- function(df) {
  
  for(i in  1:length(ClimaticVarList)) {
    
    VAR <- ClimaticVarList[[i]]
    
    print(ggplot(data = df, mapping = aes(x = scale(.data[[VAR]]), y = log(height))) +
            geom_point(position = "jitter") +
            geom_smooth(formula = y ~ x, se = FALSE) +
            geom_smooth(formula = y ~ poly(x, 2), se = FALSE, colour = "red"))
    
    ggsave(here("Output/Plots", paste(Sys.Date(), VAR,
                 
                 "Linear_Squared_Relationship.pdf", sep = "_")))
    
  }
  
}


climaticRelationFunc(regen_height)

# d_MAT
ggplot(data = regen_height, mapping = aes(x = scale(d_MAT), y = log(height))) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_MWMT
ggplot(data = regen_height, mapping = aes(x = scale(d_MWMT), y = log(height))) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_MCMT
ggplot(data = regen_height, mapping = aes(x = scale(d_MCMT), y = log(height))) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_MAP
ggplot(data = regen_height, mapping = aes(x = scale(d_MAP), y = height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_MSP
ggplot(data = regen_height, mapping = aes(x = d_MSP, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_AHM
ggplot(data = regen_height, mapping = aes(x = d_AHM, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_SHM
ggplot(data = regen_height, mapping = aes(x = d_SHM, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_NFFD
ggplot(data = regen_height, mapping = aes(x = d_NFFD, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_FFP
ggplot(data = regen_height, mapping = aes(x = d_FFP, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_PAS
ggplot(data = regen_height, mapping = aes(x = d_PAS, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_EMT
ggplot(data = regen_height, mapping = aes(x = d_EMT, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_EXT
ggplot(data = regen_height, mapping = aes(x = d_EXT, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_Eref
ggplot(data = regen_height, mapping = aes(x = d_Eref, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_CMD
ggplot(data = regen_height, mapping = aes(x = d_CMD, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")

# d_RH
ggplot(data = regen_height, mapping = aes(x = d_RH, y = ln_height)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE) +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red")


# 3. Survival Linearity --------------------------------------------------------

climate_group <- regen_survival %>% 
  group_by(d_MAT) %>% 
  nest()

climate_group$avg_survival <- NA

for (i in 1:nrow(climate_group)) {
  
  avg_survival <- mean(climate_group$data[[i]][["alive"]])
  
  climate_group$avg_survival[[i]] <- avg_survival
  
}


graphing_df <- subset(climate_group, select = c(d_MAT, avg_survival))


print(ggplot(data = graphing_df, aes(x = scale(VAR), y = avg_survival)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = TRUE, method = "lm") +
  geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red", method = "lm"))


survivalSqrdRelation <- function (df) {
  
  for (VAR in ClimaticVarList) {
    
    climate_group <- df %>% 
      group_by(.data[[VAR]]) %>% 
      nest()
    
    climate_group$avg_survival <- NA
    
    for (i in 1:nrow(climate_group)) {
      
      avg_survival <- mean(climate_group$data[[i]][["alive"]])
      
      climate_group$avg_survival[[i]] <- avg_survival
      
    }
    
    
    print(ggplot(data = climate_group, aes(x = scale(.data[[VAR]]), y = avg_survival)) +
            geom_point() +
            geom_smooth(formula = y ~ x, se = TRUE, method = "lm") +
            geom_smooth(formula = y ~ poly(x, 2), se = TRUE, colour = "red", method = "lm"))
    
    ggsave(here("Output/Plots", paste(Sys.Date(), VAR,
                                      "Survival_Linear_Squared_Relationship.pdf", sep = "_")))
    
  }
  
  
}


survivalSqrdRelation(regen_survival)


# 4. PCA  -----------------------------------------------------

regen_data <- regen[,!names(regen) %in% c("height")]

regen_data <- subset(regen, select = c("d_MAT", "d_MWMT", "d_MCMT", "d_TD", "d_MAP", "d_MSP", "d_AHM", "d_SHM",
                                        "d_DD_0", "d_DD5", "d_DD_18", "d_DD18", "d_NFFD", "d_FFP", "d_PAS",
                                       "d_EMT", "d_EXT", "d_Eref", "d_CMD", "d_RH", "d_CMI", "d_DD1040"))
Species <- regen$species

regen_PCA <- princomp(regen_data, cor = TRUE) # uses a covariance matrix
plot(regen_PCA$scores, pch = 16, col = as.factor(Species))

summary(regen_PCA)

screeplot(regen_PCA, type = "lines")

loadings(regen_PCA)

