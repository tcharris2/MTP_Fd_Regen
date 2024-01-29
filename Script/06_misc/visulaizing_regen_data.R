#####################################
###### Visulaizing Regen Data #######
#####################################

##### Cleaning Up #####
rm(list=ls(all=TRUE)) #remove any objects left from previous R runs

##### Loading Data #####
regen <- read.csv(here("Data/03_Processed", "survival_fd_b.csv"), header = TRUE)

regen_harvest_height <- subset(regen, !(is.na(height)))

summary(regen)

library(dplyr)
library(ggplot2)



ggplot(data = regen_harvest_height, mapping = aes(x = d_AHM, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_CMD, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_CMI, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_DD_0, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_DD_18, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_DD18, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_DD5, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_EMT, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_Eref, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_EXT, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_FFP, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_MAP, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_MAT, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_MCMT, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_MSP, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_MWMT, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_NFFD, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_PAS, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_RH, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_SHM, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)

ggplot(data = regen_harvest_height, mapping = aes(x = d_TD, y = height)) +
  geom_point() +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ location, nrow = 2)



ggplot(data = regen_harvest_height, mapping = aes(x = d_AHM, y = mean(height))) +
  geom_point() +
  facet_wrap(. ~ location, nrow = 2)


#### plots #####

# height bar graph
ggplot(data = regen) + 
  geom_bar(mapping = aes(x = height), width = 3)

ggplot(data = regen) + 
  geom_bar(mapping = aes(x = log(height)), width = 0.2)

ggplot(data = regen) + 
  geom_bar(mapping = aes(x = d_MAP), width = 10)

# height by crown diameter
ggplot(data = regen) +
  geom_point(mapping = aes(x = avg_crown_dia, y = height, colour = species))

# height by basal diameter
ggplot(data = regen) +
  geom_point(mapping = aes(x = basal_dia, y = height, colour = species))

# height by location
ggplot(data = regen) +
  geom_point(mapping = aes(x = locationF, y = height, colour = species), position = "jitter") 

ggplot(data = regen) +
  geom_point(mapping = aes(x = locationF, y = height), position = "jitter") +
  facet_wrap(. ~ species, nrow = 2)

ggplot(data = regen) +
  geom_point(mapping = aes(x = locationF, y = height, colour = species), position = "jitter") +
  facet_wrap(. ~ harvest, nrow = 2)

# height by tree cover 
ggplot(data = regen, mapping = aes(x = log(tree_cover + 1), y = height)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  facet_wrap(. ~ species, nrow = 2)

## climate variables   
ggplot(data = regen, mapping = aes(x = d_MAT, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_MWMT, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_MCMT, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()  

ggplot(data = regen, mapping = aes(x = d_TD, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_MAP, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_MSP, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_AHM, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_SHM, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_DD_0, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_DD5, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_DD_18, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_DD18, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_NFFD, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_FFP, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_PAS, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_EMT, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_EXT, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_Eref, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_CMD, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_RH, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_CMI, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth()

ggplot(data = regen, mapping = aes(x = d_DD1040, y = height)) +
  geom_point(mapping = aes(colour = species), position = "jitter") +
  geom_smooth(data = filter(regen, species == "Fd"), colour = "black", se = FALSE, method = "loess")

ggplot(data = regen, mapping = aes(x = d_DD1040, y = height)) +
  geom_point(position = "jitter") +
  geom_smooth(mapping = aes(colour = species), se = FALSE, method = "loess") +
  facet_wrap(. ~ harvestF, nrow = 2)

### PCA #### 

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

## survival ###

sum(regen$alive)

a <- regen %>% count(alive)

d <- a[a$alive == 0, ]

d$n/sum(a$n)

ggplot(data = regen) +
  geom_point(mapping = aes(x = harvest, y = alive, colour = location), position = "jitter")
  

  
