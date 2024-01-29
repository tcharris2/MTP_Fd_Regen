# Content: Data Prep -----------------------------------------------------------

# Author: Thomson Harris 
# Date: Oct 4th, 2023


# Data Prep Functions ----------------------------------------------------------

dataPrepFunction <- function () {
  # Variables as a factor
  regen$survival    <- as.factor(regen$survival)
  regen$locationF   <- as.factor(regen$locationNo)
  regen$blockF      <- as.factor(regen$blockNo)
  regen$plotF       <- as.factor(regen$plotNo)
  regen$splitplotF  <- as.factor(regen$splitplotNo)
  regen$harvestF    <- as.factor(regen$harvestNo)
  regen$provenanceF <- as.factor(regen$provenanceNo)
  
  # normalizing climate variables
  regen <- regen %>%
    mutate(across(starts_with("d_"), scale))
  
  # blockF is not a significant variables - re-valuing splitplotF <- need to figure out if this a valid method 
  # Until then keep old splitplot variable (Thomson - 19/09/2023)
  
  regen$splitplotNo <- (regen$locationNo * 10000) + (regen$block * 100) +
    (regen$harvestNo * 10) + (regen$provenanceNo)
  
  regen$splitplotF <- as.factor(regen$splitplotNo)
  
  regen
}


