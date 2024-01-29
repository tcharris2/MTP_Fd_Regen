# Content: Data Prep -----------------------------------------------------------

# Author: Thomson Harris 
# Date: Oct 4th, 2023


# Data Prep Functions ----------------------------------------------------------

dataPrepFunction <- function (df) {
  # Variables as a factor
  df$survival    <- as.factor(df$alive)
  df$locationF   <- as.factor(df$locationNo)
  df$blockF      <- as.factor(df$blockNo)
  df$plotF       <- as.factor(df$plotNo)
  df$splitplotF  <- as.factor(df$splitplotNo)
  df$harvestF    <- as.factor(df$harvestNo)
  df$provenanceF <- as.factor(df$provenanceNo)
  
  # normalizing climate variables
  df <- df %>%
    mutate(across(starts_with("d_"), scale))
  
  df <- df %>% select(-contains(c("p_", "s_")))
  
  
  # blockF is not a significant variables - re-valuing splitplotF <- need to figure out if this a valid method 
  # Until then keep old splitplot variable (Thomson - 19/09/2023)
  
  df
}


