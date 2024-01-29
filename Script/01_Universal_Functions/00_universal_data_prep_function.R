# Content: Data Prep -----------------------------------------------------------

# Author: Thomson Harris 
# Date: Oct 4th, 2023


# Data Prep Functions ----------------------------------------------------------

universalDataPrepFunction <- function (df) {
  # Variables as a factor
  df$survival    <- as.factor(df$survival)
  df$locationF   <- as.factor(df$locationNo)
  df$blockF      <- as.factor(df$blockNo)
  df$plotF       <- as.factor(df$plotNo)
  df$splitplotF  <- as.factor(df$splitplotNo)
  df$harvestF    <- as.factor(df$harvestNo)
  df$provenanceF <- as.factor(df$provenanceNo)
  
  # normalizing climate variables
  df <- df %>%
    mutate(across(starts_with("d_"), scale))
  
  # Removing provenance and site climatic variables 
  df <- df %>% select(-contains(c("p_", "s_")))
  
  # Function output
  return(df)
  
}
