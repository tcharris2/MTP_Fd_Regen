# Start up Script ---------------------------

# Load all Appropriate Packages --------------------------
library_setup <- function() {
library(here)
library(modelr)
library(tidyverse) # Includes ggplot2 and dplyr
library(lmtest)
library(lme4)
library(broom.mixed)

}

library_setup()

c(Sys.getenv("R_PROFILE_USER"), file.path(getwd(),".Rprofile"))

install.packages("usethis")

usethis::edit_r_profile()
