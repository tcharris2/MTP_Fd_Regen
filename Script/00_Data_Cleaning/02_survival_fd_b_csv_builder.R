# SURVIVAL FD B CLASS CSV BUILDER -----------------------------------------

# 1. Importing Data ---------------------------------------------------------------

### 1.1. Loading Packages ----
library(here)
library(tidyverse)

### 1.2. Loading Data -----
#' @ImportData
survival_fd_b_base <- read.csv(here("Data/03_Processed", "20240602_regen_merged.csv"), header=TRUE)


# 2. Removing all species except for Fd for survival ------------------------------
#' @RemoveSpecies
survival_fd_b_base <- survival_fd_b_base[survival_fd_b_base$species == "Fd", ]


# 3. Identifying Design Issues ----------------------------------------------------
#' @DesignIssues

# DESIGN ISSUES WERE IDENTIFIED IN A R SCRIPT THAT NO LONGER EXISTS 
# see /Data/experimental design issues.csv for list of blocks with issues 

# - RED block 12 no CC 
# - Venables B class fd "46129_Fd_B" missing in AF blocks 2, 3. 
# - Venables future Fd "53751_Fd_B" missing in AF block 3.
# - Alex Fraser Fd A class "63201_Fd_A" missing in AF block 2.
# - Alex Fraser B class Fd "8491_Fd_B" missing in JP block 9. 
# - John Prince B class Fd "53614_Fd_B" missing in RED block 13. 
# - Alex Fraser future Fd "39259_Fd_B" missing in all RED blocks - not sure why this is an issue
# - Same issue for Peterhope futures - again not sure why this is an issue

###### 3.1 solutions ----

# Remove RED block 12 and 13
# Remove JP block 9
# Remove Venables provs from data set
# Remove ALL futures 
# Remove all A class 

# 4. Removing Design Issues -------------------------------------------------------
#' @DesignIssues_2

# remove block 12
survival_fd_b_df <- survival_fd_b_base[!survival_fd_b_base$block == 9, ]
survival_fd_b_df <- survival_fd_b_df[!survival_fd_b_df$block == 12, ]
survival_fd_b_df <- survival_fd_b_df[!survival_fd_b_df$block == 13, ]

# Keeping only B class Fd provenances that are across all locacations/blocks/plots
# ID_tags "8491_Fd_B"  "53205_Fd_B" "53614_Fd_B" "19913_Fd_B" "44771_Fd_B"
# Identified from experimental design checks. 

ID_Tag_list <- c("8491_Fd_B",  "53205_Fd_B", "53614_Fd_B", "19913_Fd_B", "44771_Fd_B")

survival_fd_b_df <- subset(survival_fd_b_df, survival_fd_b_df$ID_tag %in% ID_Tag_list)

# Remove NA's in tree cover
survival_fd_b_df <- subset(survival_fd_b_df, !(is.na(tree_cover)))

###### 4.1 new dataframe with fixed desgin issues ------
survival_fd_b <- survival_fd_b_df

# 5. Adding Binary Damage Variable ----------------------------------------------

sum(survival_fd_b$damage_notes == "")
survival_fd_b[survival_fd_b$damage_notes == "", ]
survival_fd_b$damage <- (survival_fd_b$damage_notes != "") * 1
# "damage" denotes trees that are damaged as 1 and not damaged as 0 

survival_fd_b <- survival_fd_b %>% mutate(damage = case_when(
  condition == "D" ~ NA,
  TRUE ~ damage 
))
# only keeping damage when the trees are alive 

# 6. Adding Survival variable ------------------------------------------------------

survival_fd_b$survival <- survival_fd_b$alive 

# 7. Adding Age as a Variable -----------------------------------------------------
#' @AddAge 

survival_fd_b$survey_year <- NA

year_2021 <- "2021"
year_2020 <- "2020"

survival_fd_b[grep(year_2021, survival_fd_b$survey_date, value = F), "survey_year"] <- 2021
survival_fd_b[grep(year_2020, survival_fd_b$survey_date, value = F), "survey_year"] <- 2020

survival_fd_b$survey_year
unique(survival_fd_b$survey_year) # there is NAs present 
survival_fd_b[is.na(survival_fd_b$survey_year), ] # the NA present is a incorrect entry at Redfish. year = 2022
survival_fd_b[survival_fd_b$location == "Redfish", ] # All of Redfish was surveyed in year 2020 this is a incorrect entry and can be corrected to 2020
survival_fd_b$survey_date[survival_fd_b$survey_date == "08-Jul-2022"] <- "08_Jul-2020" 

survival_fd_b[grep(year_2020, survival_fd_b$survey_date, value = F), "survey_year"] <- 2020

unique(survival_fd_b$year_planted)
class(survival_fd_b$year_planted)
class(survival_fd_b$survey_year)
survival_fd_b$age <- survival_fd_b$survey_year - survival_fd_b$year_planted
unique(survival_fd_b$age)

# 8. Reordering Variables ----------------------------------------------------------


col_order  <- c("tree_number", "location", "locationNo", "blockNo", "plotNo",
           "splitplotNo", "rep", "harvest_name", "harvestNo",  "species", "provenance", 
           "provenanceNo",  "ID_tag", "seedlot", "alive", "survival", "height", "basal_dia",
           "crown_dia1", "crown_dia2", "avg_crown_dia", "damage", "damage_notes",
           "condition", "cover", "tree_cover","survey_date", "survey_year", "year_planted", "age",
           names(survival_fd_b %>% select(starts_with("p_"))),
           names(survival_fd_b %>% select(starts_with("s_"))),
           names(survival_fd_b %>% select(starts_with("d_")))
)


survival_fd_b <- survival_fd_b[, col_order]
names(survival_fd_b)


# 9. Writing a new CSV File ----------------------------------------------------

write.csv(survival_fd_b, file = here("Data/03_Processed" ,  paste0(Sys.Date(), "_survival_fd_b_processed.csv"), row.names = FALSE))
          



