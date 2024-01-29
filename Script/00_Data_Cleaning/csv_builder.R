# GENERIC CSV BUILDER --------------------------------------------------------

###### 0.1 test ----

# 1. Importing the cleaned dataset ------------------------------------------
#' @ImportData

regen_merged <- read.csv(here("Data/03_Processed", "regen_merged.csv"), header = TRUE)

# Create a df for height -------------------------------------------------------
#' @HeightDataFrame
# contains all species and does not fix experimental design issues if present

height_merged <- subset(regen_merged, alive == 1)

summary(height_merged)

height.na <- height_merged[is.na(height_merged$height), ]

height_final <- height_merged[!is.na(height_merged$height), ]

write.csv(height_final, file = "height_final.csv", row.names = FALSE)

# Removing all species except for Fd for survival ------------------------------
#' @RemoveSpecies

regen_merged_fd <- regen_merged[regen_merged$species == "Fd", ]

# Identifying/removing issues in the Fd dataset --------------------------------
#' @DesignIssues

# - RED block 12 no CC 
# - Venables B class fd "46129_Fd_B" missing in AF blocks 2, 3. 
# - Venables future Fd "53751_Fd_B" missing in AF block 3.
# - Alex Fraser Fd A class "63201_Fd_A" missing in AF block 2.
# - Alex Fraser B class Fd "8491_Fd_B" missing in JP block 9. 
# - John Prince B class Fd "53614_Fd_B" missing in RED block 13. 
# - Alex Fraser future Fd "39259_Fd_B" missing in all RED blocks - not sure why this is an issue
# - Same issue for Peterhope futures - again not sure why this is an issue

# Solutions 

# Remove RED block 12
# Remove Venables provs from data set
# Remove AF future from RED
# Remove PH future from RED
# Remove all A class 

# Dont think missing a prov from select blocks is an issue because no longer comparing 
# the differences between provs, ie not testing between levels. 
# Leaving in JP block 9 and RED block 13

# remove block 12
survival_fd <- regen_merged_fd[!regen_merged_fd$block == 12, ]

# keep only B class   
survival_fd <- survival_fd[ends_with("B", vars = survival_fd$ID_tag),]
unique(survival_fd$ID_tag)

# remove venables provenances 
survival_fd_1 <- survival_fd[contains("Venables", vars = survival_fd$provenance), ]

survival_fd <- anti_join(survival_fd, survival_fd_1, by = join_by(provenance))
# janky way to do this but couldnt figure out how to subset with just the first function

# removing AF future and PH future from RED
survival_fd <- survival_fd[!(survival_fd$ID_tag == "39259_Fd_B" & survival_fd$location =="Redfish" ),]

survival_fd <- survival_fd[!(survival_fd$ID_tag == "53751_Fd_B" & survival_fd$location =="Redfish" ),]

# Survival dataset is all cleaned at this point and ready to use 
# Writing CSV file


# Adding age as a variable -----------------------------------------------------
#' @AddAge 

survival_fd$survey_year <- NA

year_2021 <- "2021"
year_2020 <- "2020"

survival_fd[grep(year_2021, survival_fd$survey_date, value = F), "survey_year"] <- 2021
survival_fd[grep(year_2020, survival_fd$survey_date, value = F), "survey_year"] <- 2020

survival_fd$survey_year
unique(survival_fd$survey_year) # there is NAs present 
survival_fd[is.na(survival_fd$survey_year), ] # the NA present is a incorrect entry at Redfish. year = 2022
survival_fd[survival_fd$location == "Redfish", ] # All of Redfish was surveyed in year 2020 this is a incorrect entry and can be corrected to 2020
survival_fd$survey_date[survival_fd$survey_date == "08-Jul-2022"] <- "08_Jul-2020" 

unique(survival_fd$year_planted)
class(survival_fd$year_planted)
class(survival_fd$survey_year)
survival_fd$age <- survival_fd$survey_year - survival_fd$year_planted
unique(survival_fd$age)


# Adding splitplot for survival ----------------------------------------------

survival_fd$provenanceF <- as.factor(survival_fd$provenance)
survival_fd$provenanceNo <- as.integer(survival_fd$provenanceF)

survival_fd$splitplotNo <- (survival_fd$locationNo * 10000) + (survival_fd$block * 100) + 
                            (survival_fd$harvestNo * 10) + (survival_fd$provenanceNo)

# 10241 means location 1, block 2, harvest 4, provenance 1

unique(survival_fd$splitplotNo)


# Writing csv for survival_fd_b --------------------------------------

write.csv(survival_fd, file = here("Data/03_Processed", "survival_fd_b.csv"), row.names = FALSE)


# reordering all varaibles 

# re-ordering variable columns 
regen$splitplotF <- as.factor(regen$splitplotNo)

col_order <- c("tree_number", "survival", "location", "locationNo", "locationF", "block", "blockF", "rep", "plot", "plotF",
               "splitplotNo", "splitplotF", "harvest_name", "harvestNo", "harvestF", "species", "provenance", 
               "provenanceNo", "provenanceF", "ID_tag", "seedlot", "alive", "height", "basal_dia",
               "crown_dia1", "crown_dia2", "avg_crown_dia", "damage", "damage_notes", "condition", "cover", "tree_cover", 
               "d_MAT", "d_MWMT", "d_MCMT", "d_TD", "d_MAP", "d_MSP", "d_AHM", "d_SHM", "d_DD_0", "d_DD5", "d_DD_18", 
               "d_DD18", "d_NFFD", "d_FFP", "d_PAS", "d_EMT", "d_EXT", "d_Eref", "d_CMD", "d_RH", "d_CMI", "d_DD1040", 
               "survey_date", "year_planted")
regen.1 <- regen[, col_order]

regen.1$locationF <- as.factor(regen.1$locationNo)
regen.1$provenanceF <- as.factor(regen.1$provenanceNo)

regen.1 <- regen.1 %>% select(-c("repF"))

write.csv(regen.1, file = "survival_fd_b.csv", row.names = FALSE)









# -----------------------------------------------------------------
unique(regen_merged$ID_tag)

survival <- regen_merged[!regen_merged$block %in% c(2, 3, 9, 7, 12, 13), ]


by_plot <- survival %>% 
  group_by(plotF, ID_tag) %>% 
  nest()

by_plot.1 <- survival %>%
  group_by(plotF, ID_tag, provenance, location) %>%
  summarise( alive= n())

survival <- survival[!(survival$ID_tag == "39259_Fd_B" & survival$location =="Redfish" ), ]
regen_merged[(regen_merged$ID_tag == "39259_Fd_B" & regen_merged$location =="Redfish" ), ]

by_plot.2 <- survival %>%
  group_by(plotF, ID_tag, provenance, location) %>%
  summarise( alive= n())

survival <- survival[!(survival$provenance == "Venables B class Fd"), ]

survival <- survival[!(survival$provenance == "Redfish B class Fd"), ]
# not the proper way to do this. Redo. 
