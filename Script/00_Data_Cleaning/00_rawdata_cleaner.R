######### CREATING A CLEAN REGEN DATASET ##########
#
# Author: Thomson Harris
# Date: 26/05/2023
#
# The idea of this sheet is to:
# 1. Have a data set that can be subset into smaller projects
# 2. Include a larger array of climate variables
# 3. Have a consistent way of calling different provenances (by seedlot)
# 4. Prepare variables for future analyses
#
##### 1. Loading Data -------------------------------------------------------------
#' @ImportData
regenbase <- read.csv(here("Data/01_Raw", "Regen surveys.csv"), header=TRUE)

summary(regenbase)
str(regenbase)

# make sure to load "plry" first 
library(plyr) # not sure if plyr needs to be loaded (-Thomson 20/09/2023)
library(dplyr)

##### 2. Removing variables and consistent naming --------------------------------
#' @RenameVariables
### Removing variables that are not needed ###

regen <- subset(regenbase, select = -c(X, X.1, X.2, X.3, 
                                       Staple.flag, Plastic.marker, Staple.paint,
                                       Plot, Subplot, Tree..))

# X, X.1, X.2, X.3 are figments left over from using this as an excel file

### Renaming the remaining  variables ### 
rename_vars <- function (){

names(regen)[names(regen) == "Tree.Number"] <- "tree_number"
names(regen)[names(regen) == "Survey.date"] <- "survey_date"
names(regen)[names(regen) == "Location"] <- "location"
names(regen)[names(regen) == "Rep"] <- "rep"
names(regen)[names(regen) == "Tree.cover...."] <- "tree_cover"
names(regen)[names(regen) == "Treat"] <- "harvest"
names(regen)[names(regen) ==  "Cover"] <- "cover"
names(regen)[names(regen) == "Year.planted"] <- "year_planted"
names(regen)[names(regen) == "Genotype"] <- "provenance"
names(regen)[names(regen) == "Species"] <- "species"
names(regen)[names(regen) == "Basal.diam..mm."] <- "basal_dia"
names(regen)[names(regen) == "Height..cm."] <- "height"
names(regen)[names(regen) == "Crown.diameter1..cm."] <- "crown_dia1"
names(regen)[names(regen) == "Crown.diameter2..cm."] <- "crown_dia2"
names(regen)[names(regen) == "Avg.crown.diam..cm."] <- "avg_crown_dia"
names(regen)[names(regen) == "Condition"] <- "condition"
names(regen)[names(regen) == "Damage...notes"] <- "damage_notes"

regen
}

regen <- rename_vars()


##### 3. Cleaning the Dataset -------------------------------------------------
#' @CleanDataset

summary(regen)
head(regen)
tail(regen)

# Removing the tail (string of unwanted NAs)
# Again a figment of working with an excel file
regen <- subset(regen, tree_number >= 1)
summary(regen)

# Removing observations that dont have a provenance assigned to them
regen <- subset(regen, provenance != "")

summary(regen) 
# if not working with provenance check these data points, you may be able to leave them in

###### 3.1 condition -----
unique(regen$condition)
# condition needs to be fixed

regen <- regen %>% 
  mutate(condition = case_when(
    condition == ""  ~ NA,
    condition == "missed"  ~ NA,
    condition == "M    "  ~ "M",
    condition == "G "  ~ "G",
    condition == "M "  ~ "M",
    condition == "P "  ~ "P",
    condition == "d"  ~ "D",
    condition == "G    "  ~ "G",
    condition == "G   "  ~ "G",
    condition == "g"  ~ "G",
    condition == "D "  ~ "D",
      TRUE ~ condition
))

unique(regen$condition)
# Condition is now labeled consistently. Need to deal with NAs
# All condition == NA are alive trees - from checking them manually 

A <- is.na(regen$condition)

regen$condition[A] <- "A"

regen[A,]

# A = alive with unknown condition. It being alive is determined by checking if the tree has any height data or notes attached to it

a <- regen$condition %in% c("M", "G", "P", "N", "A")

regen$alive <- a * 1

unique(regen$condition)

###### 3.2 crown dia -------
# crown_dia2 and avg_crown_dia are character class variables 
class(regen$crown_dia2)
class(regen$avg_crown_dia)

regen$crown_dia2 <- as.numeric(regen$crown_dia2)
regen$avg_crown_dia <- as.numeric(regen$avg_crown_dia)

unique(regen$crown_dia2)
unique(regen$avg_crown_dia)

### Checking damage notes ###
unique(regen$damage_notes)
# this is a colossal mess....
# will need much work before this can be analyzed 
# 1. nested data
# 2. no consistent labeling pattern

###### 3.3 provenance -----
### Checking the provenance in the dataset ###
# List of provenance in the data set
unique(regen$provenance)

# Issues:
# 1. "Lodgepole or Ponderosa pine" 
# 2. "Larch" vs "larch" 
# 3. Larch and pines are different provenance but are labeled as one 

# Removing issue #1 
regen <- subset(regen, provenance != "Lodgepole or Ponderosa pine")
unique(regen$provenance)

# Removing issue #2
regen$provenance[regen$provenance == "larch"] <- "Larch"
regen$provenance[regen$provenance == "Venables future Fd "] <- "Venables future Fd"
unique(regen$provenance)

# Removing issue #3 

regen <- regen%>%mutate(provenance = case_when(
  provenance == "Larch" & location == "Alex Fraser" ~ "Larch_AF",
  provenance == "Larch" & location == "Jaffray" ~ "Larch_JAF",
  provenance == "Larch" & location == "John Prince" ~ "Larch_JP",
  provenance == "Larch" & location == "Narrows" ~ "Larch_NAR",
  provenance == "Larch" & location == "Redfish" ~ "Larch_RED",
  provenance == "Larch" & location == "Twobit" ~ "Larch_TWO",
  
  provenance == "Lodgepole pine" & location == "Alex Fraser" ~ "Lodgepole_AF",
  provenance == "Lodgepole pine" & location == "Jaffray" ~ "Lodgepole_JAF",
  provenance == "Lodgepole pine" & location == "John Prince" ~ "Lodgepole_JP",
  provenance == "Lodgepole pine" & location == "Narrows" ~ "Lodgepole_NAR",
  provenance == "Lodgepole pine" & location == "Redfish" ~ "Lodgepole_RED",
  provenance == "Lodgepole pine" & location == "Twobit" ~ "Lodgepole_TWO",
  
  provenance == "Ponderosa pine" & location == "Alex Fraser" ~ "Ponderosa_AF",
  provenance == "Ponderosa pine" & location == "Jaffray" ~ "Ponderosa_JAF",
  provenance == "Ponderosa pine" & location == "John Prince" ~ "Ponderosa_JP",
  provenance == "Ponderosa pine" & location == "Narrows" ~ "Ponderosa_NAR",
  provenance == "Ponderosa pine" & location == "Redfish" ~ "Ponderosa_RED",
  provenance == "Ponderosa pine" & location == "Twobit" ~ "Ponderosa_TWO",
  
  provenance == "Venables future Fd " ~ "Venables future Fd",
  TRUE ~ provenance
))

unique(regen$provenance)
unique(regenbase$Genotype)


####### 3.4 harvest -----
unique(regen$harvest)
# issue with harvest (60ret(only 1), 60Ret ???, 77)

# viewing rows with mislabeled data 
regen[regen$harvest == "77", ]
regen[regen$harvest == "60ret", ]
regen[regen$harvest == "60Ret ???", ]

regen <- subset(regen, harvest != c("77", "60Ret ???"))
unique(regen$harvest)
# shows warning but this is fine it does its job correctly in this instance
# Delete these values if harvest is an important variable in your data analysis. 
# If not they can be left. 

regen$harvest[regen$harvest == "60ret"] <- "60Ret"
unique(regen$harvest)

# harvest is fixed 


### Checking to see if there is any other issues with the dataset ###
unique(regen$location)
unique(regen$rep)
unique(regen$tree_cover)
# some NAs <- might be an issue later 
unique(regen$cover)
# some "" <- will  be an issue for later 
unique(regen$year_planted)
unique(regen$species)

###### 3.5 fixing species -------

regen[regen$species == "Pl or Py", ] # <- removed earier this error also popped up in "provenance" 
regen[regen$species == "Fd or Lw", ] # <- rename to "Fd" the provenance is John Prince B class Fd
regen[regen$species == "Fd or Py", ] # <- rename to "Py" the provenance is Ponderosa 
regen[regen$species == "Lw (yes)", ] # <- not sure what to do here, provenance is Jaffray B class Fd, probably delete (Deleted)
regen[regen$species == "", ]         # <- rename to "Fd" they are all Fd provs  
regen[regen$species =="Fd       " , ] # <- rename to "Fd"
regen[regen$species == "Fd           " , ] # <- rename to "Fd"
regen[regen$species == "Fd           " , ] # <- rename to "Fd"
regen[regen$species == "fd", ] # <- rename to "Fd"
regen[regen$species == "PL", ] # <- rename to "Pl"


regen <- regen %>% mutate(species = case_when(
  species == "Fd or Lw"  ~ "Fd",
  species == "Fd or Py"  ~ "Py",
  species == ""  ~ "Fd",
  species == "Fd       "  ~ "Fd",
  species == "Fd           "  ~ "Fd",
  species == "fd"  ~ "Fd",
  species == "PL"  ~ "Pl",
  species == "Fd "  ~ "Fd",
  TRUE ~ species
))

regen <- subset(regen, species != "Lw (yes)")


unique(regen$species)

summary(regen)

# Some provenances and species do not line up
# Unable to tell what these trees are so they will be removed. 

check<-regen %>%
  group_by(ID_tag,species) %>%
  summarise( alive= n())

check.2<-regen %>%
  group_by(provenance,species) %>%
  summarise( alive= n())
# should be 33 total obs here

fix_prov_species_disconnect <- function() {
  regen <- regen[!(regen$provenance == "Alex Fraser B class Fd" & regen$species =="Py" ), ] 
  regen <- regen[!(regen$provenance == "Jaffray B class Fd" & regen$species == "Lw"), ]
  regen <- regen[!(regen$provenance == "Jaffray future Fd" & regen$species == "Lw"), ]
  regen <- regen[!(regen$provenance == "John Prince B class Fd" & regen$species == "Pl"), ]
  regen <- regen[!(regen$provenance == "John Prince B class Fd" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Larch_JAF" & regen$species == "Fd"), ]
  regen <- regen[!(regen$provenance == "Lodgepole_AF" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Lodgepole_JAF" & regen$species == "Fd"), ]
  regen <- regen[!(regen$provenance == "Lodgepole_JAF" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Lodgepole_JP" & regen$species == "Fd"), ]
  regen <- regen[!(regen$provenance == "Lodgepole_JP" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Peterhope B class Fd" & regen$species == "Pl"), ] 
  regen <- regen[!(regen$provenance == "Ponderosa_JAF" & regen$species == "Fd"), ] 
  regen <- regen[!(regen$provenance == "Ponderosa_JAF" & regen$species == "Lw"), ] 
  regen <- regen[!(regen$provenance == "Ponderosa_JAF" & regen$species == "Pl"), ] 
  regen <- regen[!(regen$provenance == "Ponderosa_JP" & regen$species == "Fd"), ] 
  regen <- regen[!(regen$provenance == "Redfish A class Fd" & regen$species == "Pl"), ] 
  regen <- regen[!(regen$provenance == "Redfish A class Fd" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Redfish B class Fd" & regen$species == "Lw"), ]
  regen <- regen[!(regen$provenance == "Redfish B class Fd" & regen$species == "Py"), ]
  regen <- regen[!(regen$provenance == "Venables future Fd" & regen$species == "Py"), ]
  
  regen
  
}

regen <- fix_prov_species_disconnect()

summary(regen)

##### 4. Writing a New .CSV File -----------------------------------------------
#' @WriteCSV
write.csv(regen, file = here("Data/03_Processed" , "20231128_regen_cleaned.csv"), row.names = FALSE)


#### 5. Double Checking Dataset --------------------------------------------
#' @ReadCSV
regencleaned <- read.csv(here("Data/03_Processed", "20231128_regen_cleaned.csv"), header=TRUE)

###### 5.1 Check variables -------
unique(regencleaned$location)
unique(regencleaned$rep)
unique(regencleaned$harvest)
unique(regencleaned$year_planted)
unique(regencleaned$provenance)
unique(regencleaned$species)
unique(regencleaned$condition)
unique(regencleaned$alive)

# 6. Adding Location/Block/Plot/Splitplot ---------------------------------------
#' @RandomEffects

###### 6.1  provenanceNo -----
regencleaned$provenanceNo <- as.factor(regencleaned$provenance)
regencleaned$provenanceNo <- as.integer(regencleaned$provenanceNo)

###### 6.2 harvestNo -----
names(regencleaned)[names(regencleaned) == "harvest"] <- "harvest_name"

regencleaned$harvestNo <- NA

regencleaned <- regencleaned %>%
  mutate(harvestNo = case_when(
    harvest_name == "clearcut"  ~ 1,
    harvest_name == "seed"  ~ 2,
    harvest_name == "30Ret"  ~ 3,
    harvest_name == "60Ret"  ~ 4,
    TRUE ~ harvestNo
  ))


###### 6.3 locationNo -----
regencleaned$locationNo <- as.factor(regencleaned$location) 
regencleaned$locationNo <- as.integer(regencleaned$locationNo)

###### 6.4 blockNo -----
regencleaned$blockNo <- NA
  
regencleaned <- regencleaned%>%mutate(blockNo = case_when(
  rep == "1" & location == "Alex Fraser" ~ 1,
  rep == "2" & location == "Alex Fraser" ~ 2,
  rep == "3" & location == "Alex Fraser" ~ 3,
  rep == "1" & location == "Jaffray" ~ 4,
  rep == "2" & location == "Jaffray" ~ 5,
  rep == "3" & location == "Jaffray" ~ 6,
  rep == "1" & location == "John Prince" ~ 7,
  rep == "2" & location == "John Prince" ~ 8,
  rep == "3" & location == "John Prince" ~ 9,
  rep == "1" & location == "Narrows" ~ 10,
  rep == "1" & location == "Redfish" ~ 11,
  rep == "2" & location == "Redfish" ~ 12,
  rep == "3" & location == "Redfish" ~ 13,
  rep == "4" & location == "Redfish" ~ 14,
  rep == "1" & location == "Twobit" ~ 15,
  TRUE ~ blockNo
))


unique(regencleaned$blockNo)

###### 6.5  plotNo -----
regencleaned$plotNo <- NA
regencleaned$plotNo <- (regencleaned$blockNo * 100 + regencleaned$harvestNo)

unique(regencleaned$plotNo)


###### 6.6 splitplotNo -----
regencleaned$splitplotNo <- (regencleaned$locationNo * 10000) + (regencleaned$blockNo * 100) + 
  (regencleaned$harvestNo * 10) + (regencleaned$provenanceNo)

summary(regencleaned)


##### 7. Writing a Final .CSV File -----------------------------------------------
#' @WriteCSV
write.csv(regencleaned, file = here("Data/03_Processed" , "20231129_regen_cleaned.csv"), row.names = FALSE)

