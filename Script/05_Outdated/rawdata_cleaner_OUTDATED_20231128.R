######### Creating a regendata master sheet ##########
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
##### Loading Data -------------------------------------------------------------
#' @ImportData
regenbase <- read.csv(here("Data/Raw", "Regen surveys.csv"), header=TRUE)

summary(regenbase)
str(regenbase)

# make sure to load "plry" first 
library(plyr) # not sure if plyr needs to be loaded (-Thomson 20/09/2023)
library(dplyr)

###### Removing variables and consistent naming --------------------------------
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


###### Cleaning the Dataset #####
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

### Checking condition ###
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

# crown_dia2 and avg_crown_dia are character class variables 
unique(regen$crown_dia2)
unique(regen$avg_crown_dia)

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

sum(regen$damage_notes == "")
regen[regen$damage_notes == "", ]
regen$damage <- (regen$damage_notes != "") * 1
# "damage" denotes trees that are damaged as 1 and not damaged as 0 

regen <- regen %>% mutate(damage = case_when(
  condition == "D" ~ NA,
  TRUE ~ damage 
))
# only keeping damage when the trees are alive 

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


### Checking Harvest Levels ###
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


fix_species <- function () {
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

}

regen <- fix_species()

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

fix_prov.species_disconnect <- function() {
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

regen <- fix_prov.species_disconnect()

##### Writing a new .csv file #######

## adding blocking effect

regen$locationF <- as.factor(regen$location)
class(regen$locationF)
regen$locationNo <- as.integer(regen$locationF)

regen$harvestF <- as.factor(regen$harvest)
regen$harvestNo <- as.numeric(regen$harvestF)
regen$repF <- as.factor(regen$rep)

### Creating Random Effect Variables ###

# blocking effect 

create_blocks <- function (){
  regen$block <- NA
  
  regen <- regen%>%mutate(block = case_when(
    repF == "1" & location == "Alex Fraser" ~ 1,
    repF == "2" & location == "Alex Fraser" ~ 2,
    repF == "3" & location == "Alex Fraser" ~ 3,
    repF == "1" & location == "Jaffray" ~ 4,
    repF == "2" & location == "Jaffray" ~ 5,
    repF == "3" & location == "Jaffray" ~ 6,
    repF == "1" & location == "John Prince" ~ 7,
    repF == "2" & location == "John Prince" ~ 8,
    repF == "3" & location == "John Prince" ~ 9,
    repF == "1" & location == "Narrows" ~ 10,
    repF == "1" & location == "Redfish" ~ 11,
    repF == "2" & location == "Redfish" ~ 12,
    repF == "3" & location == "Redfish" ~ 13,
    repF == "4" & location == "Redfish" ~ 14,
    repF == "1" & location == "Twobit" ~ 15,
    TRUE ~ block
  ))
}

regen <- create_blocks()
regen$blockF <- as.factor(regen$block)

unique(regen$block)
summary(regen$block)

# plot effect 

regenclean$plot <- NA
regenclean$plot <- paste(regenclean$harvestNo + regenclean$block * 100)

unique(regenclean$plot)

regen$plotF <- as.factor(regen$plot)


write.csv(regen, file = "regen_cleaned.csv", row.names = FALSE)

# Further cleaning and variable editing ------------------------
regenclean <- read.csv("regen_cleaned.csv", header=TRUE)

# relabeling harvests 
regenclean <- select(regenclean, -c(harvestF, harvestNo))
names(regenclean)[names(regenclean) == "harvest"] <- "harvest_name"

regenclean$harvestNo <- NA

regenclean <- regenclean %>%
  mutate(harvestNo = case_when(
    harvest_name == "clearcut"  ~ 1,
    harvest_name == "seed"  ~ 2,
    harvest_name == "30Ret"  ~ 3,
    harvest_name == "60Ret"  ~ 4,
      TRUE ~ harvestNo
  )
)

regenclean$harvestF <- as.factor(regenclean$harvestNo)

# relabeling plot
regenclean <- select(regenclean, -c(plot, plotF))

unique(regenclean.1$plot)

# plot effect 

regenclean$plot <- NA
regenclean$plot <- paste(regenclean$harvestNo + regenclean$block * 100)

unique(regenclean$plot)

regenclean$plotF <- as.factor(regenclean$plot)
# 103 mean block 1, harvest 3. 1101 means block 11 harvest 1.

# splitplot to be added after the data set is paired down

write.csv(regenclean, file = "regen_cleaned.csv", row.names = FALSE)

######## Merging Datasets #########
### Creating ID by Seedlot number ###

# Provenances that come from the same seedlot 
# 1. Venables and Peterhope futures
# 2. Venables and Peterhope Larch
# 3. Venables and Peterhope Lodgepole
# 4. Narrows and Redfish Ponderosa
# 5. Venables, Peterhope, Alex Fraser, and John Prince Ponderosa
# 6. Narrows and Redfish Larch
# 7. Peterhope A, Venables and Peterhope futures 

regen <- read.csv("regen_cleaned.csv", header = TRUE)

unique(regen$provenance)

# Seedlot identifier contains "seedlot - species - class"
# ie. "43152-Pl-B" is Lodgepole pine b class planted at Redfish  

prov_climatebase <- read.csv("ClimateData_All_Provs_2021data.csv", header=TRUE)

# Keeping a small subset of the variables 

prov_climate <- subset(prov_climatebase, select = c(provenance, ID_tag, seedlot, MAT, MWMT, MCMT, TD, MAP, MSP, AHM, SHM,
                                                    DD_0, DD5, DD_18, DD18, NFFD, FFP, PAS, EMT, EXT, Eref, CMD, RH, CMI, DD1040))

# Needs to be specified that these are climatic variables associated with the different provenance as the process for adding climate variables 
# for the locations will be exactly the same 

rename_prov_climate_vars <- function (){
  
  names(prov_climate)[names(prov_climate) == "MAT"] <- "p_MAT"
  names(prov_climate)[names(prov_climate) == "MWMT"] <- "p_MWMT"
  names(prov_climate)[names(prov_climate) == "MCMT"] <- "p_MCMT"
  names(prov_climate)[names(prov_climate) == "TD"] <- "p_TD"
  names(prov_climate)[names(prov_climate) == "MAP"] <- "p_MAP"
  names(prov_climate)[names(prov_climate) == "MSP"] <- "p_MSP"
  names(prov_climate)[names(prov_climate) == "AHM"] <- "p_AHM"
  names(prov_climate)[names(prov_climate) == "SHM"] <- "p_SHM"
  names(prov_climate)[names(prov_climate) == "DD_0"] <- "p_DD_0"
  names(prov_climate)[names(prov_climate) == "DD5"] <- "p_DD5"
  names(prov_climate)[names(prov_climate) == "DD_18"] <- "p_DD_18"
  names(prov_climate)[names(prov_climate) == "DD18"] <- "p_DD18"
  names(prov_climate)[names(prov_climate) == "NFFD"] <- "p_NFFD"
  names(prov_climate)[names(prov_climate) == "FFP"] <- "p_FFP"
  names(prov_climate)[names(prov_climate) == "PAS"] <- "p_PAS"
  names(prov_climate)[names(prov_climate) == "EMT"] <- "p_EMT"
  names(prov_climate)[names(prov_climate) == "EXT"] <- "p_EXT"
  names(prov_climate)[names(prov_climate) == "Eref"] <- "p_Eref"
  names(prov_climate)[names(prov_climate) == "CMD"] <- "p_CMD"
  names(prov_climate)[names(prov_climate) == "RH"] <- "p_RH"
  names(prov_climate)[names(prov_climate) == "CMI"] <- "p_CMI"
  names(prov_climate)[names(prov_climate) == "DD1040"] <- "p_DD1040"
  prov_climate
}

prov_climate <- rename_prov_climate_vars()

joined_df <- merge(regen, prov_climate, by.x = "provenance", 
                   by.y = "provenance", all.x = TRUE, all.y = FALSE)

# This will merge the datasets correctly and any number of climatic variables can be added via leaving them in the "climate" dataframe

### Adding in Site climate variables ###

loc_climatebase <- read.csv("ClimateData_AllSites.csv", header=TRUE)
unique(loc_climatebase$location)

# Select location plus the desired climate variables. Make sure they all get renamed
loc_climate <- subset(loc_climatebase, select = c(location, MAT, MWMT, MCMT, TD, MAP, MSP, AHM, SHM,
                                                  DD_0, DD5, DD_18, DD18, NFFD, FFP, PAS, EMT, EXT, Eref, CMD, RH, CMI, DD1040))

rename_loc_climate_vars <- function (){
  
  names(loc_climate)[names(loc_climate) == "MAT"] <- "s_MAT"
  names(loc_climate)[names(loc_climate) == "MWMT"] <- "s_MWMT"
  names(loc_climate)[names(loc_climate) == "MCMT"] <- "s_MCMT"
  names(loc_climate)[names(loc_climate) == "TD"] <- "s_TD"
  names(loc_climate)[names(loc_climate) == "MAP"] <- "s_MAP"
  names(loc_climate)[names(loc_climate) == "MSP"] <- "s_MSP"
  names(loc_climate)[names(loc_climate) == "AHM"] <- "s_AHM"
  names(loc_climate)[names(loc_climate) == "SHM"] <- "s_SHM"
  names(loc_climate)[names(loc_climate) == "DD_0"] <- "s_DD_0"
  names(loc_climate)[names(loc_climate) == "DD5"] <- "s_DD5"
  names(loc_climate)[names(loc_climate) == "DD_18"] <- "s_DD_18"
  names(loc_climate)[names(loc_climate) == "DD18"] <- "s_DD18"
  names(loc_climate)[names(loc_climate) == "NFFD"] <- "s_NFFD"
  names(loc_climate)[names(loc_climate) == "FFP"] <- "s_FFP"
  names(loc_climate)[names(loc_climate) == "PAS"] <- "s_PAS"
  names(loc_climate)[names(loc_climate) == "EMT"] <- "s_EMT"
  names(loc_climate)[names(loc_climate) == "EXT"] <- "s_EXT"
  names(loc_climate)[names(loc_climate) == "Eref"] <- "s_Eref"
  names(loc_climate)[names(loc_climate) == "CMD"] <- "s_CMD"
  names(loc_climate)[names(loc_climate) == "RH"] <- "s_RH"
  names(loc_climate)[names(loc_climate) == "CMI"] <- "s_CMI"
  names(loc_climate)[names(loc_climate) == "DD1040"] <- "s_DD1040"
  
  loc_climate
}

loc_climate <- rename_loc_climate_vars()

regen <- merge(joined_df, loc_climate, by.x = "location", 
                   by.y = "location", all.x = FALSE, all.y = FALSE)

summary(regen)

regen$d_MAT <- (regen$s_MAT - regen$p_MAT)
regen$d_MWMT <- (regen$s_MWMT - regen$p_MWMT)
regen$d_MCMT <- (regen$s_MCMT - regen$p_MCMT)
regen$d_TD <- (regen$s_TD - regen$p_TD)
regen$d_MAP <- (regen$s_MAP - regen$p_MAP)
regen$d_MSP <- (regen$s_MSP - regen$p_MSP)
regen$d_AHM <- (regen$s_AHM - regen$p_AHM)
regen$d_SHM <- (regen$s_SHM - regen$p_SHM)
regen$d_DD_0 <- (regen$s_DD_0 - regen$p_DD_0)
regen$d_DD5 <- (regen$s_DD5 - regen$p_DD5)
regen$d_DD_18 <- (regen$s_DD_18 - regen$p_DD_18)
regen$d_DD18 <- (regen$s_DD18 - regen$p_DD18)
regen$d_NFFD <- (regen$s_NFFD - regen$p_NFFD)
regen$d_FFP <- (regen$s_FFP - regen$p_FFP)
regen$d_PAS <- (regen$s_PAS - regen$p_PAS)
regen$d_EMT <- (regen$s_EMT - regen$p_EMT)
regen$d_EXT <- (regen$s_EXT - regen$p_EXT)
regen$d_Eref <- (regen$s_Eref - regen$p_Eref)
regen$d_CMD <- (regen$s_CMD - regen$p_CMD)
regen$d_RH <- (regen$s_RH - regen$p_RH)
regen$d_CMI <- (regen$s_CMI - regen$p_CMI)
regen$d_DD1040 <- (regen$s_DD1040 - regen$p_DD1040)


## Everything is good at this point except damage notes which has not been dealt with yet 
# - Thomson (29/052023)

write.csv(regen, "regen_merged.csv", row.names = FALSE)

###### Investigating Experimental Design by Location #####

regen <- read.csv("regen_merged.csv", header = TRUE)

# diving dataset by location 

head(regen$locationNo)

location1<-regen[(regen$locationNo==1),]
location2<-regen[(regen$locationNo==2),]
location3<-regen[(regen$locationNo==3),]
location4<-regen[(regen$locationNo==4),]
location5<-regen[(regen$locationNo==5),]
location6<-regen[(regen$locationNo==6),]

dim(regen)
dim(location1);dim(location2);dim(location3);
dim(location4); dim(location5); dim(location6)
dim(location1)+ dim(location2)+dim(location3)+
  dim(location4)+ dim(location5)+ dim(location6)
# Correct subsetting of the locations the numbers add up

###### Location 1 ######

## Location 1. 3 blocks X 4 harvests = 12 plots, and 12 provenances
## = 144 splitplots Correct for the plots. 

summary(location1)
unique(location1$ID_tag) # 12  provenances 

location1 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location1 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

location1 %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 144 = 3 blocks X 4 harvests X 12 provenances
# only 139 rows missing 5 provenances somewhere in here

# Douglas-fir
location1_Fd <- subset(location1, species == "Fd")
unique(location1_Fd$ID_tag) # 9 provenances 

loc1fd <- location1_Fd %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 108 = 3 blocks X 4 harvests X 9 provenances
# only 104, missing 4
summary(loc1fd)
# missing 2 in both block 2 and 3, 2 of those in 60ret the others in clearcut and seed
plyr::count(loc1fd$harvestF)
plyr::count(loc1fd$blockF)
plyr::count(loc1fd$ID_tag)
# 46129_Fd_B missing 2, 53751_Fd_B missing 1, 63201_Fd_A, missing 1

loc1fd_46129 <- loc1fd[loc1fd$ID_tag == "46129_Fd_B", ]
# missing in clearcut block 3 and seedtree block 2
loc1fd_53751 <- loc1fd[loc1fd$ID_tag == "53751_Fd_B", ]
# missing in 60Ret block 3
loc1fd_63201 <- loc1fd[loc1fd$ID_tag == "63201_Fd_A", ]
# missing in 60Ret block 2

# Larch
location1_Lw <- subset(location1, species == "Lw")
unique(location1_Lw$ID_tag)

location1_Lw %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests X 1 provenances

# Lodgepole Pine
location1_Pl <- subset(location1, species == "Pl")
unique(location1_Pl$ID_tag)

location1_Pl %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests X 1 provenances

# Ponderosa Pine
location1_Py <- subset(location1, species == "Py")
unique(location1_Py$ID_tag)

location1_Py %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests X 1 provenances
# Block 2 seedtree missing any Ponderosa pines 

###### Location 2 ######
unique(location2$ID_tag)

location2 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location2 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

location2 %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 108 = 3 blocks X 4 harvests X 9 provenances

# Correct number of provenances 

###### Location 3 ######
location3$countA <- 1

unique(location3$ID_tag) # 10 provenances 

location3 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location3 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

location3 %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 120 = 3 blocks X 4 harvests X 10 provenances

# Only 116 rows. Missing 4 provenances somewhere

# Douglas fir
location3_Fd <- subset(location3, species == "Fd")
unique(location3_Fd$ID_tag) # 7 provenances 

loc3fd <- location3_Fd %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n())

summary(loc3fd)
# 2 missing in block 9, 1 in 30ret and 1 in 60ret
plyr::count(loc3fd$ID_tag)
# 8491_Fd_B missing 2

loc3fd_8491 <- loc3fd[loc3fd$ID_tag == "8491_Fd_B", ]
# 8491_Fd_B missing in 60Ret and 30Ret in block 9

# Larch
location3_Lw <- subset(location3, species == "Lw")
unique(location3_Lw$ID_tag) # 1 provenances 

loc3lw <- location3_Lw %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())

summary(loc3lw)
# 3351_Lw_B missing 2 in block 7, 1 in 30ret, 1 in clearcut

# Lodgepole Pine
location3_Pl <- subset(location3, species == "Pl")
unique(location3_Pl$ID_tag) 

loc3pl <- location3_Pl %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())

summary(loc3pl)
# All good

# Ponderosa Pine
location3_Py <- subset(location3, species == "Py")
unique(location3_Py$ID_tag) 

loc3py <- location3_Py %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())

summary(loc3py)
# All good

###### Location 4 #####
unique(location4$ID_tag) # 9 provenances

location4 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 4 = 1 blocks X 4 harvests 

location4 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 12 = 1 blocks X 4 plots per block

location4 %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 36 = 1 blocks X 4 harvests X 9 provenances

# Correct number of provenances 

###### Location 5 #####
unique(location5$ID_tag) # 11 provenances 

location5 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 16 = 4 blocks X 4 harvests
# only 15 missing a block or harvest
# missing block 12 clearcut

location5 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 16 = 4 blocks X 4 plots per block
# only 15 missing a block or a plotF

location5_n12 <- subset(location5, blockF != "12")

location5_n12 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests
# correct without block 12

location5_n12 %>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 132 = 3 blocks X 4 harvests X 11 provenances
# only 109 missing a lot (23)

# Douglas fir
location5_n12_Fd <- subset(location5_n12, species == "Fd")
unique(location5_n12_Fd$ID_tag) # 8 provenances

loc5fd <- location5_n12_Fd %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())
summary(loc5fd)
# missing 5 in block 11, 7 in block 13, and 6 in block 14 (total 18)
# missing 3 in clearcut, 4 in seed, 6 in 30ret, and 5 in 60ret (total 18)

plyr::count(loc5fd$ID_tag)
# missing 10 39259_Fd_B, missing 7 53751_Fd_B, missing 1 53614_Fd_B (total 18)

loc5fd_39259 <- loc5fd[loc5fd$ID_tag == "39259_Fd_B", ]
# only planted at 2 harvest in block 11
loc5fd_53751 <- loc5fd[loc5fd$ID_tag == "53751_Fd_B", ]
# missing from most harvests and blocks
loc5fd_53614 <- loc5fd[loc5fd$ID_tag == "53614_Fd_B", ]
# missing from block 13 60Ret

# Larch 
location5_n12_Lw <- subset(location5_n12, species == "Lw")
unique(location5_n12_Lw$ID_tag) # 1 provenance

loc5lw <- location5_n12_Lw %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())
summary(loc5lw)
# missing 1 from block 13 and 1 from seed (total 1)

# Lodgepole Pine

location5_n12_Pl <- subset(location5_n12, species == "Pl")
unique(location5_n12_Pl$ID_tag) # 1 provenance

loc5pl <- location5_n12_Pl %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())
summary(loc5pl)
# 1 missing from block 11, 2 missing from block 13 (total 3)
# 1 missing from seed, 1 from 30ret, 1 from 60ret (total 3)

# Ponderosa pine

location5_n12_Py <- subset(location5_n12, species == "Py")
unique(location5_n12_Py$ID_tag) # 1 provenance 

loc5py <- location5_n12_Py %>%
  group_by(blockF, harvestF, ID_tag) %>%
  summarise( alive = n())
summary(loc5py)
# 1 missing from block 13 60ret (total 1)

###### Location 6 #####
unique(location6$ID_tag) # 11 provenances

location6 %>%
  group_by(blockF,harvestF) %>%
  summarise( alive= n()) # 1 = 1 blocks X 4 harvests 

location6 %>%
  group_by(blockF,plotF) %>%
  summarise( alive= n()) # 1 = 1 blocks X 4 plots per block

location6%>%
  group_by(blockF,harvestF,ID_tag) %>%
  summarise( alive= n()) # 44 = 1 blocks X 4 harvests X 11 provenances

# Correct number of provenances












# see "experimental design issues.csv" for list of blocks with issues 
# Note: Redfish block 12 was completely removed so issues with Redfish's experimental design only consider blocks 11, 13, 14



####### Creating Custom Datasets ########

#### Full data set ####
write.csv(regen, file = "regen_cleaned.csv", row.names = FALSE)






