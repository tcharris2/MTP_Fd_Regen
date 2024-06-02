##### DATASET MERGER ---------------------------------------------------------
### Creating ID by Seedlot number ###

# Provenances that come from the same seedlot 
# 1. Venables and Peterhope futures
# 2. Venables and Peterhope Larch
# 3. Venables and Peterhope Lodgepole
# 4. Narrows and Redfish Ponderosa
# 5. Venables, Peterhope, Alex Fraser, and John Prince Ponderosa
# 6. Narrows and Redfish Larch
# 7. Peterhope A, Venables and Peterhope futures 

# 1. Loading Cleaned Regen Data ------------------------------------------------
regen_cleaned_unmerged <- read.csv(here("Data/03_Processed", "20231129_regen_cleaned.csv"), header = TRUE)

unique(regen_cleaned_unmerged$provenance)

# Seedlot identifier contains "seedlot - species - class"
# ie. "43152-Pl-B" is Lodgepole pine b class planted at Redfish from seedlot 43152 


# 2. Loading Provenance Climate Data --------------------------------------------
prov_climatebase <- read.csv(here("Data/02_ClimateBC", "ClimateData_All_Provs_2021data.csv"), header=TRUE)

# Keeping a small subset of the variables 

###### 2.1 creating a subset ------
prov_climate <- subset(prov_climatebase, select = c(provenance, ID_tag, seedlot, 
                                                    MAT, MAP, MSP, AHM,
                                                    NFFD, PAS, EMT,
                                                    EXT, RH))

# Needs to be specified that these are climatic variables associated with the different provenance as the process for adding climate variables 
# for the locations will be exactly the same 

#

rename_prov_climate_vars <- function (df){
  
  # Rename Variables
  names(df)[names(df) == "MAT"] <- "p_MAT"
  names(df)[names(df) == "MAP"] <- "p_MAP"
  names(df)[names(df) == "MSP"] <- "p_MSP"
  names(df)[names(df) == "AHM"] <- "p_AHM"
  names(df)[names(df) == "NFFD"] <- "p_NFFD"
  names(df)[names(df) == "PAS"] <- "p_PAS"
  names(df)[names(df) == "EMT"] <- "p_EMT"
  names(df)[names(df) == "EXT"] <- "p_EXT"
  names(df)[names(df) == "RH"] <- "p_RH"
  
  # Function Output
  return(df)
}

prov_climate <- rename_prov_climate_vars(prov_climate)

###### 2.2 merging regen and prov climatic data ----
joined_df <- merge(regen_cleaned_unmerged, prov_climate, by.x = "provenance", 
                   by.y = "provenance", all.x = TRUE, all.y = FALSE)

# This will merge the datasets correctly and any number of climatic variables can be added via leaving them in the "climate" dataframe

# 3. Loading Site Climate Data ------------------------------------------------

loc_climatebase <- read.csv(here("Data/02_ClimateBC", "ClimateData_AllSites.csv"), header=TRUE)
unique(loc_climatebase$location)

###### 3.1 creating a subset -----
# Select location plus the desired climate variables. Make sure they all get renamed
loc_climate <- subset(loc_climatebase, select = c(location, MAT, MAP, MSP, AHM,
                                                  NFFD, PAS, EMT, EXT, RH))

rename_loc_climate_vars <- function (df){
  
  # rename variables
  names(df)[names(df) == "MAT"] <- "s_MAT"
  names(df)[names(df) == "MAP"] <- "s_MAP"
  names(df)[names(df) == "MSP"] <- "s_MSP"
  names(df)[names(df) == "AHM"] <- "s_AHM"
  names(df)[names(df) == "NFFD"] <- "s_NFFD"
  names(df)[names(df) == "PAS"] <- "s_PAS"
  names(df)[names(df) == "EMT"] <- "s_EMT"
  names(df)[names(df) == "EXT"] <- "s_EXT"
  names(df)[names(df) == "RH"] <- "s_RH"

  # Function output
  return(df)
}

loc_climate <- rename_loc_climate_vars(loc_climate)

######## 3.2 merging updated regen with site climate data -----
regen_cleaned_merged <- merge(joined_df, loc_climate, by.x = "location", 
               by.y = "location", all.x = FALSE, all.y = FALSE)

summary(regen_cleaned_merged)

# 4. Adding Climatic Distance ------------------------------------------------
regen_cleaned_merged$d_MAT <- (regen_cleaned_merged$s_MAT - regen_cleaned_merged$p_MAT)
regen_cleaned_merged$d_MAP <- (regen_cleaned_merged$s_MAP - regen_cleaned_merged$p_MAP)
regen_cleaned_merged$d_MSP <- (regen_cleaned_merged$s_MSP - regen_cleaned_merged$p_MSP)
regen_cleaned_merged$d_AHM <- (regen_cleaned_merged$s_AHM - regen_cleaned_merged$p_AHM)
regen_cleaned_merged$d_NFFD <- (regen_cleaned_merged$s_NFFD - regen_cleaned_merged$p_NFFD)
regen_cleaned_merged$d_PAS <- (regen_cleaned_merged$s_PAS - regen_cleaned_merged$p_PAS)
regen_cleaned_merged$d_EMT <- (regen_cleaned_merged$s_EMT - regen_cleaned_merged$p_EMT)
regen_cleaned_merged$d_EXT <- (regen_cleaned_merged$s_EXT - regen_cleaned_merged$p_EXT)
regen_cleaned_merged$d_RH <- (regen_cleaned_merged$s_RH - regen_cleaned_merged$p_RH)


## Everything is good at this point except damage notes which has not been dealt with yet 
# - Thomson (29/052023)

# 5. Writing a New .CSV File ---------------------------------------------------
write.csv(regen_cleaned_merged, file = here("Data/03_Processed" , "20231201_regen_merged.csv"), row.names = FALSE)


