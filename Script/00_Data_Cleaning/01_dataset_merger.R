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
prov_climate <- subset(prov_climatebase, select = c(provenance, ID_tag, seedlot, MAT, MWMT, MCMT, MAP, MSP, AHM, SHM,
                                                    NFFD, FFP, PAS, EMT, EXT, Eref, CMD, RH))

# Needs to be specified that these are climatic variables associated with the different provenance as the process for adding climate variables 
# for the locations will be exactly the same 

#

rename_prov_climate_vars <- function (){
  
  names(prov_climate)[names(prov_climate) == "MAT"] <- "p_MAT"
  names(prov_climate)[names(prov_climate) == "MWMT"] <- "p_MWMT"
  names(prov_climate)[names(prov_climate) == "MCMT"] <- "p_MCMT"
  names(prov_climate)[names(prov_climate) == "MAP"] <- "p_MAP"
  names(prov_climate)[names(prov_climate) == "MSP"] <- "p_MSP"
  names(prov_climate)[names(prov_climate) == "AHM"] <- "p_AHM"
  names(prov_climate)[names(prov_climate) == "SHM"] <- "p_SHM"
  names(prov_climate)[names(prov_climate) == "NFFD"] <- "p_NFFD"
  names(prov_climate)[names(prov_climate) == "FFP"] <- "p_FFP"
  names(prov_climate)[names(prov_climate) == "PAS"] <- "p_PAS"
  names(prov_climate)[names(prov_climate) == "EMT"] <- "p_EMT"
  names(prov_climate)[names(prov_climate) == "EXT"] <- "p_EXT"
  names(prov_climate)[names(prov_climate) == "Eref"] <- "p_Eref"
  names(prov_climate)[names(prov_climate) == "CMD"] <- "p_CMD"
  names(prov_climate)[names(prov_climate) == "RH"] <- "p_RH"
  prov_climate
}

prov_climate <- rename_prov_climate_vars()

###### 2.2 merging regen and prov climatic data ----
joined_df <- merge(regen_cleaned_unmerged, prov_climate, by.x = "provenance", 
                   by.y = "provenance", all.x = TRUE, all.y = FALSE)

# This will merge the datasets correctly and any number of climatic variables can be added via leaving them in the "climate" dataframe

# 3. Loading Site Climate Data ------------------------------------------------

loc_climatebase <- read.csv(here("Data/02_ClimateBC", "ClimateData_AllSites.csv"), header=TRUE)
unique(loc_climatebase$location)

###### 3.1 creating a subset -----
# Select location plus the desired climate variables. Make sure they all get renamed
loc_climate <- subset(loc_climatebase, select = c(location, MAT, MWMT, MCMT, MAP, MSP, AHM, SHM,
                                                  NFFD, FFP, PAS, EMT, EXT, Eref, CMD, RH))

rename_loc_climate_vars <- function (){
  
  names(loc_climate)[names(loc_climate) == "MAT"] <- "s_MAT"
  names(loc_climate)[names(loc_climate) == "MWMT"] <- "s_MWMT"
  names(loc_climate)[names(loc_climate) == "MCMT"] <- "s_MCMT"
  names(loc_climate)[names(loc_climate) == "MAP"] <- "s_MAP"
  names(loc_climate)[names(loc_climate) == "MSP"] <- "s_MSP"
  names(loc_climate)[names(loc_climate) == "AHM"] <- "s_AHM"
  names(loc_climate)[names(loc_climate) == "SHM"] <- "s_SHM"
  names(loc_climate)[names(loc_climate) == "NFFD"] <- "s_NFFD"
  names(loc_climate)[names(loc_climate) == "FFP"] <- "s_FFP"
  names(loc_climate)[names(loc_climate) == "PAS"] <- "s_PAS"
  names(loc_climate)[names(loc_climate) == "EMT"] <- "s_EMT"
  names(loc_climate)[names(loc_climate) == "EXT"] <- "s_EXT"
  names(loc_climate)[names(loc_climate) == "Eref"] <- "s_Eref"
  names(loc_climate)[names(loc_climate) == "CMD"] <- "s_CMD"
  names(loc_climate)[names(loc_climate) == "RH"] <- "s_RH"

  loc_climate
}

loc_climate <- rename_loc_climate_vars()

######## 3.2 merging updated regen with site climate data -----
regen_cleaned_merged <- merge(joined_df, loc_climate, by.x = "location", 
               by.y = "location", all.x = FALSE, all.y = FALSE)

summary(regen_cleaned_merged)

# 4. Adding Climatic Distance ------------------------------------------------
regen_cleaned_merged$d_MAT <- (regen_cleaned_merged$s_MAT - regen_cleaned_merged$p_MAT)
regen_cleaned_merged$d_MWMT <- (regen_cleaned_merged$s_MWMT - regen_cleaned_merged$p_MWMT)
regen_cleaned_merged$d_MCMT <- (regen_cleaned_merged$s_MCMT - regen_cleaned_merged$p_MCMT)
regen_cleaned_merged$d_MAP <- (regen_cleaned_merged$s_MAP - regen_cleaned_merged$p_MAP)
regen_cleaned_merged$d_MSP <- (regen_cleaned_merged$s_MSP - regen_cleaned_merged$p_MSP)
regen_cleaned_merged$d_AHM <- (regen_cleaned_merged$s_AHM - regen_cleaned_merged$p_AHM)
regen_cleaned_merged$d_SHM <- (regen_cleaned_merged$s_SHM - regen_cleaned_merged$p_SHM)
regen_cleaned_merged$d_NFFD <- (regen_cleaned_merged$s_NFFD - regen_cleaned_merged$p_NFFD)
regen_cleaned_merged$d_FFP <- (regen_cleaned_merged$s_FFP - regen_cleaned_merged$p_FFP)
regen_cleaned_merged$d_PAS <- (regen_cleaned_merged$s_PAS - regen_cleaned_merged$p_PAS)
regen_cleaned_merged$d_EMT <- (regen_cleaned_merged$s_EMT - regen_cleaned_merged$p_EMT)
regen_cleaned_merged$d_EXT <- (regen_cleaned_merged$s_EXT - regen_cleaned_merged$p_EXT)
regen_cleaned_merged$d_Eref <- (regen_cleaned_merged$s_Eref - regen_cleaned_merged$p_Eref)
regen_cleaned_merged$d_CMD <- (regen_cleaned_merged$s_CMD - regen_cleaned_merged$p_CMD)
regen_cleaned_merged$d_RH <- (regen_cleaned_merged$s_RH - regen_cleaned_merged$p_RH)


## Everything is good at this point except damage notes which has not been dealt with yet 
# - Thomson (29/052023)

# 5. Writing a New .CSV File ---------------------------------------------------
write.csv(regen_cleaned_merged, file = here("Data/03_Processed" , "20231201_regen_merged.csv"), row.names = FALSE)


