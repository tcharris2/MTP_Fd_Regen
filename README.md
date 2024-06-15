# MTP_Fd_Regen

Project Title: Douglas-fir B class regeneration analysis

Part of the Mother Tree Project (MTP) at the University of British Columbia (UBC). 

Principal Investigator: Thomson Harris

Email - thomsonharris@gmail.com, 
        tc.harris@ubc.ca 

Phone - 778-866-4198 

# Purpose:

This study aims at investigating the individual and interactive effects of climatic distance from provenance of origin and harvesting method on the the survival and performance of interior Douglas-fir seedlings.


# Keywords:

Forestry, Silviculture, Ecology, Interior Douglas-fir, Climatic Distance, Assisted Migration, Climate Change


# Taxonomic Information:

Interior Douglas-fir - Pseudotsuga menziesii var. glauca (Mayr) Franco
		      Taxonomic Serial No.: 183428

 # Variable Information:

 Survery Variables 
tree_number - unique identifier for each observation 
survival - binary value 1 indicates alive, 0 indicates dead. Used as a factor in analysis
location - site at which study was done
locationNo - numeric identifier of location (1 = Alex Fraser, 2 = Jaffray, 3 = John Prince, 4 = 		
	    Narrows, 5 = Redfish, 6 = Twobit)
block - unique identifier to apply blocking effect. Derived form "rep" 
rep - the replicate within a location 
plot - block*100 + harvestNo to create a 3rd level random effect
splitplotNo - location*10000 + block*100 + harvestNo to create the 4th level random effect
harvest_name - clearcut = 0% retention, seed = 10% dispersed retention (seed tree), 30Ret = 30%       	             aggregate retention, 60Ret = 60% aggregate retention with thinning from below  
harvestNo - numeric identifier of harvest (1 = clearcut, 2 = seed, 3 = 30Ret, 4 = 60Ret) 
species - species codes (Fd = interior Douglas-fir)
provenance - indicates the location the provenances was taken from, species code, and seed class
ID_tag - indicates the seedlot number_species code_seed class
provenanceNo - numeric identifier of provenance
alive - same as "survival" but not used as a factor  
height - height (cm) of the seedlings 
basal_dia - basal diameter (mm) of the seedlings 
crown_dia1 - crown diameter (cm) of the seedlings measured North/South
crown_dia2 - crown diameter (cm) of the seedlings measured East/West
avg_crown_dia - the average between "crown_dia1" and "crown_dia2" (cm)
damage - binary values indicating if a seedling has any damage done to it (1 yes, 0 no)
damage_notes - note indicating what kind of damage the seedling sustained 
condition - indicates tree performance (G = good, M = moderate, P = poor, N = nearly dead, D = dead, 
	    A = alive (condition unknown))
cover - a category of crown closure
tree_cover - percentage (%) crown closure
survery_date - date that data was collected
year_planted - year the seedlings were planted  

 Climatic Variables:
s_ = location (site) climate variables, p_ = provenance climate variables, d_ = difference between provenance and location climate variables 

 Climatic variables are retried from Climate NA v7.40. Using historical climate data: Normal_1981_2010.nrm.
	
MAT	- mean annual temperature (°C)
MAP	- mean annual precipitation (mm)
MSP 	- May to September precipitation (mm)
AHM	- annual heat-moisture index (MAT + 10)/(MAP/1000)
NFFD	- the number of frost free days
PAS 	- precipitation as snow (mm) between August in previous year and July in current year 
EMT	- extreme minimum temperature over 30 years
EXT	- extreme maximum temperature over 30 years
RH	- mean annual relative humidity (%)


