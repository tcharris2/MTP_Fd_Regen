# Investigating Experimental Design by Location --------------------------------

survival_fd_b_processed <- read.csv(here("Data/03_processed" , "20231130_survival_fd_b_processed.csv"), header = TRUE)

survival_fd_b_processed <- subset(survival_fd_b_processed, !(is.na(tree_cover)))

# diving dataset by location 

location1<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==1),]
location2<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==2),]
location3<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==3),]
location4<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==4),]
location5<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==5),]
location6<-survival_fd_b_processed[(survival_fd_b_processed$locationNo==6),]

dim(survival_fd_b_processed)
dim(location1);dim(location2);dim(location3);
dim(location4); dim(location5); dim(location6)
dim(location1)+ dim(location2)+dim(location3)+
  dim(location4)+ dim(location5)+ dim(location6)
# Correct subsetting of the locations the numbers add up

######* Location 1 ######

## Location 1. 3 blocks X 4 harvests = 12 plots, and 12 provenances
## = 144 splitplots Correct for the plots. 

summary(location1)
unique(location1$ID_tag) # 6  provenances 
unique(location1$provenance)

location1 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location1 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

location1 %>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 72 = 3 blocks X 4 harvests X 6 provenances
# got all 72
# has a future provenance 


######* Location 2 ######
unique(location2$ID_tag)
unique(location2$provenance)

location2 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location2 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

location2 %>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 72 = 3 blocks X 4 harvests X 6 provenances

# Correct number of provenances 
# Got all 72 
# has a future provenance 

######* Location 3 ######
unique(location3$ID_tag) # 6 provenances 
unique(location3$provenance)

location3 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests 

location3 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block

loc_3_issue <- location3 %>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 72 = 3 blocks X 4 harvests X 6 provenances

# Only 70 rows. Missing 2 provenances somewhere

# has a future provenance

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


######* Location 4 #####
unique(location4$ID_tag) # 6 provenances
unique(location4$provenance)

location4 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 4 = 1 blocks X 4 harvests 

location4 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 4 = 1 blocks X 4 plots per block

location4 %>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 24 = 1 blocks X 4 harvests X 6 provenances

# Correct number of provenances 
# got all 24 
# has a future provenance

######* Location 5 #####
unique(location5$ID_tag) # 5 provenances 
unique(location5$provenance)

location5 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 harvests
# only 15 missing a block or harvest
# missing block 12 clearcut

location5 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 12 = 3 blocks X 4 plots per block


location5 %>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 60 = 3 blocks X 4 harvests X 5 provenances
# only 59 missing 1

# no future 

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



######* Location 6 #####
unique(location6$ID_tag) # 6 provenances
unique(location6$provenance)

location6 %>%
  group_by(blockNo, harvestNo) %>%
  summarise( alive= n()) # 1 = 1 blocks X 4 harvests 

location6 %>%
  group_by(blockNo, plotNo) %>%
  summarise( alive= n()) # 1 = 1 blocks X 4 plots per block

location6%>%
  group_by(blockNo, harvestNo, ID_tag) %>%
  summarise( alive= n()) # 24 = 1 blocks X 4 harvests X 6 provenances

# Correct number of provenances
# got all 24

# Two-bit-B-Class is only at two bit. Similar to future 




# see "experimental design issues.csv" for list of blocks with issues 
# Note: Redfish block 12 was completely removed so issues with Redfish's experimental design only consider blocks 11, 13, 14



