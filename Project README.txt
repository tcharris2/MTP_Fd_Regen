
Project: Douglas Fir Regeneration Analysis 

What this project does... 


Code: 
- All project code is found under /Script
- /Data Cleaning/rawdata_cleaner.R is code made to clean the raw data 
- /Data Cleaning/csv_builder.R is code to create a custom subset of the cleaned data
  that can be used to run separate analyses
- /Functions holds the code for all functions and descriptions of the functions used in 
  the analysis. This does not contain the functions used to clean the data and does not 
  have to be run independently of the analysis 
- /Survival contains the body of the analysis. This can be run independently of
  /Data Cleaning and /Function as it pulls the cleaned data from /Data/Processed and 
  contains the functions in the body of the code. 


Variables:

- See /Data/Metadata for a full list of variable names and units 
- Climate data is from ClimateNA version 7.40
- Climatic difference (d_X) is calculated by site climate (s_X) - provenance climate (p_X)
- All climatic variables are normalized around the mean and standard deviation 

