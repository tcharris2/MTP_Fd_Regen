# Visualizing  p-values  ----------------------------------------

# Bringing CSVs into R ----------------------------------------
data_files <- list.files(here("Data/Temp/p_vals"))  # Identify file names
data_files                          # Print file names


for (i in 1:length(data_files)) {
  assign(data_files[i], read.csv(here("Data/Temp/p_vals", data_files[i])))
}

# Combining into a single datafram of p-values ------------------------------------

# creating a list of dataframes
p_val_list <- list(AHM_p_vals.csv, CMD_p_vals.csv, CMI_p_vals.csv, DD_0_p_vals.csv, DD_18_p_vals.csv, DD1040_p_vals.csv,
             DD18_p_vals.csv, DD5_p_vals.csv, EMT_p_vals.csv, Eref_p_vals.csv, EXT_p_vals.csv, FFP_p_vals.csv, 
             MAP_p_vals.csv, MAT_p_vals.csv, MCMT_p_vals.csv, MSP_p_vals.csv, MWMT_p_vals.csv, NFFD_p_vals.csv,
             PAS_p_vals.csv, RH_p_vals.csv, SHM_p_vals.csv, TD_p_vals.csv) 

# names to relabel columns  
p_val_names <- c("AHM", "CMD", "CMI", "DD_0", "DD_18", "DD1040", "DD18", "DD5", 
                "EMT", "Eref", "EXT", "FFP", "MAP", "MAT", "MCMT", "MSP", 
                "MWMT", "NFFD", "PAS", "RH", "SHM", "TD") 

# loop to relabel
for (i in 1:length(p_val_list)) {
  p_val <- p_val_names[i]
  p_val_list[[i]][["X"]] <- p_val
}


# binding dataframes together 
p_val_df <- bind_rows(p_val_list)

write.csv(p_val_df, file = here("Data/Temp", "p_val_merged.csv"), row.names = FALSE)

# Clean up global environment (OPTOINAL)
# rm(list=ls(pattern=".csv"))


# Bringing merged p-values into R ----------------------------------------
p_vals <- read.csv(here("Data/Temp", "p_val_merged.csv"), header = TRUE)

# Finding significant p values ------------------------------------------
p_sig <- p_vals
p_sig$p_0_1[p_sig$p_0_1 >= 0.05] <- NA
p_sig$p_1_2[p_sig$p_1_2 >= 0.05] <- NA
p_sig$p_2_3[p_sig$p_2_3 >= 0.05] <- NA
p_sig$p_1_3[p_sig$p_1_3 >= 0.05] <- NA

write.csv(p_sig, file = here("Data/Temp", "p_sig.csv"), row.names = FALSE)

  
  
  
