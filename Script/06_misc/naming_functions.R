names(melt_height_harvest_df) <- paste0("harvest_", ClimaticVarList)



names(melt_height_harvest_df[1])



treatNameFunction <- function(df) {
  
  if (grepl("harvest", names(df[1]))) {
    
    paste("harvest_")
    
  } else {
    
    paste("canopy_")
    
  }
  
}
treatNameFunction(melt_height_harvest_df)


varTypeFunction <- function (df) {
  
  if (grepl("_ln_", names(df[1]))) {
    
    paste0("_ln_")
    
  } else {
    
    paste0("_")
    
  }
  
}
