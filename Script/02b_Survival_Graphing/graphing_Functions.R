



survivalProbs <- function (df, model_column) {
  
  for (i in 1:length(model_column)) {
    
    # Creates new column "survival_probs" and fills it with the estimated probabilities 
    df$data[[i]][["survival_probs"]] <- (exp(predict(model_column[[i]]))) / (1 + exp(predict(model_column[[i]])))
    
  }
  
  # Function output
  return(df)
  
}




graphESTSurvivalProb_poly <-  function(df) {
  
  for (i in 1:nrow(df)) {
    
    c_var <- df[["ClimaticVarList"]][[i]]
    
    print(ggplot(data = df$data[[i]], mapping = aes(x = .data[[c_var]], y = survival_probs)) + 
            geom_point() +
            geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
            labs(title = c_var, x = paste(c_var, "Climatic Distance"), y = "Estimated Probability of Survival") +
            theme(legend.title=element_blank()))
    
    
    # Saving plots as PDFs
    # Long piece of code that just specifics the name of the file
    # Could be done manually if wanted 
    ggsave(paste(Sys.Date(), c_var, "Est_Surivival_Prob_Survival_Harvest_All_Locs.pdf", sep = "_"))
    
  }
  
} 




graphESTSurvivalProb <- function (df) {
  
  # List of model name
  MODEL_NAME <- names(df %>% select(starts_with("model")))
  
  
  for (i in 1:nrow(df)) {
    
    # List of climatic variables
    C_VAR <- df[["ClimaticVarList"]][[i]]
    
    
    # List of model varaibles 
    VARIABLES <- if (grepl("_3", names(df[MODEL_NAME]))) {
      
      c( paste(C_VAR, "[all]"), paste("harvestF"))
      
    } else {
      
      c(paste(C_VAR, "[all]"))
      
    }
    
    # Printing plots
    print(sjPlot::plot_model(df[[MODEL_NAME]][[i]], type = "pred", terms = c(VARIABLES)) + 
            
            geom_point(data = df$data[[i]], mapping = aes(x = .data[[C_VAR]], y = survival_probs), 
                       inherit.aes = FALSE, size = 0.5) +
            labs(x = paste( C_VAR, "Climatic Distance"), 
                 y = "Estimated Probability of Survival",
                 title = NULL) )
    
    ggsave(paste(Sys.Date(), C_VAR, MODEL_NAME,
                 
                 "Est_Surivival_Prob_Survival_Harvest_All_Locs.pdf", sep = "_"))
    
  }
  
}



