
# 1. Survival Probability Calculation ------------------------------------------

survivalProbs <- function (df, model_column) {
  
  for (i in 1:length(model_column)) {
    
    # Creates new column "survival_probs" and fills it with the estimated probabilities 
    df$data[[i]][["survival_probs"]] <- (exp(predict(model_column[[i]]))) / (1 + exp(predict(model_column[[i]])))
    
  }
  
  # Function output
  return(df)
  
}



# 2. Est. Prob Graphing -----------------------------------------------------------

graphESTSurvivalProb <- function (df) {
  
  # List of model name
  MODEL_NAME <- names(df %>% select(starts_with("model")))
  
  
  for (i in 1:nrow(df)) {
    
    # List of climatic variables
    C_VAR <- df[["ClimaticVarList"]][[i]]
    
    
    # List of model varaibles 
    VARIABLES <- if (grepl("_1", names(df[MODEL_NAME]))) {
      
      c( paste(C_VAR, "[all]"))
      
    } else {
      
      c( paste(C_VAR, "[all]"), paste("harvestF"))
      
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



graphESTSurvivalProb_2 <- function (df) {
  
  # List of model name
  MODEL_NAME <- names(df %>% select(starts_with("model")))
  
  
  for (i in 1:nrow(df)) {
    
    # List of climatic variables
    C_VAR <- df[["ClimaticVarList"]][[i]]
    
    
    # List of model varaibles 
    VARIABLES <- as.character(attr(attr(df[[MODEL_NAME]][[i]]@frame, "terms"), "predvars.fixed"))
    
    VARIABLES <- VARIABLES[-c(1:2)]
    
    # Printing plots
    print(sjPlot::plot_model(df[[MODEL_NAME]][[i]], type = "pred", terms = c(VARIABLES)) + 
            
            geom_point(data = df$data[[i]], mapping = aes(x = .data[[C_VAR]], y = survival_probs), 
                       inherit.aes = FALSE, size = 0.5) +
            labs(x = paste( C_VAR, "Climatic Distance"), 
                 y = "Estimated Probability of Survival",
                 title = NULL) )
    
    ggsave(paste(Sys.Date(), C_VAR, MODEL_NAME,
                 
                 "Est_Surivival_Prob_Survival_Cover_All_Locs.pdf", sep = "_"))
    
  }
  
}


graphESTSurvivalProb_3 <- function (df) {
  
  # List of model name
  MODEL_NAME <- names(df %>% select(starts_with("model")))
  
  
  for (i in 1:nrow(df)) {
    
    # List of climatic variables
    C_VAR <- df[["ClimaticVarList"]][[i]]
    
    
    # List of model varaibles 
    VARIABLES <- if (grepl("_1", names(df[MODEL_NAME]))) {
      
      c( paste(C_VAR, "[all]"))
      
    } else {
      
      c( paste(C_VAR, "[all]"), paste("sqrt_tree_cover [0, 5, 7]"))
      
    }
    
    # Printing plots
    ggarrange(print(sjPlot::plot_model(df[[MODEL_NAME]][[i]], 
                             type = "pred", 
                             terms = c(VARIABLES),
                             legend.title = "Square Root \nTree Cover") + 
            
            geom_point(data = df$data[[i]], 
                       mapping = aes(x = .data[[C_VAR]], y = survival_probs), 
                       inherit.aes = FALSE, size = 0.5) +
           
             labs(x = paste( C_VAR, "Climatic Distance"), 
                 y = "Estimated Probability of Survival",
                 title = NULL) ) +
      
      theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
            panel.grid.major = element_line(color = "gray30", linewidth = .15),
            panel.grid.minor = element_blank()) )
      
    
    ggsave(paste(Sys.Date(), C_VAR, MODEL_NAME,
                 
                 "Est_Surivival_Prob_Survival_Harvest_All_Locs.pdf", sep = "_"))
    
  }
  
}




