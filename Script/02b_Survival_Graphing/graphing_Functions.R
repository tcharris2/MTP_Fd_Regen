
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




mod <- inter_models[[2]][[1]]
mod_2 <- climatic_models[[2]][[1]]

mod_terms <- attr(mod@frame, "terms")
mod_terms
attr(mod@frame, "predvars.fixed")

fixef_terms <- unlist(as.character(attr(attr(mod@frame, "terms"), "predvars.fixed")))
fixef_terms_2 <- as.character(attr(attr(mod@frame, "terms"), "predvars.fixed"))
fixef_terms_3 <- as.character(attr(attr(mod_2@frame, "terms"), "predvars.fixed"))



fixef_terms <- fixef_terms[-c(1:2)]

fixef_terms_test <- fixef_terms_3[c("d_MAT", "harvestF", "tree_cover")]
fixef_terms_test

fixef_terms
fixef_terms_2
fixef_terms_3

# Create a vector with the unwanted variables 
KeepVars <- c("d_MAT", "harvestF")
# use subsetting to remove them
fixef_terms_3_test <- fixef_terms_3[ , (names(fixef_terms_3) %in% KeepVars)]

grepl("survival", attr(attr(mod@frame, "terms"),"predvars.fixed"))

str(mod)

mod
