sjPlot::plot_model(harvest_2a_models[["model_2a"]][[1]],
                   type = "std", show.p = TRUE, show.values = TRUE,
                   axis.labels=c("Age", "60Ret", "30Ret", "SeedTree", "MWMT")) +
  
  ylim(-0.911, 0.911) +
  geom_hline(yintercept = 0, colour = "green4") +
  
  labs(y = "Standarized Beta Coefficents",
       title = "MWMT Beta Estimates") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major = element_line(color = "gray40", linewidth = .05),
        panel.grid.minor = element_blank()) 


graphHeighBeta <- function (df){
  
  
  MODEL_NAME <- names(df %>% select(starts_with("model")))
  
  for(i in 1:nrow(df)) {
    
    C_VAR <- df[["ClimaticVarList"]][[i]]
    
    print(sjPlot::plot_model(df[[MODEL_NAME]][[i]],
                             type = "std", show.p = TRUE, show.values = TRUE) +
            
            ylim(-0.911, 0.911) +
            geom_hline(yintercept = 0, colour = "green4") +
            
            labs(y = "Standarized Beta Coefficents",
                 title = paste(C_VAR, "Beta Estimates")) +
            
            theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
                  panel.grid.major = element_line(color = "gray40", linewidth = .05),
                  panel.grid.minor = element_blank()) )
    
    ggsave(paste(Sys.Date(), C_VAR, MODEL_NAME,
                 
                 "Height_allLocs_Std_Beta_Coefs.pdf", sep = "_"))
    
  }
  

}
