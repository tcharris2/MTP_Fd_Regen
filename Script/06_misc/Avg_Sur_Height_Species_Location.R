
regen_base <- read.csv(here("Data/03_Processed", "20231201_regen_merged.csv"), header=TRUE)

by_loc <- regen_base %>% 
  group_by(location, species) %>% 
  nest()



by_loc$avg_survival <- NA

for(i in 1:nrow(by_loc)) {
  
  mean_sur <- mean(by_loc$data[[i]][["alive"]])
  
  by_loc$avg_survival[[i]] <- mean_sur
  
}



by_loc$avg_height <- NA

for(i in 1:nrow(by_loc)) {
  
  mean_height <- mean(by_loc$data[[i]][["height"]], na.rm = TRUE)
  
  by_loc$avg_height[[i]] <- mean_height
  
}

by_loc



by_loc$avg_survival <- 100 * by_loc$avg_survival

# AVG survival
ggplot(data = by_loc, 
       aes(x = fct_reorder(location, avg_survival), y = avg_survival, fill = location)) +
  facet_wrap(~ species) +
  
  geom_col(width = 0.80, colour = "black") +
  
  geom_hline(yintercept = 0, colour = "black") +
  
  scale_fill_viridis_d() +
  
  geom_text(aes(label = round(avg_survival, 1)), vjust = -0.3) +
  
  guides(fill = "none") +
  
  labs(title = NULL,
       x = NULL,
       y = "Average Survival (%)",
       size = 12,
       fill = "Survival (%)") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major.y = element_line(color = "gray80", linewidth = .05),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# AVG Height
ggplot(data = by_loc, 
       aes(x = fct_reorder(location, avg_height), y = avg_height, fill = location)) +
  facet_wrap(~ species) +
  
  geom_col(width = 0.80, colour = "black") +
  
  geom_hline(yintercept = 0, colour = "black") +
  
  scale_fill_viridis_d() +
  
  geom_text(aes(label = round(avg_height, 1)), vjust = -0.3) +
  
  guides(fill = "none") +
  
  labs(title = NULL,
       x = NULL,
       y = "Average Height (cm)",
       size = 12,
       fill = "Height (cm)") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major.y = element_line(color = "gray80", linewidth = .05),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

