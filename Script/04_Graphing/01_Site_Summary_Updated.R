####### OVERVIEW OF SITES ################
#' @Content: Overview of the sites ---------------------------------------------

#' @Author: Thomson Harris
#' @Date: Oct 4th, 2023

# 1. Importing Data ---------------------------------------------------------------

### 1.1. Loading Packages -----
library(here)
library(tidyverse)
library(ggpubr)
library(reshape2)

### 1.2. Loading Data ------
regen <- read.csv(here("Data/03_Processed", "20240602_survival_fd_b_processed.csv"), 
                  header = TRUE)

regen$location[regen$location == "Jaffray"] <- "Cranbrook"

### 1.3. Nesting data -----
loc_group_summary <- regen %>% 
  group_by(location) %>% 
  nest()

loc_group_summary

# Location order for graphing 
# Increasing RH
location_order <- c("Alex Fraser", "Jaffray", "John Prince", "Twobit", "Redfish", "Narrows")


# 2. Organizing Data ------------------------------------------------------

# new dataframe with an empty column
loc_group_summary$avg_survival <- NA

loc_group_summary$avg_height <- NA

###### 2.1  for-loop Survival ----
for (i in 1:nrow(loc_group_summary)){
  
  avg_survival <- mean(loc_group_summary[[2]][[i]]$alive)
  
  loc_group_summary$avg_survival[[i]] <- avg_survival
  
}


###### 2.2  for-loop Height ----
for (i in 1:nrow(loc_group_summary)){
  
  avg_height <- mean(loc_group_summary[[2]][[i]]$height, na.rm = TRUE)
  
  loc_group_summary$avg_height[[i]] <- avg_height
  
}



# viewing and reducing dataframe
loc_group_summary$avg_survival <- 100 * loc_group_summary$avg_survival


# loc_group_summary <- loc_group_summary %>% select(-contains(c("data")))

loc_group_summary

####### 2.3 for-loop Site Climate -----------------

SiteClimaticVarList <- names(regen %>% select(starts_with("s_")))

SiteClimaticVarList <- SiteClimaticVarList[c(2, 3, 6, 1, 8, 7, 5, 4, 9)]
SiteClimaticVarList


regen_normalized <- regen %>%
  mutate(across(starts_with(c("s_")), scale))

# filling dataframe with ranges 
location_c_vars <- regen_normalized %>% select(location, starts_with(c("s_")))

location_c_vars <- location_c_vars %>% 
  group_by(location) %>% 
  nest()

location_c_vars[SiteClimaticVarList] <- NA

location_c_vars

for (i in 1:length(SiteClimaticVarList)) {
  
  C_VAR <- SiteClimaticVarList[[i]]
  
  for (j in 1:nrow(location_c_vars)) {
    
    value <- unique(location_c_vars[["data"]][[j]][[C_VAR]])
    
    location_c_vars[[C_VAR]][[j]] <- value
    
  }
  
}

location_c_vars

site_c_vars_df <- subset(location_c_vars, select = -c(data))

site_c_vars_df <- melt(site_c_vars_df, id.var = "location", variable.name = "Site_C_Var")

site_c_vars_df

#  3.  Graphing  ---------------------------------------------------------------

###### 3.1 Survival ----

survival_graph <- ggplot(data = loc_group_summary, 
                     aes(x = factor(location, levels = location_order), 
                         y = avg_survival, 
                         fill = location)) +
  
  geom_col(width = 0.80, colour = "black") +
  
  geom_hline(yintercept = 0, colour = "black") +
  
  scale_fill_viridis_d() +
  
  geom_text(aes(label = round(avg_survival, 1)), vjust = -0.3, size = 4) +
  
  guides(fill = "none") +
  
  labs(title = NULL,
       x = NULL,
       y = "Average Survival (%)",
       size = 12,
       fill = "Survival (%)") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title = element_text(size = 18, face = "bold"),
        text = element_text(family = "Times"))


survival_graph

####### 3.2 Height -------


height_graph <- ggplot(data = loc_group_summary, 
                   aes(x = factor(location, levels = location_order),
                       y = avg_height, 
                       fill = location)) +
  
  geom_col(width = 0.80, color = "black") +
  
  geom_hline(yintercept = 0, colour = "black") +
  
  scale_fill_viridis_d() +
  
  geom_text(aes(label = round(avg_height, 1)), vjust = -0.3, size = 4) +
  
  guides(fill = "none") +
  
  labs(title = NULL,
       x = "Location",
       y = "Average Height (cm)",
       size = 12,
       fill = "Height (cm)") +
  
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title = element_text(size = 18, face = "bold"),
        text = element_text(family = "Times"))


height_graph
###### 3.3 Avg Site Climate ------------
site_c_vars_df <- site_c_vars_df %>% arrange(factor(location, levels = location_order))

site_c_vars_df$location <- as.factor(site_c_vars_df$location)


# graphing 
site_summary <- ggplot(data = transform(site_c_vars_df,
                                 location = factor(location, levels=location_order)), 
                       aes(x = Site_C_Var, y = value, fill = Site_C_Var)) +
  
  geom_col(width = 0.80, color = "black") +
  
  geom_hline(yintercept = 0, colour = "black") +
  
  labs(title = NULL,
       x = NULL,
       y = bquote(bold("Normalized Location Climatic Variables"^"a")),
       size = 16) +

  
  scale_x_discrete(labels = c(bquote(MAP[Ls]), bquote(MSP[Ls]), bquote(PAS[Ls]), 
                              bquote(MAT[Ls]), bquote(EXT[Ls]), bquote(EMT[Ls]), 
                              bquote(NFFD[Ls]), bquote(AHM[Ls]), bquote(RH[Ls]))) +
  
  geom_text(aes(y = value + 0.5 * sign(value), label = round(value, 1)), 
            position = position_dodge(width = 0.8), 
            size = 4) +
  
  facet_wrap( ~ location, ncol = 1) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "white", color = "black", linewidth = 0.75),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 12),
        text = element_text(family = "Times"))

site_summary
# 4. Grouping Graphs ------------------------------------------------------

ggarrange(site_summary,
          ggarrange(survival_graph, height_graph, nrow = 2, labels = c("B.", "C."), align = "hv"),
          ncol = 2, labels = "A.", widths = c(1, 1), align = "v")

# Save this graph at 1400 x 1000
