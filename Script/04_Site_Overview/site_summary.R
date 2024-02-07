####### OVERVIEW OF SITES ################
#' @Content: Overview of the sites ---------------------------------------------

#' @Author: Thomson Harris
#' @Date: Oct 4th, 2023

# 1. Importing Data ---------------------------------------------------------------

regen <- read.csv(here("Data/03_Processed", "20231201_survival_fd_b_processed.csv"), header = TRUE)


regen <- regen[regen$blockNo != 9 & regen$blockNo != 13, ]

regen <- subset(regen, !(is.na(tree_cover)))

regen <- subset(regen, !regen$provenance %in% c("Jaffray future Fd",  "John Prince future Fd",
                                                "Peterhope future Fd", "Alex Fraser future Fd", 
                                                 "Twobit B class Fd"))

# nesting data
loc_group_summary <- regen %>% 
  group_by(location) %>% 
  nest()

loc_group_summary

# 2. Survival by Location ------------------------------------------------------


# new dataframe with an empty column
loc_group_survival_summary <- loc_group_summary

loc_group_survival_summary$avg_survival <- NA

# for loop to calculate averages per location
for (i in 1:nrow(loc_group_survival_summary)){
  
  avg_survival <- mean(loc_group_survival_summary[[2]][[i]]$alive)
  
  loc_group_survival_summary$avg_survival[[i]] <- avg_survival
  
}

# viewing and reducing dataframe
loc_group_survival_summary$avg_survival

loc_group_survival_summary <- loc_group_survival_summary %>% select(-contains(c("data")))

loc_group_survival_summary

###### 2.1 Graphing avg survival ----
ggplot(data = loc_group_survival_summary) +
  geom_bar(mapping = aes(x = location, y = avg_survival), stat = "identity") +
  labs(title = "loc_group_survival_summary")
  



# 3. Height by Location --------------------------------------------------------

# new dataframe with an empty column
loc_group_height_summary <- loc_group_summary

loc_group_height_summary$avg_height <- NA

# for loop to calculate averages per location
for (i in 1:nrow(loc_group_height_summary)){
  
  avg_height <- mean(loc_group_height_summary[[2]][[i]]$height, na.rm = TRUE)
  
  loc_group_height_summary$avg_height[[i]] <- avg_height
  
}

# viewing and reducing dataframe
loc_group_height_summary$avg_height

loc_group_height_summary <- loc_group_height_summary %>% select(-contains(c("data")))

loc_group_height_summary

###### 3.1 Graphing avg height ---- 
ggplot(data = loc_group_height_summary) +
  geom_bar(mapping = aes(x = location, y = avg_height), stat = "identity") +
  labs(title = "loc_group_height_summary")

ggplot(data = loc_group_height_summary) +
  geom_bar(mapping = aes(x = location, y = log(avg_height)), stat = "identity") +
  labs(title = "loc_group_ln_height_summary")


# 4. Spread of Climatic Variables ----------------------------------------------

###### 4.1 Standard ----

climatic_vars_df <- regen %>% select(starts_with(c("p_", "s_", "d_")))

# range_df <- data.frame(t(sapply(climatic_vars_df, range)))

range_df <- Map(function(c_var) cbind(c_var, min = min(climatic_vars_df[ ,c_var]), 
                                  mean = mean(unique(climatic_vars_df[ ,c_var])),
                                  max = max(climatic_vars_df[ ,c_var])),
                                  as.list(names(climatic_vars_df)))

range_df <- data.frame(do.call(rbind, range_df))

range_df



range_df$min <- as.numeric(range_df$min)
range_df$mean <- as.numeric(range_df$mean)
range_df$max <- as.numeric(range_df$max)


####### 4.1.1 Graphing Standard -----

# Create subsets by variable type
prov_climatic_vars_df <- range_df[grep("p_", range_df$c_var), ]
site_climatic_vars_df <- range_df[grep("s_", range_df$c_var), ]
dist_climatic_vars_df <- range_df[grep("d_", range_df$c_var), ]

# dist_climatic_vars_df$c_var <- fct_reorder(dist_climatic_vars_df$c_var, dist_climatic_vars_df$min, .desc = TRUE)

ggplot(prov_climatic_vars_df, aes(y = c_var))+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean))
  
ggsave(paste0(Sys.Date(), "prov_climatic_range.pdf"))


ggplot(site_climatic_vars_df, aes(y = c_var), inherit.aes = TRUE)+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean))
  
ggsave(paste0(Sys.Date(), "site_climatic_range.pdf"))


ggplot(dist_climatic_vars_df, aes(y = c_var), inherit.aes = TRUE)+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean))

ggsave(paste0(Sys.Date(), "dist_climatic_range.pdf"))



###### 4.2 Normalized ----

regen_normalized <- regen %>%
  mutate(across(starts_with(c("p_", "s_", "d_")), scale))

norm_range_df <- regen_normalized %>% select(starts_with(c("p_", "s_", "d_")))

# normalized_range_df <- data.frame(t(sapply(normalized_climatic_vars_df, range)))


norm_range_df <- Map(function(c_var) cbind(c_var, min = min(norm_range_df[,c_var]), 
                            mean = mean(unique(norm_range_df[,c_var])),
                            max = max(norm_range_df[,c_var])), 
                            as.list(names(norm_range_df)))

norm_range_df <- data.frame(do.call(rbind, norm_range_df))

norm_range_df

norm_range_df$min <- as.numeric(norm_range_df$min)
norm_range_df$mean <- as.numeric(norm_range_df$mean)
norm_range_df$max <- as.numeric(norm_range_df$max)


####### 4.2.1 Graphing Normalized -----

# Create subsets by variable type

norm_prov_climatic_df <- norm_range_df[grep("p_", norm_range_df$c_var), ]
norm_site_climatic_df <- norm_range_df[grep("s_", norm_range_df$c_var), ]
norm_dist_climatic_df <- norm_range_df[grep("d_", norm_range_df$c_var), ]


ggplot(norm_prov_climatic_df, aes(y = c_var))+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean))

ggsave(paste0(Sys.Date(), "norm_prov_climatic_range.pdf"))


ggplot(norm_site_climatic_df, aes(y = c_var))+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean)) 

ggsave(paste0(Sys.Date(), "norm_site_climatic_range.pdf"))


ggplot(norm_dist_climatic_df, aes(y = c_var))+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  geom_point(aes(x = mean)) 

ggsave(paste0(Sys.Date(), "norm_dist_climatic_range.pdf"))



# 5. Climate at Each Site ------------------------------------------------------

###### 5.1 Standard ----

# filling dataframe with ranges 
group_climatic_vars_df <- regen %>% select(location, starts_with(c("p_", "s_", "d_")))

group_climatic_vars_df <- group_climatic_vars_df %>% 
  group_by(location) %>% 
  nest()

group_climatic_vars_df$group_range_df <- list(NA) # list() included to give the correct data format

# for loop to pull all the ranges
for (i in 1:nrow(group_climatic_vars_df)){

    group_range_df <- Map(function(c_var) cbind(c_var, min = min(group_climatic_vars_df$data[[i]][, c_var]), 
                                     mean_list = c(unique(group_climatic_vars_df$data[[i]][, c_var])),
                                     max = max(group_climatic_vars_df$data[[i]][,c_var])), 
                                     as.list(names(group_climatic_vars_df$data[[i]])))

    group_range_df <- data.frame(do.call(rbind, group_range_df))

    group_climatic_vars_df$group_range_df[[i]] <- group_range_df
  
}

group_climatic_vars_df

# unnesting the dataframe so the data is in the right dimensions
group_climatic_vars_df <- group_climatic_vars_df %>%
  unnest(c(group_range_df))

group_climatic_vars_df

# Calculating means 
group_climatic_vars_df$mean <- NA

for (i in 1:nrow(group_climatic_vars_df)) {
  
  mean <- mean(group_climatic_vars_df$mean_list[[i]])
  
  group_climatic_vars_df$mean[[i]] <- mean
  
}

# unnesting variables 
group_climatic_vars_df <- group_climatic_vars_df %>%
  unnest(c(c_var, min, max))

group_climatic_vars_df


# Creating 3 seperate dataframes 
group_prov_climatic_df <- group_climatic_vars_df[grep("p_", group_climatic_vars_df$c_var), ]
group_site_climatic_df <- group_climatic_vars_df[grep("s_", group_climatic_vars_df$c_var), ]
group_dist_climatic_df <- group_climatic_vars_df[grep("d_", group_climatic_vars_df$c_var), ]

###### 5.1.1 Graphing Standard ----

ggplot(group_prov_climatic_df, aes(y = c_var)) +
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min)) +
  geom_point(aes(x = max)) +
  geom_point(aes(x = mean)) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)

ggplot(group_site_climatic_df, aes(y = c_var))+
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min))+
  geom_point(aes(x = max))+
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)

ggplot(data = group_site_climatic_df) +
  geom_bar(mapping = aes(x = c_var, y = mean), stat = "identity")+
  facet_wrap( ~ location, nrow = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(group_dist_climatic_df, aes(y = c_var)) +
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min)) +
  geom_point(aes(x = max)) +
  geom_point(aes(x = mean)) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)


###### 5.2 Normalized  ----

regen_normalized <- regen %>%
  mutate(across(starts_with(c("p_", "s_", "d_")), scale))

# filling dataframe with ranges 
norm_group_climatic_vars_df <- regen_normalized %>% select(location, starts_with(c("p_", "s_", "d_")))


norm_group_climatic_vars_df <- norm_group_climatic_vars_df %>% 
  group_by(location) %>% 
  nest()

norm_group_climatic_vars_df$norm_group_range_df <- list(NA) # list() included to give the correct data format

# for loop to pull all the ranges
for (i in 1:nrow(norm_group_climatic_vars_df)){
  
  norm_group_range_df <- Map(function(c_var) cbind(c_var, min = min(norm_group_climatic_vars_df$data[[i]][,c_var]), 
                                          mean_list = c(unique(norm_group_climatic_vars_df$data[[i]][,c_var])),
                                          max = max(norm_group_climatic_vars_df$data[[i]][,c_var])), 
                                          as.list(names(norm_group_climatic_vars_df$data[[i]])))
  
  norm_group_range_df <- data.frame(do.call(rbind, norm_group_range_df))
  
  norm_group_climatic_vars_df$norm_group_range_df[[i]] <- norm_group_range_df
  
}

norm_group_climatic_vars_df

# unnesting the dataframe so the data is in the right dimensions
norm_group_climatic_vars_df <- norm_group_climatic_vars_df %>%
  unnest(c(norm_group_range_df))

norm_group_climatic_vars_df

# Calculating means 
norm_group_climatic_vars_df$mean <- NA

for (i in 1:nrow(norm_group_climatic_vars_df)) {
  
  mean <- mean(norm_group_climatic_vars_df$mean_list[[i]])
  
  norm_group_climatic_vars_df$mean[[i]] <- mean
  
}

# unnesting variables 
norm_group_climatic_vars_df <- norm_group_climatic_vars_df %>%
  unnest(c(c_var, min, max))

norm_group_climatic_vars_df


# Creating 3 seperate dataframes 
norm_group_prov_climatic_df <- norm_group_climatic_vars_df[grep("p_", norm_group_climatic_vars_df$c_var), ]
norm_group_site_climatic_df <- norm_group_climatic_vars_df[grep("s_", norm_group_climatic_vars_df$c_var), ]
norm_group_dist_climatic_df <- norm_group_climatic_vars_df[grep("d_", norm_group_climatic_vars_df$c_var), ]

# adding means to prov and dist 

####### 5.2.1 Graphing Normalized -----

ggplot(norm_group_prov_climatic_df, aes(y = c_var)) +
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min)) +
  geom_point(aes(x = max)) +
  geom_point(aes(x = mean)) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)

ggplot(norm_group_site_climatic_df, aes(y = c_var)) +
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min)) +
  geom_point(aes(x = max)) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)

ggplot(data = norm_group_site_climatic_df) +
  geom_bar(mapping = aes(x = c_var, y = mean), stat = "identity")+
  facet_wrap( ~ location, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(norm_group_dist_climatic_df, aes(y = c_var)) +
  geom_linerange(aes(xmin = min, xmax = max)) +
  geom_point(aes(x = min)) +
  geom_point(aes(x = max)) +
  geom_point(aes(x = mean)) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.25) +
  facet_wrap( ~ location, nrow = 2)



# 6. Average Tree Cover by Harvest ---------------------------------------------

summary(regen)

# dropping NAs and creating a new dataframe 
regen_canopy <- regen %>% drop_na(tree_cover)

harvest_grouped <- regen %>% 
  group_by(harvest_name) %>% 
  nest()

harvest_group_summary <- harvest_grouped

harvest_group_summary$avg_cover <- NA

# for loop to pull means
for (i in 1:nrow(harvest_group_summary)) {
  
  mean_cover <- mean(harvest_group_summary[[2]][[i]]$tree_cover)
  
  harvest_group_summary$avg_cover[[i]] <- mean_cover
  
  
}

harvest_group_summary


###### 6.0.1 Graphing Means ----

# bar graph
ggplot(data = harvest_group_summary) +
  geom_bar(mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
                         y = avg_cover), stat = "identity") +
  geom_text(aes(label = avg_cover))
  labs(x = "harvest_name", title = "tree cover by harvest")

ggplot(data = harvest_group_summary,
    mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
    y = avg_cover)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(avg_cover))) +
  labs(x = "harvest_name", title = "tree cover by harvest")

# box plot 
ggplot(data = regen) +
  geom_boxplot(mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
                             y = tree_cover)) + 
  labs(x = "harvest_name", title = "tree cover by harvest")

# box plot by location
ggplot(data = regen) +
  geom_boxplot(mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
                             y = tree_cover)) + 
  facet_wrap( ~ location, nrow = 2) + 
  labs(x = "harvest_name", title = "tree cover by harvest")

# scatterplot by location 
ggplot(data = regen) +
  geom_point(mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
                           y = tree_cover), position = "jitter") + 
  facet_wrap( ~ location, nrow = 2)+ 
  xlab("harvest_name")



###### 6.1 Tree Cover Ranges -------

group_tree_cover_df <- regen_canopy %>% select(c(location, harvest_name, tree_cover, cover))

# nest by location and harvest  
group_tree_cover_df <- regen_canopy %>% 
  group_by(location, harvest_name) %>% 
  nest()

group_tree_cover_df

# for loop to store means 
group_tree_cover_df$mean <- NA

for (i in 1:nrow(group_tree_cover_df)) {
  
  mean <- mean(group_tree_cover_df[[3]][[i]]$tree_cover)
  
  group_tree_cover_df$mean[[i]] <- mean
  
}

group_tree_cover_df

# graphing
ggplot(data = group_tree_cover_df) +
  geom_bar(mapping = aes(x = factor(harvest_name, level = c('clearcut', 'seed', '30Ret', '60Ret')), 
                         y = mean), stat = "identity") + 
  facet_wrap( ~ location, nrow = 2) + 
  xlab("harvest_name")






