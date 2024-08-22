

model_1 <- lmer(ln_height ~ tree_cover + (1|locationF/blockF/plotF/splitplotF), 
              data = regen_height, REML = FALSE, 
              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


model_2 <- lmer(log(height) ~ sqrt(tree_cover) * d_MAP^2 + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_3 <- lmer(ln_height ~ sqrt_tree_cover + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


model_1
model_2


plot_model(model_1, type = "pred", terms = c("tree_cover"))


plot_model(model_2, type = "pred", terms = c("tree_cover", "d_MAP[-1, 0, 1]"))

plot_model(model_2, type = "pred", terms = c("d_MAP", "tree_cover[0, 30, 60]"))



plot_model(model_3, type = "pred", terms = c("sqrt_tree_cover"))


regen$test_1 <- (regen$s_MAP)^2 - (regen$p_MAP)^2
test_1a <- scale(test_1)

regen$test_2 <- (regen$s_MAP) - (regen$p_MAP)

regen$test_3 <- test_2^2

test_4 <- scale(test_3)
regen$test_4 <- scale(test_3)
summary(test_1a)
summary(test_4)

model_a <- lmer(ln_height ~ d_MAP + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_b <- lmer(ln_height ~ d_MAP^2 + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_c <- lmer(ln_height ~ test_3 + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_d <- lmer(ln_height ~ test_4 + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


model_a
model_b
model_c
model_d


regen_height


MAT_1 <- (regen$s_MAP) - (regen$p_MAP)

MAT_1 <- MAT_1^2

MAT_1 <- scale(MAT_1)

MAT_2 <- (regen$s_MAP) - (regen$p_MAP)

MAT_2 <- scale(MAT_2)

MAT_3 <- (regen$s_MAP) - (regen$p_MAP)

MAT_3 <- scale(MAT_3^2)


MAT_1 == MAT_2


model_MAP <- lmer(log(height) ~ scale(d_MAP) * sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), 
                data = regen_height, REML = FALSE, 
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_MAP_1 <- lmer(log(height) ~ scale(d_MAP) + (1|locationF/blockF/plotF/splitplotF), 
                    data = regen_height, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_MAP_2 <- lmer(log(height) ~ scale(d_MAP^2) + (1|locationF/blockF/plotF/splitplotF), 
                    data = regen_height, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_MAP_3 <- lmer(log(height) ~ scale(d_MAP) + scale(d_MAP^2) + (1|locationF/blockF/plotF/splitplotF), 
                  data = regen_height, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_NULL <- lmer(log(height) ~ 1+ (1|locationF/blockF/plotF/splitplotF), 
                  data = regen_height, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(model_NULL, model_MAP_1, model_MAP_2, model_MAP_3)



model_PAS_1 <- lmer(log(height) ~ scale(d_PAS) + (1|locationF/blockF/plotF/splitplotF), 
                    data = regen_height, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_PAS_2 <- lmer(log(height) ~ scale(d_PAS^2) + (1|locationF/blockF/plotF/splitplotF), 
                    data = regen_height, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_PAS_3 <- lmer(log(height) ~ scale(d_PAS) + scale(d_PAS^2) + (1|locationF/blockF/plotF/splitplotF), 
                    data = regen_height, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_NULL <- lmer(log(height) ~ 1+ (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(model_NULL, model_PAS_1, model_PAS_2, model_PAS_3)


lrtest(model_MAP_1, model_MAP_2)

plot_model(model_MAP, type = "emm", terms = c("d_MAP", "tree_cover [0, 25, 50]"))


plot_model(model_MAP, type = "int", terms = c("tree_cover"), se = 0.8)


unique(regen_height$d_MAP)

library(ggeffects)
pr <- ggpredict(
  model_MAP,
  terms=c('d_MAP')
)

pr

plot(pr)

blank <-regen_height$d_MAP
test <- scale(regen_height$d_MAP)

test_1 <- scale(regen_height$d_MAP^2)

summary(test_1)

plot(test, regen_height$height)
plot(test_1, regen_height$height)
plot(blank, regen_height$height)


scale_tree_cov <- scale(regen_height$tree_cover)
summary(scale_tree_cov)

sqrt_tree_cov <- sqrt(regen_height$tree_cover)

plot(scale_tree_cov, regen_height$height)
plot(sqrt_tree_cov, regen_height$height)



model_NULL <- lmer(log(height) ~ 1 + (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_sqrt_tree <- lmer(log(height) ~ sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_scale_tree <- lmer(log(height) ~ log(scale(tree_cover)) + (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

AIC(model_NULL, model_sqrt_tree, model_scale_tree)

model_sqrt_tree
model_scale_tree

plot(model_scale_tree)

sum(scale_tree_cov >= 5)


model_NULL <- lmer(log(height) ~ 1 + (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_age <- lmer(log(height) ~ age + (1|locationF/blockF/plotF/splitplotF), 
                         data = regen_height, REML = FALSE, 
                         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_scale_age <- lmer(log(height) ~ scale(age) + (1|locationF/blockF/plotF/splitplotF), 
                  data = regen_height, REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

AIC(model_NULL, model_age, model_scale_age)

# Survival -------------------------------------------------------------------


model_NULL <- glmer(survival ~ 1 + (1|locationF/blockF/plotF/splitplotF), data = regen_survival,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_cov <- glmer(survival ~ tree_cover + (1|locationF/blockF/plotF/splitplotF), data = regen_survival,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_sqrt_cov <- glmer(survival ~ sqrt(tree_cover) + (1|locationF/blockF/plotF/splitplotF), data = regen_survival,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_sqrt_cov_2 <- glmer(survival ~ sqrt(tree_cover) + tree_cover + (1|locationF/blockF/plotF/splitplotF), data = regen_survival,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


AIC(model_NULL, model_cov, model_sqrt_cov, model_sqrt_cov_2)

lrtest(model_cov, model_sqrt_cov_2)

model_cov

model_sqrt_cov

# Squard GLMER models --------------------------------------------------------

harvest_1_AIC <- NA

for (i in 1:length(harvest_1)) {
  
  AIC_val <- AIC(harvest_1[[i]])

  harvest_1_AIC[[i]] <- AIC_val  
}

harvest_1_sqrd_AIC <- NA

for (i in 1:length(harvest_1_sqrd)) {
  
  AIC_val <- AIC(harvest_1_sqrd[[i]])
  
  harvest_1_sqrd_AIC[[i]] <- AIC_val  
}


df <- data.frame(harvest_1_AIC, harvest_1_sqrd_AIC, ClimaticVarList)

df

p_vals <- read.csv(here("Data/05_Output", "2024-03-07_Survival_group_squared_terms.csv"), 
                   header = TRUE)

new_df <- cbind(p_vals, df)

new_df

var <- "d_MAT"

var %in% ClimaticVarList

SqrdVarList <- c("d_MAT", "d_MCMT", "d_MAP", "d_AHM", "d_NFFD", 
                 "d_FFP", "d_EMT", "d_CMD", "d_RH")

if (var %in% SqrdVarList) {
  
  paste("^2)")
  
} else {
  
  paste(")")
  
}


paste0("scale(", var, if (var %in% SqrdVarList) {
  
  paste("^2)")
  
} else {
  
  paste(")")
  
})
