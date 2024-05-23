
library(survival)
library(sjPlot)


lung <- 
  lung %>% 
  mutate(status = recode(status, `1` = 0, `2` = 1))

lung_mod <- glm(status ~ meal.cal * factor(sex), data = lung, family = "binomial", na.action = na.omit)

plot_model(lung_mod, type = "pred",
           terms = c("meal.cal [all]", "sex"))


emtrends(lung_mod, pairwise ~ sex, var = "meal.cal")

emmip(lung_mod, sex ~ meal.cal, cov.reduce = range)



flchain


fl_mod <- glm(death ~ kappa * factor(flc.grp), data = flchain, family = "binomial", na.action = na.omit)

plot_model(fl_mod, type = "pred",
           terms = c("kappa [all]", "flc.grp"), se = FALSE)


emtrends(fl_mod, pairwise ~ flc.grp, var = "kappa")

emmip(lung_mod, flc.grp ~ kappa, cov.reduce = range)
