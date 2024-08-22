mod_0 <- ln_height_cover_models$s_group_model_null[[15]]

mod_1 <- ln_height_cover_models$s_group_model_harvest_1[[15]]

mod_2 <- ln_height_cover_models$s_group_model_harvest_2[[15]]
  
mod_3 <- ln_height_cover_models$model_3[[8]]

mod_4 <- ln_height_cover_models$model_3[[4]]

lrtest(mod_0, mod_3)

AIC(mod_0, mod_1, mod_2, mod_3)

mod_1
mod_2
mod_3
mod_4

library(emmeans)

df <- regen_height

emmip(mod_3, tree_cover ~ d_NFFD, cov.reduce = range, 
      lmerTest.limit = 5809, pbkrtest.limit = 5809)

emtrends(mod_3, pairwise ~ tree_cover, var = "d_NFFD", cov.reduce = range, 
      lmerTest.limit = 5809, pbkrtest.limit = 5809)

joint_tests(mod_3, by = "d_NFFD", 
            lmerTest.limit = 5809, pbkrtest.limit = 5809)



mod <- glmer(survival ~ d_MAP + (1|locationF/blockF/plotF/splitplotF), 
             data = regen_survival, family = binomial, 
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

mod_scale <- glmer(survival ~ scale(d_MAP) + (1|locationF/blockF/plotF/splitplotF), 
             data = regen_survival, family = binomial, 
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


plot_models(mod, mod_scale)
plot_model(mod) + ylim(1, 1.01)
plot_model(mod_scale)

mod <- lmer(log(height) ~ d_RH + (1|locationF/blockF/plotF/splitplotF), 
             data = regen_height, REML = FALSE,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

mod_scale <- lmer(log(height) ~ scale(d_RH) + (1|locationF/blockF/plotF/splitplotF), 
                   data = regen_height, REML = FALSE,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

mod
mod_scale
plot_models(mod, mod_scale) + ylim(0, 0.2)
plot_model(mod) + ylim()
plot_model(mod_scale, transform = NULL)

myfun <- function(x) {
  exp(x)
}
plot_model(mod_scale, transform = "myfun", show.values = TRUE)

