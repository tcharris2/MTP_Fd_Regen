

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
