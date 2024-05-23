library(sjPlot)
library(ggplot2)

mtcars <- mtcars

mod_disp <- lm(log(mpg) ~ disp, data = mtcars) 
mod_hp <- lm(log(mpg) ~ hp, data = mtcars) 


plot_models(mod_disp, mod_hp, show.values = TRUE) + 
  ylim(-0.1, 0.1)

