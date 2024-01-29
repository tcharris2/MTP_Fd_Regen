plot(resid(height_group_harvest_models$h_group_model_harvest[[1]]))
plot(resid(height_group_harvest_models$h_group_model_harvest[[1]]))
qqnorm(resid(height_group_harvest_models$h_group_model_harvest[[1]]))

resids <- resid(height_group_harvest_models$h_group_model_harvest[[1]], level = 4)

ggplot() + 
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]], level = 4), 
                  y = resid(height_group_harvest_models$h_group_model_harvest[[1]], level = 4)))

ggplot() + 
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest[[1]])))

ggplot() + 
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_2[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_2[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_2[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_2[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_3[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_3[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_3[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_3[[1]]))) +
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_age_har[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_age_har[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_age_har[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_age_har[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1a[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1a[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1a[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1a[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_2a[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_2a[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_2a[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_2a[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_3a[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_3a[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_3a[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_3a[[1]])))



ggplot() + 
  geom_line(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                y = resid(height_group_harvest_models$h_group_model_harvest[[1]]))) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest[[1]])))+ 
  geom_line(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]]))) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]])))


resid(height_group_harvest_models$h_group_model_harvest[[1]], level = 4)

ggplot() +
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[1]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[1]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[2]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[2]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[2]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[2]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[3]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[3]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[3]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[3]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[4]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[4]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[4]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[4]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[5]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[5]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[5]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[5]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[6]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[6]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[6]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[6]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[7]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[7]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[7]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[7]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[8]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[8]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[8]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[8]]))) + 
  
  geom_point(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[9]]), 
                 y = resid(height_group_harvest_models$h_group_model_harvest_1[[9]])), size = 0.25) +
  geom_smooth(aes(x = fitted(height_group_harvest_models$h_group_model_harvest_1[[9]]), 
                  y = resid(height_group_harvest_models$h_group_model_harvest_1[[9]])))
