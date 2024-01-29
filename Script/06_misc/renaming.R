test <- names(regen %>% 
  list(all_of(starts_with("d_"))))


test  <- names(regen %>% select(starts_with("d_")))

test <- names(regen)

test %>% select(matches("d_"))


test <- names(regen %>% 
      select(map(c('d_'), 
             starts_with, 
             vars = colnames(.)) %>% 
           unlist()))



