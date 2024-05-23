funfun <- function(inc = 1){
  dataEnv <- new.env(parent=globalenv())
  dataEnv$d1 <- 1 + inc
  dataEnv$d2 <- 2 + inc
  dataEnv$d3 <- rnorm(10000)
  return(dataEnv)
}


myEnv <- funfun()
object.size(myEnv)

head(myEnv$d3)

obj <- tibble(myEnv$d3)

model_obj <- background_job_results$s_group_model_age_can
model_obj

model_null <- background_job_results$s_group_model_null_canopy
model_null

obj1 <<- 1
