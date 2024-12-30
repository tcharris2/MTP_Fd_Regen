
# Packages
library(here)
library(lmtest)
library(sjPlot)

# Survival - Harvest ------------------------------------------------------------
survival_harvest_mods <- readRDS(file = here("Data/04_Temp", "2024-06-24_survival_harvest_models.rds"))

survival_harvest_mods

RH_mod_sh <- survival_harvest_mods[9, ]
RH_mod_sh
RH_mod_sh$model_1 <- c(RH_mod_sh[[3]][[1]][["d_RH"]])
RH_mod_sh

with(RH_mod_sh, lrtest(model_0[[1]], model_h[[1]]))
with(RH_mod_sh, lrtest(model_0[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(RH_mod_sh, AIC(model_0[[1]], model_h[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(RH_mod_sh, lrtest(model_1[[1]], model_3[[1]]))


with(RH_mod_sh, tab_model(model_0[[1]], model_h[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))



tab <- tab_model(RH_mod_sh$model_3[[1]],
          pred.labels = c("Intercept", "RH",
                          "Seed Tree", "30% Retention", "60% Retention",
                          "RH:Seed Tree", "RH:30% Retention","RH:60% Retention"),
          string.ci = "CI (95%)",
          string.p = "P-Value",
          CSS = list(css.table = '+font-family: Times;')) 

tab$page.complete <- gsub("locationF","Location",
                          tab$page.complete)
tab$page.complete <- gsub("blockF","Block",
                          tab$page.complete)
tab$page.complete <- gsub("plotF","Plot",
                          tab$page.complete)
tab$page.complete <- gsub("splitplotF","Split-Plot",
                          tab$page.complete)
tab$page.complete <- gsub("survival","Survival",
                          tab$page.complete)
tab$page.complete <- gsub("RH", "RH<sub>td</sub>",
                         tab$page.complete)

tab


# Survival - Canopy -------------------------------------------------------

survival_cover_mods <- readRDS(file = here("Data/04_Temp", "2024-06-24_survival_cover_models.rds"))

survival_cover_mods

RH_mod_sc <- survival_cover_mods[9, ]
RH_mod_sc
RH_mod_sc$model_1 <- c(RH_mod_sc[[3]][[1]][["d_RH"]])
RH_mod_sc

with(RH_mod_sc, lrtest(model_0[[1]], model_c[[1]]))
with(RH_mod_sc, lrtest(model_0[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(RH_mod_sc, AIC(model_0[[1]], model_c[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(RH_mod_sc, tab_model(model_0[[1]], model_c[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))

tab_model(RH_mod_sc$model_3[[1]])

RH_mod_sc$model_3[[1]]


tab.1 <- tab_model(RH_mod_sc$model_3[[1]],
                 pred.labels = c("Intercept", "RH", "Crown Closure (sqrt)",
                                 "RH x Crown Closure (sqrt)"),
                 string.ci = "CI (95%)",
                 string.p = "P-Value",
                 CSS = list(css.table = '+font-family: Times;')) 

tab.1$page.complete <- gsub("locationF","Location",
                          tab.1$page.complete)
tab.1$page.complete <- gsub("blockF","Block",
                          tab.1$page.complete)
tab.1$page.complete <- gsub("plotF","Plot",
                          tab.1$page.complete)
tab.1$page.complete <- gsub("splitplotF","Split-Plot",
                          tab.1$page.complete)
tab.1$page.complete <- gsub("survival","Survival",
                          tab.1$page.complete)
tab.1$page.complete <- gsub("RH", "RH<sub>td</sub>",
                          tab.1$page.complete)

tab.1

# Height - Harvest --------------------------------------------------------


ln_height_harvest_models <- readRDS(file = here("Data/04_Temp", 
                                                "2024-06-24_ln_height_harvest_models.rds" ))

ln_height_harvest_models


PAS_mod_hh <- ln_height_harvest_models[6, ]
PAS_mod_hh

with(PAS_mod_hh, lrtest(model_0[[1]], model_h[[1]]))
with(PAS_mod_hh, lrtest(model_0[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(PAS_mod_hh, lrtest(model_1[[1]], model_3[[1]]))


with(PAS_mod_hh, AIC(model_0[[1]], model_h[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(PAS_mod_hh, tab_model(model_0[[1]], model_h[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))

tab_model(PAS_mod_hh$model_2[[1]])


tab.2 <- tab_model(PAS_mod_hh$model_2[[1]],
                   pred.labels = c("Intercept", "PAS", 
                                   "Seed Tree", "30% Retention", "60% Retention"),
                   string.ci = "CI (95%)",
                   string.p = "P-Value",
                   CSS = list(css.table = '+font-family: Times;')) 

tab.2$page.complete <- gsub("locationF","Location",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("blockF","Block",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("plotF","Plot",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("splitplotF","Split-Plot",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("log","ln",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("height","Height",
                            tab.2$page.complete)
tab.2$page.complete <- gsub("PAS", "PAS<sub>td</sub>",
                            tab.2$page.complete)

tab.2


# Height - Canopy ---------------------------------------------------------


ln_height_cover_models <- readRDS(file = here("Data/04_Temp", 
                                              "2024-06-24_ln_height_cover_models.rds" ))

ln_height_cover_models

PAS_mod_hc <- ln_height_cover_models[6, ]
PAS_mod_hc

with(PAS_mod_hc, lrtest(model_0[[1]], model_c[[1]]))
with(PAS_mod_hc, lrtest(model_0[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))

with(PAS_mod_hc, AIC(model_0[[1]], model_c[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))
with(PAS_mod_hc, tab_model(model_0[[1]], model_c[[1]], model_1[[1]], model_2[[1]], model_3[[1]]))

tab_model(PAS_mod_hc$model_2[[1]])


tab.3 <- tab_model(PAS_mod_hc$model_2[[1]],
                   pred.labels = c("Intercept", "PAS", 
                                   "Crown Closure (sqrt)"),
                   string.ci = "CI (95%)",
                   string.p = "P-Value",
                   CSS = list(css.table = '+font-family: Times;')) 

tab.3$page.complete <- gsub("locationF","Location",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("blockF","Block",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("plotF","Plot",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("splitplotF","Split-Plot",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("log","ln",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("height","Height",
                            tab.3$page.complete)
tab.3$page.complete <- gsub("PAS", "PAS<sub>td</sub>",
                            tab.3$page.complete)

tab.3
