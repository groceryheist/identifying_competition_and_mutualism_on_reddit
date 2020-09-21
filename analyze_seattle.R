library(arrow)
library(ggplot2)
library(data.table)
library(vars)

source("RemembR/R/RemembeR.R")
source("model_interpretation.R")

remember.model(draws.file="var_stan_p1_stanmod",
               fit.file="data/var_stan_data_fit.feather",
               forecast.file="data/var_stan_data_forecast.feather",
               prefix='seattle')
