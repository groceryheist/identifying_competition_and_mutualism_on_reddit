library(arrow)
library(ggplot2)
library(data.table)
library(vars)

source("RemembR/R/RemembeR.R")
source("model_interpretation.R")

remember.model(draws.file="design_stanmod",
               fit.file="data/var_design_data_fit.feather",
               forecast.file="data/var_design_data_forecast.feather",
               prefix='design')
