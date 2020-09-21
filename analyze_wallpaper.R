library(arrow)
library(ggplot2)
library(data.table)
library(vars)

source("RemembR/R/RemembeR.R")
source("model_interpretation.R")

remember.model("wallpaper/var_p1_all_stanmod",
               "data/var_wallpaper_data_fit.feather",
               "data/var_wallpaper_data_forecast.feather",
               prefix='wallpaper')


