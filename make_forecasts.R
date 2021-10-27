# Preparations ------------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyr)
library(quantreg)
library(scoringRules)
library(crch)
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)


# Forecasts ---------------------------------------------------------------


fcst_dax = dax_quantreg("2021-10-23", TRUE)
fcst_dax
plot_forecasts_dax('2021-10-23', fcst_dax, 100, 'quantile regression (basic)')

fcst_temp = temp_emos('2021-10-23')
fcst_temp
#TODO Funktion um diese Forecasts zu plotten

fcst_wind = wind_emos('2021-10-23')
fcst_wind
#TODO Funktion um diese Forecasts zu plotten

create_csv("2021-10-23", fcst_dax, fcst_temp, fcst_wind)
