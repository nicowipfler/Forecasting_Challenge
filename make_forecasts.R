# Preparations ------------------------------------------------------------


# get librarys
library(dplyr)
library(lubridate)
library(tidyr)
library(quantreg)
library(scoringRules)
library(crch)
# set quantile levels
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)
# load functions for forecasting, forecast evaluation and forecast export
source('model_dax.R')
source('model_wind.R')
source('model_temp.R')
source('visual_checks.R')
source('create_csv.R')
#TODO The current data must have been saved in the corresponding directories.
#TODO DAX: Yahoo Finance ^GDAXI, historical data with maximum timeframe under the name "yyyy-mm-dd-dax.csv".
#TODO Temperature: Get from git repo
#TODO Wind: Get from git repo


# Forecasts ---------------------------------------------------------------


rolling_window_dax = 190
fcst_dax = dax_quantreg('2021-10-27', transpose=TRUE, rolling_window=rolling_window_dax)
fcst_dax
plot_forecasts_dax('2021-10-27', fcst_dax, history_size=rolling_window_dax, model_name='quantile regression (basic)')

fcst_temp = temp_emos('2021-10-27')
fcst_temp
#TODO Funktion um diese Forecasts zu plotten

fcst_wind = wind_emos('2021-10-27')
fcst_wind
#TODO Funktion um diese Forecasts zu plotten

create_csv("2021-10-27", fcst_dax, fcst_temp, fcst_wind)

