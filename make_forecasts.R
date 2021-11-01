# This Script contains all necessary libraries and R-scripts in order to make and evaluate the forecasts


# Preparations ------------------------------------------------------------


# get librarys
source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2'))
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


# DAX
rolling_window_dax = 150 # Aus model_enhancement: 150 sollte optimal sein
fcst_dax = dax_quantreg('2021-10-27', transpose=TRUE, rolling_window=rolling_window_dax)
fcst_dax
plot_forecasts_dax('2021-10-27', fcst_dax, history_size=rolling_window_dax, model_name='quantile regression (basic)')

# Temperature
history_weather = 10
fcst_temp = temp_emos('2021-10-27')
fcst_temp
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?
plot_forecasts_weather('2021-10-27', fcst_temp, history_size=history_weather, model_name='EMOS using normal distribution', 'air_temperature')

# Wind
#fcst_wind = wind_emos_tn('2021-10-27')
#fcst_wind
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?
#plot_forecasts_weather('2021-10-27', fcst_wind, history_size=history_weather, model_name='EMOS using truncated normal distribution', 'wind')

# Wind using Truncated Logistic
fcst_wind = wind_emos_tl('2021-10-27')
fcst_wind
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?
plot_forecasts_weather('2021-10-27', fcst_wind, history_size=history_weather, model_name='EMOS using truncated normal distribution', 'wind')

# Export
create_csv("2021-10-27", fcst_dax, fcst_temp, fcst_wind)

