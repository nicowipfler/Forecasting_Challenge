# This Script contains all necessary libraries and R-scripts in order to make and evaluate the forecasts


# Preparations ------------------------------------------------------------


# get librarys
source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
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


date = '2021-11-10'


### DAX


## QUANTILE REGRESSION
rolling_window_dax = 150 # Aus model_enhancement: 150 sollte optimal sein
#fcst_dax = dax_quantreg('2021-11-03', transpose=TRUE, rolling_window=rolling_window_dax)

## GARCH
fcst_dax = dax_ugarch(date, garchorder=c(6,6))

# Evaluation
fcst_dax
plot_forecasts_dax(date, fcst_dax, history_size=rolling_window_dax, model_name='quantile regression (basic)')


### Temperature


## EMOS
#fcst_temp = temp_emos(date)
#fcst_temp

## EMOS with radiation
fcst_temp = temp_emos_multi(date)
fcst_temp

# Evaluation
history_weather = 10
plot_forecasts_weather(date, fcst_temp, history_size=history_weather, model_name='EMOS using normal distribution', 'air_temperature')
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?


### Wind


## EMOS with truncated normal
#fcst_wind = wind_emos_tn(date)

## EMOS with truncated logistic
#fcst_wind = wind_emos_tl(date)
#fcst_wind

## EMOS with truncated logistic and mean sea level pressure
fcst_wind = wind_emos_tl_multi(date)
fcst_wind

# Evaluation
plot_forecasts_weather(date, fcst_wind, history_size=history_weather, model_name='EMOS using truncated normal distribution', 'wind')
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?


### Export
create_csv(date, fcst_dax, fcst_temp, fcst_wind)

