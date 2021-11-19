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
rolling_window_dax = 150 # Aus model_enhancement: 150 sollte optimal sein UPDATE: 1100 besser, 150 war nur lokales Minimum
#fcst_dax = dax_quantreg(date, transpose=TRUE, rolling_window=rolling_window_dax)

## One GARCH model
#fcst_dax = dax_ugarch(date, garchorder=c(6,6))

## MULTIPLE GARCH MODELS
# These three history sizes should be kind of optimal, but it can happen that the model does not converge, then try surrounding sizes
fcst_dax = dax_ugarch_combined('2021-11-03', garchorder=c(6,6), history_sizes=c(243,800,1030))

## COMBINATION
#fcst_dax = combine_forecasts(fcst_ugarch, fcst_quantreg)

# Evaluation
fcst_dax
plot_forecasts_dax(date, fcst_dax, history_size=200, model_name='UGARCH(6,6)')


### Temperature


## EMOS
#fcst_temp = temp_emos(date)
#fcst_temp

## EMOS with radiation
fcst_temp = temp_emos_multi(date)
fcst_temp

# Evaluation
history_weather = 10
plot_forecasts_weather(date, fcst_temp, history_size=history_weather, 
                       model_name='EMOS using normal distribution with 2nd regressor', 'air_temperature')
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
plot_forecasts_weather(date, fcst_wind, history_size=history_weather, 
                       model_name='EMOS using truncated normal distribution with 2nd regressor', 'wind')
#TODO Alte DWD Daten müssen gelöscht werden, damit neue heruntergeladen werden können -> Automatisieren?


### Export
create_csv(date, fcst_dax, fcst_temp, fcst_wind)

