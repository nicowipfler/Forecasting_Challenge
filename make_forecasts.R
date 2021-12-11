# This Script contains all necessary libraries and R-scripts in order to make and evaluate the forecasts


# Preparations ------------------------------------------------------------


# get librarys
source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2',
                   'rugarch','quantmod','quantregForest','moments'))
# load functions for forecasting, forecast evaluation and forecast export
source('model_dax.R')
source('model_wind.R')
source('model_temp.R')
source('visual_checks.R')
source('create_csv.R')
#TODO Temperature: Get from git repo berlin
#TODO Wind: Get from git repo berlin


# Forecasts ---------------------------------------------------------------


date = '2021-12-08'


### DAX +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## QUANTILE REGRESSION
fcst_dax_quantreg_model = 'Base Quantile Regression with rolling window'
fcst_dax_quantreg = dax_quantreg(date, transpose=TRUE, 
                                 rolling_window=150) # UPDATE: 1100 besser, 150 war nur lokales Minimum
fcst_dax_quantreg

## One GARCH model
fcst_dax_garch_model = 'UGARCH model with order 6,6'
fcst_dax_garch = dax_ugarch(date, garchorder=c(6,6))
fcst_dax_garch

## MULTIPLE GARCH MODELS
# These three history sizes should be kind of optimal, but it can happen that the model does not converge, then try surrounding sizes
# Maybe combine further with quantreg? Probably not?
fcst_dax_garch_mixture_model = 'Multiple UGARCH models with optimal history_sizes'
fcst_dax_garch_mixture = dax_ugarch_combined(date, garchorder=c(6,6), 
                                             history_sizes=c(243,800,1030))
fcst_dax_garch_mixture

## COMBINATION
fcst_dax_quantgarch_model = 'GARCH + Quantile Regression'
fcst_dax_quantgarch = dax_quantgarch(date, history_size=1400)
fcst_dax_quantgarch

# WHICH ONE SHOULD BE USED? +++++++++++++++++++++++++++++++++++++++++++++++++
dax_model_name = fcst_dax_quantgarch_model
fcst_dax = fcst_dax_quantgarch

# Visual Check
plot_forecasts_dax(date, fcst_dax, history_size=200, ylim = c(-8, 8),
                   model_name=dax_model_name)


### Temperature +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## EMOS
fcst_temp_emos_base_model = 'Base EMOS model'
fcst_temp_emos_base = temp_emos(date)
fcst_temp_emos_base

## EMOS with radiation
fcst_temp_multi_base_model = 'EMOS model with direct_rad'
fcst_temp_multi = temp_emos_multi(date)
fcst_temp_multi

## EMOS with radiation and boosting -> PERFORMS WORSE THAN JUST EMOS multi
fcst_temp_multi_boosting_model = 'EMOS model with direct_rad via BOOSTING'
fcst_temp_multi_boosting = temp_emos_multi_boosting(date)
fcst_temp_multi_boosting

## EMOS multi + boosting mixture
fcst_temp_multi_mixture_model = 'EMOS model with direct_rad MIXTURE (+boosting)'
fcst_temp_multi_mixture = temp_emos_multi_boosting_mixture(date, weights=c(0.8,0.2)) 
# Optimal weights have been found out in by testing
fcst_temp_multi_mixture

## Quantile Regression Forest
fcst_temp_qrf_model = 'Quantile Regression Forest'
fcst_temp_qrf = temp_qrf(date)
fcst_temp_qrf

# WHICH ONE SHOULD BE USED? +++++++++++++++++++++++++++++++++++++++++++++++++
temp_model_name = fcst_temp_qrf_model
fcst_temp = fcst_temp_qrf

# Visual Check
#par(mfrow=c(2,1))
plot_forecasts_weather(date, fcst_temp, history_size=14, ylim = c(-5,20),
                       model_name=temp_model_name, 'air_temperature')


### Wind ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## EMOS with truncated normal
fcst_wind_tn_model = 'Base EMOS model using trunc norm'
fcst_wind_tn = wind_emos_tn(date)
fcst_wind_tn

## EMOS with truncated logistic
fcst_wind_tl_model = 'EMOS model using trunc logistic'
fcst_wind_tl = wind_emos_tl(date)
fcst_wind_tl

## EMOS with truncated logistic and MEAN SEA LEVEL PRESSURE
fcst_wind_tl_multi_model = 'EMOS model using trunc logistic with MSLP'
fcst_wind_tl_multi = wind_emos_tl_multi(date)
fcst_wind_tl_multi

## EMOS with truncated logistic and mean sea level pressure and BOOSTING
fcst_wind_tl_multi_boost_model = 'EMOS model using trunc logistic with MSLP via BOOSTING'
fcst_wind_tl_multi_boost = wind_emos_tl_multi_boosting(date)
fcst_wind_tl_multi_boost

## Quantile Regression Forest
fcst_wind_qrf_model = 'Quantile Regression Forest'
fcst_wind_qrf = wind_qrf(date, addrad=TRUE)
fcst_wind_qrf

# WHICH ONE SHOULD BE USED? +++++++++++++++++++++++++++++++++++++++++++++++++
wind_model_name = fcst_wind_tl_multi_boost_model
fcst_wind = fcst_wind_tl_multi_boost

# Visual check
plot_forecasts_weather(date, fcst_wind, history_size=14, ylim=c(0,40),
                       model_name=wind_model_name, 'wind')


# Export ------------------------------------------------------------------


create_csv(date, fcst_dax, fcst_temp, fcst_wind)
