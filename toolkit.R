# Small file containing useful tools for handy code

# Function to load necessary librarys
load_libs = function(libs){
  #' small function that loads all libs in one handy command
  #' libs: list of librarys to be loaded, e.g. c('dplyr','lubridate')
  
  lapply(libs, require, character.only = TRUE)
}

# Functions to get historical and current ensemble predictions
get_hist_temp_data = function(){
  #' Function to get historical temp data
  
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_t_2m.RData"))
  return(data_icon_eps)
}

get_current_temp_data = function(init_date){
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,
                                      "00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  return(new_fcst)
}

get_hist_wind_data = function(){
  #' Function to get historical wind data
  
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_wind_10m.RData"))
  return(data_icon_eps)
}

get_current_wind_data = function(init_date){
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,
                                      "00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  return(new_fcst)
}

get_hist_data_varname = function(varname){
  #' Function to get historical data of var varname
  #' can be direct_rad, clct, mslp
  
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, paste0("icon_eps_",varname,".RData")))
  return(data_icon_eps)
}

get_current_data_varname = function(varname, init_date){
  # Get current ensemble forecastsfor var varname in direct_rad, mslp, clct
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  current_data = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,
                                          "00_",varname,"_Berlin.txt"), sep = "|", header = TRUE)
  return(current_data)
}

# Functions for combination of forecasts
combine_forecasts = function(fc_in1, fc_in2, weights=c(0.5,0.5)){
  #' Function that combines two forecasts by weighted averaging. Standard: arithmetic mean
  #' fc_in1 and fc_in2: matrices containing the forecasts
  
  fc_out = weights[1] * fc_in1 + weights[2] * fc_in2
  return(fc_out)
}

combine_many_forecasts = function(fc_array, weights=0){
  #' Function that combines any amount of forecasts by weighted averaging. Standard: arithmetic mean
  #' fc_array: array of matrices containing the forecasts
  #' weights: list of weights belonging to the forecasts. 0 indicates arithmetic mean
  
  n = dim(fc_array)[3]
  if(weights == 0){
    weights = rep(1/n,n)
  }
  fc_out = matrix(nrow=dim(fc_array)[1], ncol=dim(fc_array)[2], 0)
  for(i in 1:n){
    fc_out = fc_out + fc_array[,,i] * weights[i]
  }
  fc_out
  return(fc_out)
}

# Functions for feature engineering
qrf_feature_eng_train = function(df, lt, addmslp=FALSE, addclct=FALSE, addrad=FALSE){
  #' Function that makes feature engineering for weather FALSEile regression forests training, returns ensemble statistics used for qrf
  #' It gets summary statistics for target variable and adds simple summary statistics for additional regressors, if wanted
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  
  # First do summary statistics for target variable
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_sd = sqrt(df_lt$ens_var)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[7:46], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[7:46], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[7:46], 1, kurtosis, na.rm=T)
  df_lt$mon = month(df_lt$obs_tm)
  df_working = select(df_lt, obs_tm, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon, obs)
  # now add summary statistics for additional variables that should be included
  if (addmslp){
    df_varname = get_hist_data_varname('mslp')
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, obs_tm, 'dez01_mslp'=dez01, 'ens_mean_mslp'=ens_mean, 'dez09_mslp'=dez09)
    df_working = merge(df_working, df_varname_final, by='obs_tm')
  }
  if (addclct){
    df_varname = get_hist_data_varname('clct')
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, obs_tm, 'dez01_clct'=dez01, 'ens_mean_clct'=ens_mean, 'dez09_clct'=dez09)
    df_working = merge(df_working, df_varname_final, by='obs_tm')
  }
  if (addrad){
    df_varname = get_hist_data_varname('direct_rad')
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, obs_tm, 'dez01_direct_rad'=dez01, 'ens_mean_direct_rad'=ens_mean, 
                              'dez09_direct_rad'=dez09)
    df_working = merge(df_working, df_varname_final, by='obs_tm')
  }
  # Omit obs_tm, just needed for matching the rows
  df_pred = df_working[,-1]
  return(df_pred)
}

qrf_feature_eng_predict = function(df, lt, init_date, addmslp=FALSE, addclct=FALSE, addrad=FALSE){
  #' Function that makes feature engineering for weather quantile regression forests predictions.
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  #' init_date: date on which the prediction is to be made
  
  # First do summary statistics for target variable
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_sd = apply(df_lt[3:42], 1, sd, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[3:42], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[3:42], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[3:42], 1, kurtosis, na.rm=T)
  df_lt$mon = month(as.Date(init_date)+floor(lt/24))
  df_working = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon)
  if (addmslp){
    df_varname = get_current_data_varname('mslp', init_date)
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$ens_mean = apply(df_varname_lt[3:42], 1, mean, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, 'dez01_mslp'=dez01, 'ens_mean_mslp'=ens_mean, 'dez09_mslp'=dez09)
    df_working = merge(df_working, df_varname_final)
  }
  if (addclct){
    df_varname = get_current_data_varname('clct', init_date)
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$ens_mean = apply(df_varname_lt[3:42], 1, mean, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, 'dez01_clct'=dez01, 'ens_mean_clct'=ens_mean, 'dez09_clct'=dez09)
    df_working = merge(df_working, df_varname_final)
  }
  if (addrad){
    df_varname = get_current_data_varname('direct_rad', init_date)
    df_varname_lt = subset(df_varname, fcst_hour == lt)
    df_varname_lt$dez01 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
    df_varname_lt$ens_mean = apply(df_varname_lt[3:42], 1, mean, na.rm=T, probs= 0.1)
    df_varname_lt$dez09 = apply(df_varname_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
    df_varname_final = select(df_varname_lt, 'dez01_direct_rad'=dez01, 'ens_mean_direct_rad'=ens_mean, 
                              'dez09_direct_rad'=dez09)
    df_working = merge(df_working, df_varname_final)
  }
  return(df_working)
}
