# Small file containing useful tools for handy code

load_libs = function(libs){
  #' small function that loads all libs in one handy command
  #' libs: list of librarys to be loaded, e.g. c('dplyr','lubridate')
  
  lapply(libs, require, character.only = TRUE)
}


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


qrf_feature_eng_train = function(df, lt){
  #' Function that makes feature engineering for weather quantile regression forests training, returns ensemble statistics used for qrf
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_sd = sqrt(df_lt$ens_var)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[7:46], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[7:46], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[7:46], 1, kurtosis, na.rm=T)
  df_lt$mon = month(df_lt$obs_tm)
  df_pred = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon)
  return(df_pred)
}


qrf_feature_eng_predict = function(df, lt, init_date){
  #' Function that makes feature engineering for weather quantile regression forests predictions
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  #' init_date: date on which the prediction is to be made
  
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_sd = apply(df_lt[3:42], 1, sd, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[3:42], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[3:42], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[3:42], 1, kurtosis, na.rm=T)
  df_lt$mon = month(init_date)
  df_pred = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon)
  return(df_pred)
}
