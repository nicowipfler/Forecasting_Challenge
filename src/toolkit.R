# Small file containing useful tools for handy code

# General Toolkit ---------------------------------------------------------

# Function to load necessary librarys
load_libs = function(libs){
  #' small function that loads all libs in one handy command
  #' libs: list of librarys to be loaded, e.g. c('dplyr','lubridate')
  
  lapply(libs, require, character.only = TRUE)
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

# Function for plotting forecasts
plot_forecasts_dax = function(init_date, forecasts, history_size, model_name, ylim=c(-4,4)){
  #' Function to plot forecasts of DAX alongside historic data to visually ensure conclusiveness
  #' init_date: String containing date of initialization of the forecasts, e.g. "2021-10-23"
  #' forecasts: 5x5 Matrix containing DAX forecasts, rows: horizons, columns: quantile levels
  #' history_size: Integer containing the number of days the graph should reach into the past, e.g. 100
  #' model_name: String containing the model name, e.g. "quantile regression"
  #' ylim: vector of two floats containing the y limits for the plot
  
  # get data corresponding to init_date
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = get_dax_data_directly(init_date)
  # select relevant columns
  dat = data.frame(as.Date(dat$Date), dat$ret1)
  colnames(dat) = c('Date', 'ret1')
  # select relevant rows
  dat = subset(dat, Date > as.Date(init_date)-history_size)
  # add q0.5 from forecasts as 'data points'
  Date = c(as.Date(init_date)+1, as.Date(init_date)+2, as.Date(init_date)+3, as.Date(init_date)+4, as.Date(init_date)+5)
  ret1 = unname(forecasts[,3])
  forecasts_df = data.frame(Date, ret1)
  dat[nrow(dat) + 1:5,] = forecasts_df
  # plot historic data and q0.5 forecasts
  plot(dat$Date, dat$ret1, type="b", ylim=ylim, main=paste0("DAX Forecasts by ", model_name), xlab="Date", ylab="Return (after 1 day)")
  # mark forecasted values
  for (i in 1:5){
    points(dat$Date[length(dat$Date)-5+i], forecasts[i,3], pch=20)
  }
  # draw forecasted confidence intervals
  for (i in 1:5){
    segments(dat$Date[length(dat$Date)-5+i], forecasts[i,1], dat$Date[length(dat$Date)-5+i], forecasts[i,5], col="darkgreen")
    segments(dat$Date[length(dat$Date)-5+i], forecasts[i,2], dat$Date[length(dat$Date)-5+i], forecasts[i,4], col="blue")
  }
  # legend
  legend('topleft', legend=c("50%-CI", "95%-CI", 'q0.5'), col=c('blue', 'darkgreen', NA), lty=c(1,1))
  legend('topleft', legend=c("", "", ''), col = 'black', pch=c(NA,NA,20), bty='n')
}

plot_forecasts_weather = function(init_date, forecasts, history_size, model_name, variable, ylim=c(-5,20)){
  #' Function to plot forecasts of weather alongside historic data to visually ensure conclusiveness
  #' init_date: String containing date of initialization of the forecasts, e.g. "2021-10-23"
  #' forecasts: 5x5 Matrix containing weather (temp OR wind) forecasts, rows: horizons, columns: quantile levels
  #' history_size: Integer containing the number of days the graph should reach into the past, e.g. 10
  #' model_name: String containing the model name, e.g. "EMOS with (truncated) normal distribution"
  #' variable: String indicating wether wind or temp are to be checked, must be either 'wind' or 'air_temperature'
  #' ylim: vector of two floats containing the y limits for the plot
  
  # Get recent observations
  dwd_url = selectDWD(
    name = "Berlin-Tempelhof",
    res = "hourly",
    per = "recent",
    var = variable
  )
  if(variable=='wind'){
    # Delete old file before downloading new one
    dwd_file = 'C:/dev/Forecasting_Challenge/DWDdata/hourly_wind_recent_stundenwerte_FF_00433_akt.zip'
    file.remove(dwd_file)
    dataDWD(dwd_url)
    obs_data = readDWD(dwd_file)
    # m/s to km/h
    obs_data$F = obs_data$F * 3.6
    # time format
    obs_data$MESS_DATUM = ymd_hms(obs_data$MESS_DATUM)
    # select relevant columns
    dat = data.frame(obs_data$MESS_DATUM, obs_data$F)
    colnames(dat) = c('Date', 'var')
  }
  else{
    # Delete old file before downloading new one
    dwd_file = 'C:/dev/Forecasting_Challenge/DWDdata/hourly_air_temperature_recent_stundenwerte_TU_00433_akt.zip'
    file.remove(dwd_file)
    dataDWD(dwd_url)
    obs_data = readDWD(dwd_file)
    # time format
    obs_data$MESS_DATUM = ymd_hms(obs_data$MESS_DATUM)
    # select relevant columns
    dat = data.frame(obs_data$MESS_DATUM, obs_data$TT_TU)
    colnames(dat) = c('Date', 'var')
  }
  # select relevant rows
  dat = subset(dat, as.Date(Date) > as.Date(init_date)-history_size)
  dat = subset(dat, as.Date(Date) < as.Date(init_date))
  # add q0.5 from forecasts as 'data points'
  Date = c(as.Date(init_date)+1.5, as.Date(init_date)+2, as.Date(init_date)+2.5, as.Date(init_date)+3, as.Date(init_date)+3.5)
  var = unname(forecasts[,3])
  forecasts_df = data.frame(Date, var)
  dat[nrow(dat) + 1:5,] = forecasts_df
  # plot historic data and q0.5 forecasts
  plot(dat$Date, dat$var, type="b", 
       main=paste0(variable, " Forecasts by ", model_name), 
       xlab="Date", ylab=variable, ylim=ylim)
  # mark forecasted values
  for (i in 1:5){
    points(dat$Date[length(dat$Date)-5+i], forecasts[i,3], pch=20)
  }
  # draw forecasted confidence intervals
  for (i in 1:5){
    segments(dat$Date[length(dat$Date)-5+i], forecasts[i,1], dat$Date[length(dat$Date)-5+i], forecasts[i,5], col="darkgreen")
    segments(dat$Date[length(dat$Date)-5+i], forecasts[i,2], dat$Date[length(dat$Date)-5+i], forecasts[i,4], col="blue")
  }
  # legend
  legend('topleft', legend=c("50%-CI", "95%-CI", 'q0.5'), col=c('blue', 'darkgreen', NA), lty=c(1,1))
  legend('topleft', legend=c("", "", ''), col = 'black', pch=c(NA,NA,20), bty='n')
}

# Function for producing csv file
create_csv = function(init_date, fcst_dax, fcst_temp, fcst_wind){
  #' Function to create the csv-file, that is to be submissed, after the forecasts were made
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' fcst_dax: 5x5 Matrix containing forecasts for DAX, rows: horizons, columns: quantile levels
  #' fcst_temp: 5x5 Matrix containing forecasts for t2m, rows: horizons, columns: quantile levels
  #' fcst_wind: 5x5 Matrix containing forecasts for wind, rows: horizons, columns: quantile levels
  
  # Prep
  headers = c("forecast_date", "target", "horizon", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  forecast = data.frame(matrix(ncol = 8, nrow = 15))
  colnames(forecast) = headers
  # Input forecast dates
  forecast[,1] = init_date
  # Input targets
  forecast[1:5,2] = "DAX"
  forecast[6:10,2] = "temperature"
  forecast[11:15,2] = "wind"
  # Input horizons
  for (i in 1:5){
    horizons = c(1,2,5,6,7)
    forecast[i,3] = paste0(horizons[i], " day")
  }
  for (i in 6:15){
    horizons = rep(c(36,48,60,72,84),2)
    forecast[i,3] = paste0(horizons[i-5], " hour")
  }
  # write forecasts to data frame
  forecast[1:5,4:8] = fcst_dax
  forecast[6:10,4:8] = fcst_temp
  forecast[11:15,4:8] = fcst_wind
  forecast
  # save results to csv file
  out_dir = "C://dev//Forecasting_Challenge//forecasts//"
  # Generate Filename based on Current Date
  #date = gsub("-","",Sys.Date())
  # Save Forecasts
  write.csv(forecast, paste0(out_dir,paste0(gsub("-","",init_date),'_ObiWanKenobi.csv')), row.names = FALSE, quote = FALSE)
}

# Weather Toolkit ---------------------------------------------------------

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

# Functions for feature engineering WEATHER
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

# DAX Toolkit -------------------------------------------------------------

# Functions to get and prepare DAX data
compute_return = function(y, type = "log", h = 1){
  #' Function to compute log returns of DAX
  #' y: observations
  #' type: log or not
  #' h: int, horizon
  
  n <- length(y)
  y2 <- y[-(1:h)] # exclude first h observations
  y1 <- y[-(n:(n-h+1))] # exclude last h observations
  # compute h-step cumulative returns
  if (type == "log"){
    ret <- c(rep(NA, h), 100*(log(y2)-log(y1)))
  } else {
    ret <- c(rep(NA, h), 100*(y2-y1)/y1)
  }
  ret
}

get_dax_data = function(init_date){
  #' Get the DAX data of the corresponding date loaded from the folder data_dir
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = read.table(paste0(data_dir,init_date,"-dax.csv"), sep = ",", header = TRUE,
                   na.strings = "null") %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1),
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5),
           Date = ymd(Date))
  return(dat)
}

get_dax_data_directly = function(init_date){
  #' Get the DAX data of the corresponding date loaded directly from yahoo finance
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  
  dat = data.frame(getSymbols('^GDAXI',src='yahoo', from = as.Date(init_date)-2000, to = as.Date(init_date)+1, auto.assign=FALSE)) %>%
    mutate(ret1 = compute_return(GDAXI.Adjusted, h = 1),
           ret2 = compute_return(GDAXI.Adjusted, h = 2),
           ret3 = compute_return(GDAXI.Adjusted, h = 3),
           ret4 = compute_return(GDAXI.Adjusted, h = 4),
           ret5 = compute_return(GDAXI.Adjusted, h = 5))#,
  #Date = ymd(Date))
  dat = cbind(Date = rownames(dat), dat)
  return(dat)
}

dax_qrf_feature_eng_train = function(init_date, hist=1000, add_futures=TRUE){
  #' Function for feature engineering for DAX QRF (according to paper DAX_QRF_Inputs and ..._2)
  
  predictor_variables = c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200", 
                          "ret1", "ret2", "ret3", "ret4", "ret5")
  data = getSymbols('^GDAXI',src='yahoo', from = as.Date(init_date)-hist, to = as.Date(init_date)+1, auto.assign=FALSE) %>% na.omit
  if(add_futures){
    data = cbind(data, getSymbols('DY',src='yahoo', from = as.Date(init_date)-hist, 
                                  to = as.Date(init_date)+1, auto.assign=FALSE) %>% na.omit) %>% na.omit
    predictor_variables = append(predictor_variables, "DY.Adjusted")
    predictor_variables = append(predictor_variables, "DY.Volume")
  }
  data$RSI = RSI(data$GDAXI.Adjusted)
  data$Stoch_Oscill = stoch(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
  data$MACD = MACD(data$GDAXI.Adjusted)
  data$ROC = ROC(data$GDAXI.Adjusted)
  data$WPR = WPR(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
  data$CCI = CCI(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
  data$ADX = ADX(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
  data$OBV = OBV(data$GDAXI.Adjusted, data$GDAXI.Volume)
  data$MA200 = SMA(data$GDAXI.Adjusted, n=200)
  data$ret1 = compute_return(matrix(data$GDAXI.Adjusted), h = 1)
  data$ret2 = compute_return(matrix(data$GDAXI.Adjusted), h = 2)
  data$ret3 = compute_return(matrix(data$GDAXI.Adjusted), h = 3)
  data$ret4 = compute_return(matrix(data$GDAXI.Adjusted), h = 4)
  data$ret5 = compute_return(matrix(data$GDAXI.Adjusted), h = 5)
  data = data[,predictor_variables] %>% na.omit
  return(data)
}
