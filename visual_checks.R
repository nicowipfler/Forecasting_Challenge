# This file contains functions that serve to plot already made forecasts alongside the observed data


plot_forecasts_dax = function(init_date, forecasts, history_size, model_name){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of the forecasts, e.g. "2021-10-23"
  #' forecasts: 5x5 Matrix containing DAX forecasts, rows: horizons, columns: quantile levels
  #' history_size: Integer containing the number of days the graph should reach into the past, e.g. 100
  #' model_name: String containing the model name, e.g. "quantile regression"
  
  # get data corresponding to init_date
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = read.table(paste0(data_dir,init_date,"-dax.csv"), sep = ",", header = TRUE,
                   na.strings = "null") %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1),
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5),
           Date = ymd(Date))
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
  plot(dat$Date, dat$ret1, type="b", ylim=c(-4,4), main=paste0("DAX Forecasts starting from ", init_date," using ", model_name), xlab="Date", ylab="Return (after 1 day)")
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

plot_forecasts_weather = function(init_date, forecasts, history_size, model_name, variable){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of the forecasts, e.g. "2021-10-23"
  #' forecasts: 5x5 Matrix containing weather (temp OR wind) forecasts, rows: horizons, columns: quantile levels
  #' history_size: Integer containing the number of days the graph should reach into the past, e.g. 10
  #' model_name: String containing the model name, e.g. "EMOS with (truncated) normal distribution"
  #' variable: String indicating wether wind or temp are to be checked, must be either 'wind' or 'air_temperature'
  
  # Get recent observations
  dwd_url = selectDWD(
    name = "Berlin-Tempelhof",
    res = "hourly",
    per = "recent",
    var = variable
  )
  dataDWD(dwd_url)
  if(variable=='wind'){
    obs_data = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_wind_recent_stundenwerte_FF_00433_akt.zip')
    # m/s to km/h
    obs_data$F = obs_data$F * 3.6
    # time format
    obs_data$MESS_DATUM = ymd_hms(obs_data$MESS_DATUM)
    # select relevant columns
    dat = data.frame(obs_data$MESS_DATUM, obs_data$F)
    colnames(dat) = c('Date', 'var')
  }
  else{
    obs_data = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_air_temperature_recent_stundenwerte_TU_00433_akt.zip')
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
  plot(dat$Date, dat$var, type="b", main=paste0(variable, " Forecasts starting from ", init_date," using ", model_name), xlab="Date", ylab=variable)
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
