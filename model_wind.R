# This file contains several functions that serve the purpose of estimating wind


get_hist_wind_data = function(){
  #' Function to get historical temp data
  
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_wind_10m.RData"))
  return(data_icon_eps)
}


wind_emos_tn = function(init_date, mode=1){
  #' Function to make forecasts of temp using EMOS with truncated normal distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' mode: Integer indicating wether [1] forecasts or [2] model_parameters are to be returned
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = 5, nrow = 5)
  if(mode==2){
    pars = matrix(nrow=5,ncol=2)
  }
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    wind_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                           data = wind_data,
                           dist = "gaussian",
                           left = 0,
                           truncated = TRUE,
                           link.scale = "log",
                           type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    wind_benchmark2_loc = predict(wind_benchmark2,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_benchmark2,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtnorm(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    if(mode==2){
      pars[i,1] = wind_benchmark2_loc
      pars[i,2] = wind_benchmark2_sc
    }
    i = i+1
  }
  if(mode==1){
    return(fcst_wind)
  }
  else if(mode==2){
    return(pars)
  }
}


wind_emos_tl = function(init_date, mode=1){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' mode: Integer indicating wether [1] forecasts or [2] model_parameters are to be returned
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  # TODO CHANGE DATE when current file has been downloaded to the corresponding folder
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = 5, nrow = 5)
  if(mode==2){
    pars = matrix(nrow=5,ncol=2)
  }
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    wind_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                           data = wind_data,
                           dist = "logistic",
                           left = 0,
                           truncated = TRUE,
                           link.scale = "log",
                           type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    wind_benchmark2_loc = predict(wind_benchmark2,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_benchmark2,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtlogis(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    if(mode==2){
      pars[i,1] = wind_benchmark2_loc
      pars[i,2] = wind_benchmark2_sc
    }
    i = i+1
  }
  if(mode==1){
    return(fcst_wind)
  }
  else if(mode==2){
    return(pars)
  }
}

