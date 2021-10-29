wind_emos = function(init_date){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  
  # Get historical data
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  load(paste0(data_dir, "icon_eps_wind_10m.RData"))
  wind_data_raw = data_icon_eps
  rm(data_icon_eps)
  # Get current ensemble forecasts
  # TODO CHANGE DATE when current file has been downloaded to the corresponding folder
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = 5, nrow = 5)
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
    i = i+1
  }
  return(fcst_wind)
}



#TODO Aufräumen / Löschen, wenn Funktion für visual checks wind und temp steht
# # Forecasts ready to be checked visually with script visual_checks_weather.R
# fcst_wind_df = data.frame(matrix(nrow = 25, ncol = 2))
# colnames(fcst_wind_df) = c("MESS_DATUM", "F")
# class(fcst_wind_df$MESS_DATUM) = "character"
# for (i in 0:4){
#   # time
#   date = Sys.Date()
#   print(date)
#   leads = c(36,48,60,72,84)
#   lead = leads[i+1]
#   time = toString(date + hours(lead))
#   if (i == 0 ||  i == 2 || i == 4){
#     fcst_wind_df[(1+i*5):(5+i*5),1] = time
#   }
#   else{
#     fcst_wind_df[(1+i*5):(5+i*5),1] = paste(time,"00:00:00")
#   }
#   
#   # forecasts
#   fcst_wind_df[(1+i*5):(5+i*5),2] = t(fcst_wind)[,i+1]
# }
# fcst_wind_df
