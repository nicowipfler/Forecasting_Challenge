# This file contains several functions that serve the purpose of estimating temperature


get_hist_temp_data = function(){
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_t_2m.RData"))
  return(data_icon_eps)
}


temp_emos = function(init_date){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  
  # prepare historical data
  t2m_data_raw = get_hist_temp_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_temp = matrix(ncol = 5, nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    t2m_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                          data = t2m_data,
                          dist = "gaussian",
                          link.scale = "log",
                          type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    t2m_benchmark2_loc = predict(t2m_benchmark2,
                                 pred_df,
                                 type = "location")
    t2m_benchmark2_sc = predict(t2m_benchmark2,
                                pred_df,
                                type = "scale")
    t2m_benchmark2_pred = qnorm(quantile_levels, t2m_benchmark2_loc, t2m_benchmark2_sc)
    # Write to Output Data
    fcst_temp[i,] = t2m_benchmark2_pred
    i = i+1
  }
  # Forecasts ready to write to csv
  return(fcst_temp)
}
