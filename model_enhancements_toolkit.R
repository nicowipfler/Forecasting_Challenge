# Functions for approximation of crps
quantile_score = function(quantile, forecast, realization) {
  #' Function to compute quantile score of a given forecast on quantile-level quantile and realization
  #' quantile: float containing quantile level, e.g. 0.95
  #' forecast: float containing forecasted value on quantile level, e.g. 0.05
  #' realization: float containing the realized value, e.g. 0.04

  if(forecast > realization){
    return(2*(1-quantile)*(forecast-realization))
  }
  else{
    return(2*quantile*(realization-forecast))
  }
}

approx_crps = function(quantile_levels, fc, obs){
  #' Function that returns an approximation of the CRPS of given forecasts & obs at given quantile level (should be equidistant grid!)
  #' quantile_levels: vector containing quantile levels as numeric values
  #' fc: matrix containing forecasts. rows: horizons (must be 5!), columns: quantile_levels
  #' obs: vector containing observations (ordered) as numeric values
  
  scores = matrix(nrow=5, ncol=length(quantile_levels))
  for (n_horizon in 1:5){
    for (n_quantile in 1:length(quantile_levels)){
      scores[n_horizon,n_quantile] = quantile_score(quantile_levels[n_quantile],fc[n_horizon,n_quantile],obs[n_horizon])
    }
  }
  return(mean(scores))
}

# Functions for model evaluation based on CRPS approx. via mean of quantile scores
evaluate_model_weather = function(model_func,variable,quantile_levels=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                                  init_dates,show_vals=FALSE,...){
  #' Function, that evaluates model on historic dataset based on mean of quantile scores as approx. of CRPS
  #' model_func: Function that puts out the models forecasts, e.g. emos_temp
  #' variable: String indicating wether wind or temp are to be checked, must be either 'wind' or 'air_temperature'
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Preparations
  scores_dates = matrix(nrow=length(init_dates), ncol=1)
  # Iterate over init_dates, for which we have all the data
  for (i in 1:length(init_dates)){
    # Prep
    init_date = init_dates[i]
    # Get observations at each init_date that we have all information for (as of now, just two dates)
    observations = get_obs(variable,init_date)
    print(init_date)
    # Get Forecasts of the given model for given init_date
    forecasts = model_func(init_date=init_date, quantile_levels=quantile_levels, ...)
    # Compare to obs: Compute Quantile Scores / Approx- CRPS
    scores_dates[i] = approx_crps(quantile_levels, forecasts, observations)
    if(show_vals){
      for(h_num in 1:dim(forecasts)[1]){
        print(paste0('Horizon number ',h_num,':'))
        print(paste0('Observation: ',observations[h_num]))
        for(q_num in 1:dim(forecasts)[1]){
          print(paste0('Predicted quantile number ', q_num, ':', forecasts[h_num,q_num]))
        }
      }
    }
  }
  final_score = mean(scores_dates)
  return(final_score)
}

evaluate_model_dax = function(model_func,quantile_levels=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), quantreg=FALSE, 
                              init_dates, ...){
  #' Function, that evaluates model on historic dataset based on mean of quantile scores as approx. of CRPS
  #' model_func: Function that puts out the models forecasts, e.g. emos_temp
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' quantreg: Boolean indicating wether quantreg is used, bc values have to be transposed then
  #' init_dates: list of strings containing the dates on which the model should be evaluated
  
  # Preparations
  scores_dates = matrix(NA, nrow=length(init_dates), ncol=1)
  # Iterate over init_dates, for which we have all the data
  for (i in 1:length(init_dates)){
    # Prep
    init_date = init_dates[i]
    print(init_date)
    dax_data = get_dax_data_directly(as.Date(init_date)+7)
    observations = c(dax_data[dim(dax_data)[1]-4,'ret1'], dax_data[dim(dax_data)[1]-3,'ret2'], dax_data[dim(dax_data)[1]-2,'ret3'],
            dax_data[dim(dax_data)[1]-1,'ret4'], dax_data[dim(dax_data)[1],'ret5'])
    # Get Forecasts of the given model for given init_date
    tryCatch({
      forecasts = model_func(init_date=init_date, quantile_levels=quantile_levels, ...)
      if(quantreg){
        forecasts = t(forecasts)
      }
      # Compare to obs: Compute Quantile Scores / Approx- CRPS
      scores_dates[i] = approx_crps(quantile_levels, forecasts, observations)
    },
    error = function(cond){
      scores_dates[i] = NA
    })
  }
  final_score = mean(scores_dates, na.rm=TRUE)
  return(final_score)
}

# Other toolkit functions
generate_times = function(date){
  #' Function to return dates for which forecasts initiated on date have to be done
  #' date: string containing init date, e.g. '2021-10-27'
  
  dates = matrix(ncol=1, nrow=5)
  for (i in 1:5){
    if(i==1){
      dates[1] = paste0(as.Date(date)+1,' 12:00')
    }
    else if(i==2){
      dates[2] = paste0(as.Date(date)+2, ' 00:00')
    }
    else if(i==3){
      dates[3] = paste0(as.Date(date)+2,' 12:00')
    }
    else if(i==4){
      dates[4] = paste0(as.Date(date)+3,' 00:00')
    }
    else if(i==5){
      dates[5] = paste0(as.Date(date)+3,' 12:00')
    }
  }
  return(dates)
}

get_obs = function(variable,init_date){
  #' variable: String indicating wether wind or temp are to be checked, must be either 'wind' or 'air_temperature'
  #' init_date: String containing the starting date of observations (always get the observations that match the forecast horizon on this init_date!)
  
  dwd_url = selectDWD(
    name = "Berlin-Tempelhof",
    res = "hourly",
    per = "recent",
    var = variable
  )
  dataDWD(dwd_url)
  # NOTE: OLD DATA MIGHT HAVE TO BE DELETED MANUALLY
  if (variable=='wind'){
    obs_data = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_wind_recent_stundenwerte_FF_00433_akt.zip')
  } else if (variable=='air_temperature'){
    obs_data = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_air_temperature_recent_stundenwerte_TU_00433_akt.zip')
  }
  dates = generate_times(init_date)
  if (variable=='wind'){
    observations = subset(obs_data,
                          MESS_DATUM == dates[1]|
                            MESS_DATUM == dates[2]|
                            MESS_DATUM == dates[3]|
                            MESS_DATUM == dates[4]|
                            MESS_DATUM == dates[5])$F
    observations = observations * 3.6
  } else if (variable=='air_temperature'){
    observations = subset(obs_data,
                          MESS_DATUM == dates[1]|
                            MESS_DATUM == dates[2]|
                            MESS_DATUM == dates[3]|
                            MESS_DATUM == dates[4]|
                            MESS_DATUM == dates[5])$TT_TU
  }
  return(observations)
}
