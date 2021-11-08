quantile_score = function (quantile, forecast, realization) {
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
