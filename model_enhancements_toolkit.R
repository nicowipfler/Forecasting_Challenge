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
