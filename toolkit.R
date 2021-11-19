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
