# Small file containing useful tools for handy code

load_libs = function(libs){
  #' small function that loads all libs in one handy command
  #' libs: list of librarys to be loaded, e.g. c('dplyr','lubridate')
  
  lapply(libs, require, character.only = TRUE)
}


combine_forecasts = function(fc_in1, fc_in2, weights=c(0.5,0.5)){
  #' Function that combines forecasts by weighted averaging. Standard: arithmetic mean
  #' fc_in1 and fc_in2: matrices containing the forecasts
  
  fc_out = weights[1] * fc_in1 + weights[2] * fc_in2
  return(fc_out)
}
