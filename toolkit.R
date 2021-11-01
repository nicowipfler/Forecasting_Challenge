# Small file containing useful tools for handy code

load_libs = function(libs){
  #' small function that loads all libs in one handy command
  #' libs: list of librarys to be loaded, e.g. c('dplyr','lubridate')
  
  lapply(libs, require, character.only = TRUE)
}