# FUNCTION create csv -----------------------------------------------------


create_csv = function(init_date, fcst_dax, fcst_temp, fcst_wind){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' fcst_dax: 5x5 Matrix containing forecasts for DAX, rows: horizons, columns: quantile levels
  #' fcst_temp: 5x5 Matrix containing forecasts for t2m, rows: horizons, columns: quantile levels
  #' fcst_wind: 5x5 Matrix containing forecasts for wind, rows: horizons, columns: quantile levels
  
  # Prep
  headers = c("forecast_date", "target", "horizon", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  forecast = data.frame(matrix(ncol = 8, nrow = 15))
  colnames(forecast) = headers
  # Input forecast dates
  forecast[,1] = init_date
  # Input targets
  forecast[1:5,2] = "DAX"
  forecast[6:10,2] = "temperature"
  forecast[11:15,2] = "wind"
  # Input horizons
  for (i in 1:5){
    horizons = c(1,2,5,6,7)
    forecast[i,3] = paste0(horizons[i], " day")
  }
  for (i in 6:15){
    horizons = rep(c(36,48,60,72,84),2)
    forecast[i,3] = paste0(horizons[i-5], " hour")
  }
  # write forecasts to data frame
  forecast[1:5,4:8] = fcst_dax
  forecast[6:10,4:8] = fcst_temp
  forecast[11:15,4:8] = fcst_wind
  forecast
  # save results to csv file
  out_dir = "C://dev//Forecasting_Challenge//forecasts//"
  # Generate Filename based on Current Date
  #date = gsub("-","",Sys.Date())
  # Save Forecasts
  write.csv(forecast, paste0(out_dir,paste0(gsub("-","",init_date),'_ObiWanKenobi.csv')), row.names = FALSE, quote = FALSE)
}