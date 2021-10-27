# Function to plot --------------------------------------------------------


plot_forecasts_dax = function(init_date, forecasts, history_size, model_name){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing date of initialization of the forecasts, e.g. "2021-10-23"
  #' forecasts: 5x5 Matrix containing DAX forecasts, rows: horizons, columns: quantile levels
  #' history_size: Integer containing the number of days the graph should reach into the past, e.g. 100
  #' model_name: String containing the model name, e.g. "quantile regression"
  
  # get data corresponding to init_date
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = read.table(paste0(data_dir,init_date,"-dax.csv"), sep = ",", header = TRUE,
                   na.strings = "null") %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1),
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5),
           Date = ymd(Date))
  # select relevant columns
  dat = data.frame(as.Date(dat$Date), dat$ret1)
  colnames(dat) = c('Date', 'ret1')
  # select relevant rows
  dat = subset(dat, Date > as.Date(init_date)-history_size)
  # add q0.5 from forecasts as 'data points'
  Date = c(as.Date(init_date)+1, as.Date(init_date)+2, as.Date(init_date)+3, as.Date(init_date)+4, as.Date(init_date)+5)
  ret1 = unname(forecasts[,3])
  forecasts_df = data.frame(Date, ret1)
  dat[nrow(dat) + 1:5,] = forecasts_df
  # plot historic data and q0.5 forecasts
  plot(dat$Date, dat$ret1, type="b", ylim=c(-4,4), main=paste0("DAX Forecasts starting from ", init_date," using ", model_name), xlab="Date", ylab="Return (after 1 day)")
  # mark forecasted values
  for (i in 1:5){
    points(as.Date(init_date)+i, forecasts[i,3], pch=20)
  }
  # draw forecasted confidence intervals
  for (i in 1:5){
    segments(as.Date(init_date)+i, forecasts[i,1], as.Date(init_date)+i, forecasts[i,5], col="darkgreen")
    segments(as.Date(init_date)+i, forecasts[i,2], as.Date(init_date)+i, forecasts[i,4], col="blue")
  }
  # legend
  legend('topleft', legend=c("50%-CI", "95%-CI", 'q0.5'), col=c('blue', 'darkgreen', NA), lty=c(1,1))
  legend('topleft', legend=c("", "", ''), col = 'black', pch=c(NA,NA,20), bty='n')
}
