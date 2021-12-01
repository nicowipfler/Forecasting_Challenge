# This file contains several functions that serve the purpose of estimating DAX (log) returns


compute_return = function(y, type = "log", h = 1){
  #' Function to compute log returns of DAX
  #' y: observations
  #' type: log or not
  #' h: int, horizon
  
  n <- length(y)
  y2 <- y[-(1:h)] # exclude first h observations
  y1 <- y[-(n:(n-h+1))] # exclude last h observations
  # compute h-step cumulative returns
  if (type == "log"){
    ret <- c(rep(NA, h), 100*(log(y2)-log(y1)))
  } else {
    ret <- c(rep(NA, h), 100*(y2-y1)/y1)
  }
  ret
}


get_dax_data = function(init_date){
  #' Get the DAX data of the corresponding date loaded from the folder data_dir
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = read.table(paste0(data_dir,init_date,"-dax.csv"), sep = ",", header = TRUE,
                   na.strings = "null") %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1),
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5),
           Date = ymd(Date))
  return(dat)
}


get_dax_data_directly = function(init_date){
  #' Get the DAX data of the corresponding date loaded directly from yahoo finance
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  
  dat = data.frame(getSymbols('^GDAXI',src='yahoo', from = as.Date(init_date)-2000, to = as.Date(init_date)+1, auto.assign=FALSE)) %>%
    mutate(ret1 = compute_return(GDAXI.Adjusted, h = 1),
           ret2 = compute_return(GDAXI.Adjusted, h = 2),
           ret3 = compute_return(GDAXI.Adjusted, h = 3),
           ret4 = compute_return(GDAXI.Adjusted, h = 4),
           ret5 = compute_return(GDAXI.Adjusted, h = 5))#,
           #Date = ymd(Date))
  dat = cbind(Date = rownames(dat), dat)
  return(dat)
}


dax_quantreg = function(init_date, transpose=FALSE, rolling_window=100, give_data = FALSE, 
                        data = NA, quantile_levels = c(0.025,0.25,0.5,0.75,0.975)){
  #' DAX Forecast using quantile regression with moving_window of rolling_window
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' transpose: Boolean, expresses weather the results should be transposed or not, e.g. TRUE
  #' rolling_window: Integer setting the amount of past observations to be included to the training set, e.g. 150
  #' data: dataset containing dax data, standard value NA -> get newest (just for testing purposes as of now)
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # get data
  if(!give_data){
    dat = get_dax_data_directly(init_date)
  } else {
    dat = data
  }
  # restrict to data in rolling window
  dat = dat[(dim(dat)[1]-rolling_window):dim(dat)[1],]
  # prep
  pred_rq = matrix(NA, length(quantile_levels), 5)
  #rownames(pred_rq) = c("q0.025","q0.25","q0.5","q0.75","q0.975")
  colnames(pred_rq) = c("1 day", "2 days", "5 days", "6 days", "7 days")
  # Run separate quantile regression for each h (see p. 19-21 on slides)
  for (h in 1:5){
    # Select returns at steps h and 1
    if (h > 1){
      df <- dat[, paste0("ret", c(1, h))]
    } else {
      df <- data.frame(ret1 = dat$ret1)
    }
    # Use lagged one-step return as regressor
    df$lag_abs_ret <- lag(abs(df$ret1), h)
    # Select data, drop NA values
    df <- df[, c(paste0("ret", h), "lag_abs_ret")] %>% na.omit
    # Fit quantile regression for current value of h
    fit <- rq(formula(paste0("ret", h, "~ lag_abs_ret")),
              data = df,
              tau = quantile_levels)
    # Most recent value of regressor (used for prediction)
    new_data <- data.frame(lag_abs_ret = tail(abs(na.omit(dat$ret1)), 1))
    # Enter prediction into matrix
    pred_rq[,h] <- predict(fit, newdata = new_data)
  }
  if(transpose){
    pred_rq = t(pred_rq)
  }
  return(pred_rq)
}


dax_ugarch = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(1,1), history_size = 1400, solver='solnp'){
  #' DAX Forecast using GARCH(n,m) model with ARMA(1,1) model. Might be modularized further later on. Own GARCH model for each horizon
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' garchorder: vector containing the order of the GARCH model, e.g. c(6,6)
  #' history_size: integer containing the size of the rolling window to be used, whereas 1400 (result of hypertuning)
  #' solver: string containing solver algo as in ?ugarchfit, probably for testing purposes only
  
  # Prepare data
  dax_data = get_dax_data_directly(init_date)
  dax_data = dax_data[!is.na(dax_data$ret5),]
  dax_data = subset(dax_data, as.Date(Date) > as.Date(init_date) - history_size )
  # Model
  spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = garchorder),
                    mean.model = (list(armaOrder = c(1,1), include.mean = TRUE)),
                    distribution.model = 'std')
  # Prepare Output
  pred_rq = matrix(NA, ncol=length(quantile_levels), nrow=5)
  for (i in 1:5){
    # Prepare Data for ret_i
    retx = paste0('ret',i)
    dax_df = dax_data[,c('Date',retx)] %>% na.omit
    dax_subset = dax_df
    dax_df = data.frame(dax_subset[,retx])
    rownames(dax_df) = dax_subset$Date
    # Fit Model
    ugarch_fit = ugarchfit(spec, data = dax_df, solver=solver)
    # Forecasts
    ugarch_fc = ugarchforecast(ugarch_fit, n.ahead = i)
    for (n_quantile in 1:length(quantile_levels)){
      # fitted(ugarch_fc) and quantile(ugarch_fc, 0.5) yield the same value!
      pred_rq[i,n_quantile] = quantile(ugarch_fc, probs=quantile_levels[n_quantile])[i]
    }
  }
  return(pred_rq)
}


dax_ugarch_combined = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(1,1), history_sizes, debug=FALSE){
  #' DAX Forecast using GARCH(1,1) model with ARMA(1,1) model. Might be modularized further later on. Own GARCH model for each horizon
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' history_sizes: vector containing the sizes of the rolling window to be used, whereas 0 indicates the 2020-01-01, e.g. c(200,800)
  #' debug: boolean wether debugging (printing history_sizes)
  
  for(model_num in 1:length(history_sizes)){
    print(history_sizes[model_num])
    #fcst = paste('fcst',model_num,sep='')
    #assign(fcst, dax_ugarch(init_date=init_date, quantile_levels=quantile_levels, 
    #                        garchorder=garchorder, history_size=history_sizes[model_num]))
    fcst = dax_ugarch(init_date=init_date, quantile_levels=quantile_levels, 
                      garchorder=garchorder, history_size=history_sizes[model_num])
    print('fitted')
    if(model_num==1){
      fcst_array = array(fcst,dim=c(5,length(quantile_levels),length(history_sizes)))
    } else {
      fcst_array[,,model_num] = fcst
    }
  }
  fcst_combined = combine_many_forecasts(fc_array=fcst_array, weights=0)
  return(fcst_combined)
}


dax_quantgarch = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(6,6), history_size = 1400, 
                          solver='solnp', rolling_window=800, weight_garch=0.5){
  #' Arguments as used in subfunctions
  
  fcst_garch = dax_ugarch(init_date = init_date, quantile_levels = quantile_levels, 
                          garchorder = garchorder, history_size = history_size, solver = solver)
  fcst_quantreg = dax_quantreg(init_date=init_date, transpose=TRUE, quantile_levels=quantile_levels, rolling_window=rolling_window)
  fcst_out = combine_forecasts(fcst_garch, fcst_quantreg, weights = c(weight_garch, 1-weight_garch))
  return(fcst_out)
}
