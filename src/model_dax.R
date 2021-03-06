# This file contains several functions that serve the purpose of estimating DAX (log) returns


dax_baseline = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975)){
  data = get_dax_data_directly(init_date)#, hist_size=1000) # this includes weekends and holidays
  data = data[1:1000,]
  forecasts = matrix(NA, nrow=5, ncol=length(quantile_levels))
  for(horizon in 1:5){
    forecasts[horizon,] = quantile(data[,paste0('ret',horizon)], quantile_levels, na.rm=TRUE)
  }
  return(forecasts)
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


dax_ugarch = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(1,1), 
                      history_size = 1400, solver='solnp'){
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


dax_ugarch_combined = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(1,1), 
                               history_sizes=c(243,800,1030), debug=FALSE){
  #' DAX Forecast using GARCH(1,1) model with ARMA(1,1) model. Might be modularized further later on. Own GARCH model for each horizon
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
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


dax_quantgarch = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(6,6), 
                          history_size = 1200, solver='solnp', rolling_window=300, weight_garch=0.5){
  #' Arguments as used in subfunctions
  
  fcst_garch = dax_ugarch(init_date = init_date, quantile_levels = quantile_levels, 
                          garchorder = garchorder, history_size = history_size, solver = solver)
  fcst_quantreg = dax_quantreg(init_date=init_date, transpose=TRUE, quantile_levels=quantile_levels, rolling_window=rolling_window)
  fcst_out = combine_forecasts(fcst_garch, fcst_quantreg, weights = c(weight_garch, 1-weight_garch))
  return(fcst_out)
}


dax_qrf = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), 
                   add_futures=TRUE, add_msci=FALSE, add_ucits=FALSE, add_us_futures=FALSE, days_before=0){
  #' Function for Quantile Regression Forest for DAX using stock market statistics calculated from GDAXI values
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' add_futures: Boolean, wether to contain DAX Futures from DY (FDAX.EX)
  #' add_...: Same as add_futures, but other indices
  #' days_before: Integer containing the amount of days before the init date that should be used for predictions
  
  # Get data
  predictor_variables = c("ret1", "ret2", "ret3", "ret4", "ret5", "RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV")
  if(add_futures){
    predictor_variables = append(predictor_variables, "DY.Adjusted")
    predictor_variables = append(predictor_variables, "DY.Volume")
  }
  if(add_msci){
    predictor_variables = append(predictor_variables, "XWD.TO.Adjusted")
    predictor_variables = append(predictor_variables, "XWD.TO.Volume")
  }
  if(add_ucits){
    predictor_variables = append(predictor_variables, "EXS1.DE.Adjusted")
    predictor_variables = append(predictor_variables, "EXS1.DE.Volume")
  }
  if(add_us_futures){
    predictor_variables = append(predictor_variables, "YM.F.Adjusted")
    predictor_variables = append(predictor_variables, "YM.F.Volume")
  }
  data = dax_qrf_feature_eng_train(init_date, add_futures=add_futures, add_msci=add_msci, 
                                   add_ucits=add_ucits, add_us_futures=add_us_futures)
  # Add earlier days as predictors
  data = data[,predictor_variables]
  predictor_variables_base = predictor_variables
  if(days_before>1){
    for(num_day in 1:days_before){
      data_earlier = data[1:(dim(data)[1]-num_day),predictor_variables_base]
      data = data[(1+num_day):(dim(data)[1]),]
      index(data_earlier) = index(data)
      colnames(data_earlier) = paste0(colnames(data_earlier),'_',num_day,'_earlier')
      data = cbind(data, data_earlier)
      predictor_variables = append(predictor_variables, paste0(predictor_variables_base,'_',num_day,'_earlier'))
    }
  }
  df_predict = data[dim(data)[1], predictor_variables]
  # QRF
  predictions = matrix(NA, ncol=length(quantile_levels), nrow=5)
  for(horizon in 1:5){
    # Training data
    df_predict_train = data[1:(dim(data)[1]-1), predictor_variables]
    df_predict_train = df_predict_train[-((dim(df_predict_train)[1]-(horizon-1)):dim(df_predict_train)[1]),] # Leave out some predictors
    df_obs_train = data[1:(dim(data)[1]-1),c(paste0("ret",horizon))]
    df_obs_train = df_obs_train[-(1:horizon),] # Leave out first 'horizon' obs -> Predict ret5 based on variables measured 5 day before
    # Train and predict
    qrf = quantregForest(df_predict_train, df_obs_train, nthreads = 4)
    predictions[horizon,] = predict(qrf, newdata = df_predict, what = quantile_levels)
  }
  return(predictions)
}


dax_qrfgarch = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(6,6), 
                        history_size = 1400, solver='solnp', weight_garch=0.5, add_futures=TRUE, days_before=0){
  #' Arguments as used in subfunctions
  
  fcst_garch = dax_ugarch(init_date = init_date, quantile_levels = quantile_levels, 
                          garchorder = garchorder, history_size = history_size, solver = solver)
  fcst_qrf = dax_qrf(init_date=init_date, quantile_levels=quantile_levels, add_futures=add_futures, days_before=0)
  fcst_out = combine_forecasts(fcst_garch, fcst_qrf, weights = c(weight_garch, 1-weight_garch))
  return(fcst_out)
}
