# This file contains several functions that serve the purpose of estimating temperature

temp_baseline = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function that implements baseline model for temp: Use raw ensemble quantiles
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  ensemble_data = get_current_temp_data(init_date)
  fcst = matrix(ncol = length(quantile_levels), nrow = 5)
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    subset = subset(ensemble_data, fcst_hour==lead_time)
    fcst[i,] = quantile(subset, quantile_levels, na.rm=TRUE)
    i = i+1
  }
  return(fcst)
}

temp_emos = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), training_data){
  #' Function to make forecasts of temp using EMOS with normal distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  if(missing(training_data)){
    # Get historical data
    t2m_data_raw = get_hist_temp_data()
    # Get current ensemble forecasts
    new_fcst = get_current_temp_data(init_date)
    # get rid of empty first and last column
    new_fcst[,1] = NULL
    new_fcst[,ncol(new_fcst)] = NULL
  }
  else{
    t2m_data_raw = training_data
    new_fcst = subset(get_hist_temp_data(), init_tm==as.Date(init_date))[,c(4,7:46)]
  }
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    t2m_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                          data = t2m_data,
                          dist = "gaussian",
                          link.scale = "log",
                          type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    t2m_benchmark2_loc = predict(t2m_benchmark2,
                                 pred_df,
                                 type = "location")
    t2m_benchmark2_sc = predict(t2m_benchmark2,
                                pred_df,
                                type = "scale")
    t2m_benchmark2_pred = qnorm(quantile_levels, t2m_benchmark2_loc, t2m_benchmark2_sc)
    # Write to Output Data
    fcst_temp[i,] = t2m_benchmark2_pred
    i = i+1
  }
  # Forecasts ready to write to csv
  return(fcst_temp)
}

temp_emos_multi = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), training_data){
  #' Function to make forecasts of temp using EMOS with normal distribution and additional regressor radiation
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  if(missing(training_data)){
    # Get historical data
    t2m_data_raw = get_hist_temp_data()
    # Get current ensemble forecasts temp
    new_fcst = get_current_temp_data(init_date)
    # Get rid of empty first and last column
    new_fcst[,1] = NULL
    new_fcst[,ncol(new_fcst)] = NULL
    
    # Get historic rad data
    data_direct_rad = get_hist_data_varname('direct_rad')
    # Get current rad data
    new_fcst_rad = get_current_data_varname('direct_rad', init_date)
    # at 2021-11-17 name was changed from aswdir-s to direct-rad in gitlab...
    new_fcst_rad[,1] = NULL
    new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  }
  else{
    # Get wind data
    t2m_data_raw = training_data
    new_fcst = subset(get_hist_temp_data(), init_tm==as.Date(init_date))[,c(4,7:46)]
    # Get rad data
    data_direct_rad = get_hist_data_varname('direct_rad')
    new_fcst_rad = subset(get_hist_data_varname('direct_rad'), init_tm==as.Date(init_date))[,c(4,7:46)]
  }
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    # get rad data for forecast horizon
    rad_data = subset(data_direct_rad, fcst_hour == lead_time)
    rad_data = rad_data[!is.na(rad_data$obs),]
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    # Merge data temp and rad
    merge = merge(x=t2m_data, y=rad_data, by="obs_tm")
    # evaluate model on full historic data (with corresponding lead_time)
    t2m_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd, 
                     data = merge,
                     dist = "gaussian",
                     link.scale = "log",
                     type = "crps")
    
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # extract current forecasts of rad
    ens_fc_rad = new_fcst_rad[new_fcst_rad$fcst_hour == lead_time,][2:ncol(new_fcst_rad)]
    ens_fc_rad = as.numeric(ens_fc_rad)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_rad, na.rm=TRUE))
    t2m_model_loc = predict(t2m_model,
                            pred_df,
                            type = "location")
    t2m_model_sc = predict(t2m_model,
                           pred_df,
                           type = "scale")
    t2m_model_pred = qnorm(quantile_levels, t2m_model_loc, t2m_model_sc)
    # Write to Output Data
    fcst_temp[i,] = t2m_model_pred
    i = i+1
  }
  # Forecasts ready to write to csv
  return(fcst_temp)
}

temp_emos_multi_boosting = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), training_data){
  #' Function to make forecasts of temp using EMOS with normal distribution and additional regressor radiation
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  if(missing(training_data)){
    # Get historical data
    t2m_data_raw = get_hist_temp_data()
    # Get current ensemble forecasts temp
    new_fcst = get_current_temp_data(init_date)
    # Get rid of empty first and last column
    new_fcst[,1] = NULL
    new_fcst[,ncol(new_fcst)] = NULL
    
    # Get historic rad data
    data_direct_rad = get_hist_data_varname('direct_rad')
    # Get current rad data
    new_fcst_rad = get_current_data_varname('direct_rad', init_date)
    # at 2021-11-17 name was changed from aswdir-s to direct-rad in gitlab...
    new_fcst_rad[,1] = NULL
    new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  }
  else{
    # Get wind data
    t2m_data_raw = training_data
    new_fcst = subset(get_hist_temp_data(), init_tm==as.Date(init_date))[,c(4,7:46)]
    
    # Get rad data
    data_direct_rad = get_hist_data_varname('direct_rad')
    new_fcst_rad = subset(get_hist_data_varname('direct_rad'), init_tm==as.Date(init_date))[,c(4,7:46)]
  }
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    # get rad data for forecast horizon
    rad_data = subset(data_direct_rad, fcst_hour == lead_time)
    rad_data = rad_data[!is.na(rad_data$obs),]
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    # Merge data temp and rad
    merge = merge(x=t2m_data, y=rad_data, by="obs_tm")
    # evaluate model on full historic data (with corresponding lead_time)
    t2m_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd, 
                     data = merge,
                     dist = "gaussian",
                     link.scale = "log",
                     type = "crps",
                     method = 'boosting')
    
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # extract current forecasts of rad
    ens_fc_rad = new_fcst_rad[new_fcst_rad$fcst_hour == lead_time,][2:ncol(new_fcst_rad)]
    ens_fc_rad = as.numeric(ens_fc_rad)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_rad))
    t2m_model_loc = predict(t2m_model,
                            pred_df,
                            type = "location")
    t2m_model_sc = predict(t2m_model,
                           pred_df,
                           type = "scale")
    t2m_model_pred = qnorm(quantile_levels, t2m_model_loc, t2m_model_sc)
    # Write to Output Data
    fcst_temp[i,] = t2m_model_pred
    i = i+1
  }
  # Forecasts ready to write to csv
  return(fcst_temp)
}

temp_emos_multi_boosting_mixture = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), weights=c(0.5,0.5), ...){
  #' Function to make forecasts of temp using EMOS with normal distribution and additional regressor radiation
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  #' weights: vector containing numeric weights (that add up to 1) for combination of models
  
  fc_emos = temp_emos_multi(init_date, quantile_levels, ...)
  fc_boost = temp_emos_multi_boosting(init_date, quantile_levels, ...)
  fc_comb = combine_forecasts(fc_emos, fc_boost, weights)
  return(fc_comb)
}

temp_qrf = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5,
                    addmslp=FALSE, addclct=FALSE, addrad=FALSE, training_data){
  #' Function that predicts temp based on a quantile regression forest
  #' init_date: as all the time
  #' quantile_levels: as all the time
  #' ntree: number of trees in random forest (see randomForest Docu)
  #' nodesize: minimum size of terminal nodes (see randomForest Docu)
  
  if(missing(training_data)){
    # Get historical data
    df = get_hist_temp_data() %>% na.omit
    df_new = get_current_temp_data(init_date)[,-c(1,43)]
  }
  else{
    df = training_data %>% na.omit
    df_new = subset(get_hist_temp_data(), init_tm==as.Date(init_date))[,c(4,7:46)]
  }
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train(df=df, lt=lead_time, addmslp, addclct, addrad)
    df_training_target = df_training[,10]
    df_training_predictors = df_training[,-10]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new_predictors = qrf_feature_eng_predict(df_new, lead_time, init_date, addmslp=addmslp, addclct=addclct, addrad=addrad)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
