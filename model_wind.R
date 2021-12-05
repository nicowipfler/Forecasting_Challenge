# This file contains several functions that serve the purpose of estimating wind


wind_emos_tn = function(init_date, mode=1, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated normal distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' mode: Integer indicating wether [1] forecasts or [2] model_parameters are to be returned
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  new_fcst = get_current_wind_data(init_date)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  if(mode==2){
    pars = matrix(nrow=5,ncol=2)
  }
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    wind_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                           data = wind_data,
                           dist = "gaussian",
                           left = 0,
                           truncated = TRUE,
                           link.scale = "log",
                           type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    wind_benchmark2_loc = predict(wind_benchmark2,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_benchmark2,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtnorm(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    if(mode==2){
      pars[i,1] = wind_benchmark2_loc
      pars[i,2] = wind_benchmark2_sc
    }
    i = i+1
  }
  if(mode==1){
    return(fcst_wind)
  }
  else if(mode==2){
    return(pars)
  }
}

wind_emos_tl = function(init_date, mode=1, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' mode: Integer indicating wether [1] forecasts or [2] model_parameters are to be returned
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  new_fcst = get_current_wind_data(init_date)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  if(mode==2){
    pars = matrix(nrow=5,ncol=2)
  }
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # evaluate model on full historic data (with corresponding lead_time)
    wind_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                           data = wind_data,
                           dist = "logistic",
                           left = 0,
                           truncated = TRUE,
                           link.scale = "log",
                           type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
    wind_benchmark2_loc = predict(wind_benchmark2,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_benchmark2,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtlogis(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    if(mode==2){
      pars[i,1] = wind_benchmark2_loc
      pars[i,2] = wind_benchmark2_sc
    }
    i = i+1
  }
  if(mode==1){
    return(fcst_wind)
  }
  else if(mode==2){
    return(pars)
  }
}

wind_emos_tl_multi = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution and additional regressor mslp
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get historic mslp data
  data_mslp = get_hist_data_varname('mslp')
  # Get current ensemble forecasts
  new_fcst = get_current_wind_data(init_date)
  # Get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Get current rad data
  new_fcst_mslp = get_current_data_varname('mslp', init_date)
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # get rad data for forecast horizon
    mslp_data = subset(data_mslp, fcst_hour == lead_time)
    mslp_data = mslp_data[!is.na(mslp_data$obs),]
    # Merge data temp and rad
    merge = merge(x=wind_data, y=mslp_data, by="obs_tm")
    # evaluate model on full historic data (with corresponding lead_time)
    wind_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd,
                      data = merge,
                      dist = "logistic",
                      left = 0,
                      truncated = TRUE,
                      link.scale = "log",
                      type = "crps")
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # extract current forecasts of mslp
    ens_fc_mslp = new_fcst_mslp[new_fcst_mslp$fcst_hour == lead_time,][2:ncol(new_fcst_mslp)]
    ens_fc_mslp = as.numeric(ens_fc_mslp)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_mslp, na.rm=TRUE))
    wind_benchmark2_loc = predict(wind_model,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_model,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtlogis(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    i = i+1
  }
  return(fcst_wind)
}

wind_emos_tl_multi_boosting = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution and additional regressor mslp via BOOSTING
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # get historic mslp data
  data_mslp = get_hist_data_varname('mslp')
  # Get current ensemble forecasts
  new_fcst = get_current_wind_data(init_date)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_mslp = get_current_data_varname('mslp', init_date)
  new_fcst_mslp[,1] = NULL
  new_fcst_mslp[,ncol(new_fcst_mslp)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # get rad data for forecast horizon
    mslp_data = subset(data_mslp, fcst_hour == lead_time)
    mslp_data = mslp_data[!is.na(mslp_data$obs),]
    # Merge data temp and rad
    merge = merge(x=wind_data, y=mslp_data, by="obs_tm")
    # evaluate model on full historic data (with corresponding lead_time)
    wind_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd,
                      data = merge,
                      dist = "logistic",
                      left = 0,
                      truncated = TRUE,
                      link.scale = "log",
                      type = "crps",
                      method = 'boosting')
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    # extract current forecasts of rad
    ens_fc_mslp = new_fcst_mslp[new_fcst_mslp$fcst_hour == lead_time,][2:ncol(new_fcst_mslp)]
    ens_fc_mslp = as.numeric(ens_fc_mslp)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_mslp))
    wind_benchmark2_loc = predict(wind_model,
                                  pred_df,
                                  type = "location")
    wind_benchmark2_sc = predict(wind_model,
                                 pred_df,
                                 type = "scale")
    wind_benchmark2_pred = qtlogis(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
    # Write to Output Data
    fcst_wind[i,] = wind_benchmark2_pred
    i = i+1
  }
  return(fcst_wind)
}

wind_qrf = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5,
                    addmslp=FALSE, addclct=FALSE, addrad=FALSE){
  #' Function that predicts wind based on a quantile regression forest
  #' init_date: as all the time
  #' quantile_levels: as all the time
  #' ntree: number of trees in random forest (see randomForest Docu)
  #' nodesize: minimum size of terminal nodes (see randomForest Docu)
  
  df = get_hist_wind_data() %>% na.omit
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
    df_new = get_current_wind_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict(df_new, lead_time, init_date, addmslp=addmslp, addclct=addclct, addrad=addrad)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
