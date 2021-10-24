# Preparations ------------------------------------------------------------


library(scoringRules)
library(lubridate)
library(crch)
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)


# Load Data ---------------------------------------------------------------


# Get historical data
data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//"
data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//"
load(paste0(data_dir, "icon_eps_t_2m.RData"))
t2m_data_raw = data_icon_eps
rm(data_icon_eps)

# Get current ensemble forecasts
# TODO CHANGE DATE when current file has been downloaded to the corresponding folder
new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_2021102300_t_2m_Karlsruhe.txt"),
                      sep = "|", header = TRUE)
# get rid of empty first and last row
new_fcst[,1] = NULL
new_fcst[,ncol(new_fcst)] = NULL


# Model -------------------------------------------------------------------


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
  
  # forecast with baseline model
  t2m_benchmark1_pred = quantile(ens_fc, quantile_levels)
  
  # forecast with EMOS model
  pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
  t2m_benchmark2_loc = predict(t2m_benchmark2,
                               pred_df,
                               type = "location")
  t2m_benchmark2_sc = predict(t2m_benchmark2,
                              pred_df,
                              type = "scale")
  t2m_benchmark2_pred = qnorm(quantile_levels, t2m_benchmark2_loc, t2m_benchmark2_sc)
  
  # print forecasts made by both models
  print(paste("lead_time:",lead_time,"h"))
  print(rbind(t2m_benchmark1_pred,t2m_benchmark2_pred))
}
