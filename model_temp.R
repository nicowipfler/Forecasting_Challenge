# Preparations ------------------------------------------------------------

library(scoringRules)
library(lubridate)
library(crch)
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)

# Load Data ---------------------------------------------------------------

data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//"
data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//"
load(paste0(data_dir, "icon_eps_t_2m.RData"))
t2m_data_raw = data_icon_eps
rm(data_icon_eps)

# Examine Data ------------------------------------------------------------

head(t2m_data_raw)
range(t2m_data_raw$init_tm)

horizon = 48
t2m_data = subset(t2m_data_raw, fcst_hour == horizon)[!is.na(t2m_data$obs),]

# Evaluate raw ensemble predictions ---------------------------------------

t2m_ensfc_matrix <- as.matrix(t2m_data[,7:46])
t2m_ens_crps <- crps_sample(y = t2m_data$obs, dat = t2m_ensfc_matrix)
# this yields the CRPS of the ensemble forecasts (something like MSE)
summary(t2m_ens_crps)

t2m_ens_vrh <- sapply(seq_along(1:nrow(t2m_ensfc_matrix)),
                      function(i) rank(c(t2m_data$obs[i], t2m_ensfc_matrix[i,]))[1])
hist(t2m_ens_vrh, nclass = 41, freq = F, main="Verification Rank Histogram")
# because the histogram is not flat at all, we can conclude that post-processing is needed

# Fit EMOS model ----------------------------------------------------------

t2m_data$ens_sd = sqrt(t2m_data$ens_var)
# split data for training and testing
t2m_data_train = subset(t2m_data, init_tm <= "2020-09-24")
t2m_data_test = subset(t2m_data, init_tm >= "2020-09-25")
# EMOS model
t2m_model = crch(obs ~ ens_mean|ens_sd, # model formula
                  data = t2m_data_train, # dataset
                  dist = "gaussian", # parametric forecast distribution
                  link.scale = "log", # link function for scale parameter
                  type = "crps") # loss function
t2m_model
# evaluate ensemble predictions on test set
t2m_ensfc_matrix_test = as.matrix(t2m_data_test[,7:46])
t2m_ens_crps_test = crps_sample(y = t2m_data_test$obs, dat = t2m_ensfc_matrix_test)
mean(t2m_ens_crps_test)
# evaluate EMOS model on test set
t2m_pred_loc = predict(t2m_model,
                        t2m_data_test,
                        type = "location")
t2m_pred_sc = predict(t2m_model,
                       t2m_data_test,
                       type = "scale")
t2m_model_crps_test = crps(y = t2m_data_test$obs, family = "norm", mean = t2m_pred_loc, sd = t2m_pred_sc)
mean(t2m_model_crps_test)

# Assess Calibration on test data set -------------------------------------

t2m_ens_vrh_test = sapply(seq_along(1:nrow(t2m_ensfc_matrix_test)),
                           function(i) rank(c(t2m_data_test$obs[i], t2m_ensfc_matrix_test[i,]))[1])
t2m_model_pit_test = pnorm(t2m_data_test$obs, t2m_pred_loc, t2m_pred_sc)
par(mfrow=c(1,2))
hist(t2m_ens_vrh_test, nclass = 41, freq = F, main="Raw Ensemble Predictions"); abline(h = 1/41, lty = 2)
hist(t2m_model_pit_test, nclass = 41, freq = F, ylim = c(0,6), main="EMOS Model"); abline(h = 1, lty = 2)

###########################################################################
#TODO Automatisiert für alle forecast horizons (examine data gehört dazu)

# Estimate Final Model ----------------------------------------------------

t2m_benchmark2 = crch(obs ~ ens_mean|ens_sd,
                       data = t2m_data,
                       dist = "gaussian",
                       link.scale = "log",
                       type = "crps")

# Get Current Ensemble Forecasts ------------------------------------------

#TODO Hier automatisiert das Datum anpassen?
new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_2021102300_t_2m_Karlsruhe.txt"),
                       sep = "|", header = TRUE)
# get rid of empty first and last row
new_fcst[,1] = NULL
new_fcst[,ncol(new_fcst)] = NULL
# extract forecasts for targeted horizon
ens_fc = new_fcst[new_fcst$fcst_hour == horizon,][2:ncol(new_fcst)]
ens_fc = as.numeric(ens_fc)

# Forecast ----------------------------------------------------------------

# baseline model
t2m_benchmark1_pred = quantile(ens_fc, quantile_levels)
# EMOS model
pred_df = data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
t2m_benchmark2_loc = predict(t2m_benchmark2,
                              pred_df,
                              type = "location")
t2m_benchmark2_sc = predict(t2m_benchmark2,
                             pred_df,
                             type = "scale")
t2m_benchmark2_pred = qnorm(quantile_levels, t2m_benchmark2_loc, t2m_benchmark2_sc)
rbind(t2m_benchmark1_pred,t2m_benchmark2_pred)
