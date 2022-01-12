# Script containing several exploratory analyses aiming to improve existing models or to find new ones
source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2',
                   'rugarch','quantmod','quantregForest','moments','TTR','gbm'))
source('src/model_enhancements_toolkit.R')
#


# WEEK 2: Test better fitting distributions for EMOS wind -----------------


# First plot PIT rank validation diagrams for EMOS model using truncated normal
wind_data_raw = get_hist_wind_data()
horizons = c(36,48,60,72,84)

for (i in 1:5){
  # For each horizon
  fcst_horizon = horizons[i]
  # Get the corresponding data
  wind_data = subset(wind_data_raw, fcst_hour == fcst_horizon)
  # That is not NA
  wind_data = wind_data[!is.na(wind_data$obs),]
  # Add SD
  wind_data$ens_sd <- sqrt(wind_data$ens_var)
  # Split data for training and testing
  wind_data_train <- subset(wind_data, init_tm <= "2020-10-31")
  wind_data_test <- subset(wind_data, init_tm >= "2020-11-01")
  # Model as in wind_model_tn
  wind_model <- crch(obs ~ ens_mean|ens_sd,
                     data = wind_data_train,
                     dist = "gaussian",
                     left = 0,
                     truncated = TRUE,
                     link.scale = "log",
                     type = "crps")
  # Evaluate on test data
  wind_pred_loc <- predict(wind_model,
                           wind_data_test,
                           type = "location")
  wind_pred_sc <- predict(wind_model,
                          wind_data_test,
                          type = "scale")
  wind_model_pit_test <- ptnorm(wind_data_test$obs, wind_pred_loc, wind_pred_sc, left = 0)
  hist(wind_model_pit_test, nclass = 41, freq = F, ylim = c(0,10)); abline(h = 1, lty = 2)
  Sys.sleep(2) 
}

# truncated_log.pdf indicates, that truncated logistic distribution is a much better fit

# So: Now plot PIT rank validation diagrams for EMOS model using truncated logistic
for (i in 1:5){
  # For each horizon
  fcst_horizon = horizons[i]
  # Get the corresponding data
  wind_data = subset(wind_data_raw, fcst_hour == fcst_horizon)
  # That is not NA
  wind_data = wind_data[!is.na(wind_data$obs),]
  # Add SD
  wind_data$ens_sd <- sqrt(wind_data$ens_var)
  # Split data for training and testing
  wind_data_train <- subset(wind_data, init_tm <= "2020-10-31")
  wind_data_test <- subset(wind_data, init_tm >= "2020-11-01")
  # Model as in wind_model_tn
  wind_model <- crch(obs ~ ens_mean|ens_sd,
                     data = wind_data_train,
                     dist = "logistic",
                     left = 0,
                     truncated = TRUE,
                     link.scale = "log",
                     type = "crps")
  # Evaluate on test data
  wind_pred_loc <- predict(wind_model,
                           wind_data_test,
                           type = "location")
  wind_pred_sc <- predict(wind_model,
                          wind_data_test,
                          type = "scale")
  wind_model_pit_test <- ptlogis(wind_data_test$obs, wind_pred_loc, wind_pred_sc, left = 0)
  hist(wind_model_pit_test, nclass = 41, freq = F, ylim = c(0,10)); abline(h = 1, lty = 2)
  Sys.sleep(2) 
}


# differences aren't huge


# WEEK 2: Find optimal window_length for DAX Quantile Regression ----------


library('ggplot2')
source('src/model_dax.R')
source('src/model_enhancements_toolkit.R')
data = get_dax_data('2021-10-27')
# Start erst nach der Finanzkrise
data = subset(data, as.Date(Date) > '2010-01-01')
# Nur Zeilen mit keinem NA
data = data[complete.cases(data),]
dates = data$Date
amount_data = length(dates)
# Prepare matrix for scores
length_scores = matrix(nrow=1491,ncol=2)
# Quantile Levels
quantile_levels = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 
# First implementation: c(0.05,0.25,0.5,0.75,0.975), but for this analysis they should be equidistant

# For each rolling_window length:
for (window_length in 10:1500){
  # Split Data into sections of length (window_length+5)
  section_size = window_length + 5
  amount_sections = floor(amount_data / section_size)
  # Scores in each section:
  scores = matrix(nrow=amount_sections,ncol=1)
  # For each section
  for (i in 1:amount_sections){
    section = data[(amount_data-i*section_size+1):(amount_data-(i-1)*section_size),]
    # Predict its last 5 values based on all the others (exactly window_length observations)
    pred = dax_quantreg(section[length(section)-5]$Date, transpose=TRUE, rolling_window=window_length, give_data=TRUE, 
                        data=section[1:(section_size-5),], quantile_levels=quantile_levels)
    # Compute Quantile Scores of these predictions on equidistant grid
    score = matrix(nrow=5,ncol=length(quantile_levels))
    for (horizon in 1:5){
      obs = section[(section_size-5+horizon),paste0('ret',horizon)]
      for (n_quantile in 1:length(quantile_levels)){
        quantile = quantile_levels[n_quantile]
        forecast = pred[horizon, n_quantile]
        score[horizon, n_quantile] = quantile_score(quantile, forecast, obs)
      }
    }
    # Mean of all quantile scores is approx. of CRPS in this section
    scores[i] = mean(score)
  }
  # Mean of all scores is approx. of CRPS in general (using corresponding window_length)
  print(paste0('Mean Score for window_length ', window_length,': ', mean(scores)))
  length_scores[window_length-9,1] = window_length
  length_scores[window_length-9,2] = mean(scores)
}

plot(length_scores,type='l',xlab='w_length',ylab='mean(mean(quantile_scores))',
     main='Scores of Forecasts made using Quantile Regression with window of length w_length')
# Man sieht: Die Scores sind sehr noisy
# Aber insgesamt lassen sich leichte Trends erkennen 
# Eine window_length von circa 145 oder 150 scheint optimal zu sein
# Speichere eine Grafik für den Bericht
length_scores_df = data.frame(length_scores)
length_scores_df
colnames(length_scores_df) = c('w_len', 'score')
dev.new()
ggplot(length_scores_df, aes(w_len, score)) + 
  geom_line() + 
  geom_smooth(method='loess',level=0) +
  ggtitle('Scores of Forecasts made using Quantile Regression with window of length w_length') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x='window_length')
# Blaue (smoothe) Linie mit LOESS (Locally estimated scatterplot smoothing)
savePlot(filename='C://dev//Forecasting_Challenge//graphics for elaboration//DAX_quantile_regression_optimal_window_length_EQUIDISTANT_QUANTILES.jpg', type="jpeg")
dev.off()


# WEEK 3: GARCH for DAX ---------------------------------------------------


# as common
source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2'))
source('src/model_dax.R')
# for garch model
library('rugarch')
# for qq plots
library('car')


## Prepare Data


dax_data = get_dax_data("2021-10-27")
dax_data = dax_data[!is.na(dax_data$ret5),]
ggplot(data = dax_data, aes(x = Date, y = ret1)) + geom_line()
# Can be modelled as stationary time series, so ARMA modelling before GARCH is not necessary
dax_df = dax_data[,c('Date','ret1')]
dax_subset = subset(dax_df, as.Date(Date) > '2020-01-01')
dax_df = data.frame(dax_subset[,'ret1'])
rownames(dax_df) = dax_subset$Date


## Erstes Test-Modell


spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  mean.model = (list(armaOrder = c(1,1), include.mean = TRUE)),
                  distribution.model = 'norm')
ugarch_fit_test = ugarchfit(spec, data = dax_df)
res_std_test = ugarch_fit_test@fit$residuals / ugarch_fit_test@fit$sigma
qqPlot(res_std_test, dist='norm', main='QQ plot of residuals assuming normal distribution in GARCH model')
# Normalverteilung scheint nicht ganz zu passen, versuche t-Verteilung


## DAS MODELL


spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  mean.model = (list(armaOrder = c(1,1), include.mean = TRUE)),
                  distribution.model = 'std')
# Modell schätzen
ugarch_fit = ugarchfit(spec, data = dax_df)
ugarch_fit
# Ljung-Box-Test: Statistisch kann keine Korrellation mehr in dne quadrierten, standardisierten Residuen nachgewiesen werden
# Standardisierte Residuen:
res_std = residuals(ugarch_fit, standardize = TRUE)
# df der t-Vert. in Modellspezifikation
t_df = ugarch_fit@fit$coef['shape']
ugarch_fit@fit$coef
# Problem: residuals()-Funktion liefert Daten mit, das wollen wir hier nicht. Also manuell:
res_std_2 = ugarch_fit@fit$residuals / ugarch_fit@fit$sigma
qqPlot(res_std_2, dist='t', df=t_df, ylim=c(-6,6))
# Besser, aber noch nicht gut
infocriteria(ugarch_fit_test)
infocriteria(ugarch_fit)
# Auch nach Informationskriterien (etwas) besser!
# NOTE: Also ist student distr. besser geeignet als normal distr.
# NOTE: ARMA(1,1)-Modell inkludiert, obwohl Zeitreihe beinahe stationär, da Modell mit ARMA besseren AIC, BIC, ... als ohne liefert. Siehe unten


## FORECASTS


ugarch_fc = ugarchforecast(ugarch_fit, n.ahead = 5)
quantile(ugarch_fc, probs=0.025)
quantile(ugarch_fc, probs=0.25)
fitted(ugarch_fc)
quantile(ugarch_fc, probs=0.75)
quantile(ugarch_fc, probs=0.975)
# Visualization
plot(ugarch_fc)


## Überprüfe relative Überdeckungshäufigkeiten für Forecasts innerhalb des Modells


# GRAFISCH
# ret1
plot(dax_subset$ret1, type='l')
# forecasted mean
in_fit = ugarch_fit@fit$fitted.values
points(in_fit, pch=16, col='blue', cex=0.2)
# forecasted q0.025
q0.025 = as.vector(quantile(ugarch_fit, 0.025))
points(q0.025, pch=16, col='green', cex=0.15)
# forecasted q0.25
q0.25 = as.vector(quantile(ugarch_fit, 0.25))
points(q0.25, pch=16, col='darkgreen', cex=0.15)
# forecasted q0.75
q0.75 = as.vector(quantile(ugarch_fit, 0.75))
points(q0.75, pch=16, col='darkgreen', cex=0.15)
# forecasted q0.975
q0.975 = as.vector(quantile(ugarch_fit, 0.975))
points(q0.975, pch=16, col='green', cex=0.15)
# NUMERISCH
count_vec_rel = function(vec, le=TRUE){
  count = 0
  if(le){
    for (i in 1:length(vec)){
      if(vec[i] < 0){
        count = count+1
      }
    }
  }
  else if (ge){
    for (i in 1:length(vec)){
      if(vec[i] > 0){
        count = count+1
      }
    }
  }
  return(count/length(vec))
}
# Relativer Anteil der Beobachtungen, die unter dem vorhergesagten 0.025 Quantil liegen
count_vec_rel(dax_subset$ret1-q0.025)
# Relativer Anteil der Beobachtungen, die unter dem vorhergesagten 0.25 Quantil liegen
count_vec_rel(dax_subset$ret1-q0.25)
# Relativer Anteil der Beobachtungen, die über dem vorhergesagten 0.75 Quantil liegen
count_vec_rel(q0.75-dax_subset$ret1)
# Relativer Anteil der Beobachtungen, die über dem vorhergesagten 0.975 Quantil liegen
count_vec_rel(q0.975-dax_subset$ret1)
# Insgesamt sind die Überdeckungshäufigkeiten der Konfidenzintervalle also SEHR gut:
# Should be about 0.5
count_vec_rel(dax_subset$ret1-q0.25) + count_vec_rel(q0.75-dax_subset$ret1)
# Should be about 0.05
count_vec_rel(dax_subset$ret1-q0.025) + count_vec_rel(q0.975-dax_subset$ret1)
# Das Modell scheint also keine systematischen Fehler zu machen


## (Test des Modells in einem Rolling Window Ansatz:)


# Verwendung in Rolling Window Ansatz
modelroll = ugarchroll(spec = spec, dax_df, n.ahead = 1, forecast.length = 5, 
                        refit.every = 1, refit.window = 'moving', window.size = 150, calculate.VaR = FALSE)
# Hier lassen sich bei Mu die vorhergesagten Erwartungswerte und bei Realized die realisierten Werte ablesen
modelroll


## Test, ob ARMA modeling weglassen das Modell besser macht


spec_test = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  mean.model = (list(armaOrder = c(0,0), include.mean = TRUE)),
                  distribution.model = 'std')
# Modell schätzen
ugarch_fit_test = ugarchfit(spec_test, data = dax_df)
infocriteria(ugarch_fit_test)
# Mit ARMA minimal besser


# WEEK 3: Seasonal Data for EMOS ------------------------------------------


# Prep
source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
source('src/model_wind.R')
source('src/model_temp.R')
source('src/model_enhancements_toolkit.R')

#### Testversion der Funktion temp_emos
emos_temp_test = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975),season_radius=0,lead_time=36){
  # prepare historical data
  t2m_data_raw = get_hist_temp_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 1)
  # MODEL
  # create dataset with ensemble predictions and observations corresponding to current lead time
  t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
  t2m_data = t2m_data[!is.na(t2m_data$obs),]
  t2m_data$ens_sd = sqrt(t2m_data$ens_var)
  # Only use seasonal data if season_days_around > 0
  if(season_radius>0){
    lower = as.Date(init_date) - season_radius
    upper = as.Date(init_date) + season_radius
    t2m_data = subset(t2m_data, as.Date(init_tm) %in% lower:upper | 
                        as.Date(init_tm) %in% (lower-365):(upper-365) | 
                        as.Date(init_tm) %in% (lower-2*365):(upper-2*365)| 
                        as.Date(init_tm) %in% (lower-3*365):(upper-3*365))
  }
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
  fcst_temp[1,] = t2m_benchmark2_pred
  return(fcst_temp)
}
#emos_temp_test('2021-10-27',season_radius = 40,lead_time=36)
#emos_temp_test('2021-10-27',season_radius = 0,lead_time=36)

# Evaluate Scores of different values for season_radius
#TODO
init_date = '2021-11-03'
# Quantile Levels
quantile_levels = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# Lead times
lead_times = c(36,48,60,72,84)
# Current Observations
dwd_url = selectDWD(
  name = "Berlin-Tempelhof",
  res = "hourly",
  per = "recent",
  var = 'air_temperature'
)
dataDWD(dwd_url)
obs_data = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_air_temperature_recent_stundenwerte_TU_00433_akt.zip')

# Get observations
dates = generate_times(init_date)
observations = subset(obs_data,
                      MESS_DATUM == dates[1]|
                        MESS_DATUM == dates[2]|
                        MESS_DATUM == dates[3]|
                        MESS_DATUM == dates[4]|
                        MESS_DATUM == dates[5])$TT_TU
# For each lead_time
for (n_lead_time in 1:5){
  lead_time = lead_times[n_lead_time]
  obs = observations[n_lead_time]
  scores = matrix(nrow=181,1)
  # For each season_radius:
  for (season_radius in 0:180){
    forecasts = emos_temp_test(init_date,season_radius = season_radius,lead_time=lead_time,quantile_levels=quantile_levels)
    # Compute Quantile Scores of these predictions on equidistant grid
    score = matrix(nrow=1,ncol=length(quantile_levels))
    for (n_quantile in 1:length(quantile_levels)){
      quantile = quantile_levels[n_quantile]
      forecast = forecasts[n_quantile]
      score[1, n_quantile] = quantile_score(quantile, forecast, obs)
    }
    #print(paste0('Model using season radius of ',season_radius,' has a CRPS (approx.) of ', mean(score), ' for lead_time ', lead_time))
    # Mean of all quantile scores is approx. of CRPS for this season_radius
    scores[season_radius+1] = mean(score)
  }
  # Plot Scores for every season_radius tested for given lead_time and init_date
  plot(scores,type='l',xlab='season_radius',ylab='mean(quantile_scores)',
       main=paste0('EMOS Forecast Scores with seasonal r (x-axis) for lead_time ',lead_time, ' init on ', init_date))
  invisible(readline(prompt="Press [enter] to continue"))
  # Bester season_radius:
  #opt_sr = which(scores == min(scores))
  #print(paste0('Bester season_radius für lead_time of ', lead_time, ' is ', opt_sr))
}


# WEEK 4: Additional regressors for weather -------------------------------


source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2'))
source('src/model_temp.R')
# Start with temp
data_temp = get_hist_temp_data()
data_temp
# Get other vars
data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
load(paste0(data_dir, "icon_eps_clct.RData"))
data_clct = data_icon_eps
rm(data_icon_eps)
data_clct
data_clct$obs[!is.na(data_clct$obs)]
# clct obs nur NA, hier gibt es nur ensemble forecasts
load(paste0(data_dir, "icon_eps_mslp.RData"))
data_mslp = data_icon_eps
rm(data_icon_eps)
data_mslp
# pressure available
load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
data_aswdir_s = data_icon_eps
rm(data_icon_eps)
data_aswdir_s
# short wave radiation available
plot(obs~obs_tm, data=data_aswdir_s)
data_temp

# For horizon of 36:
data_temp_36 = subset(data_temp, fcst_hour==36)
data_rad_36 = subset(data_aswdir_s, fcst_hour==36)
plot(obs~obs_tm, data=data_rad_36)
points(obs~obs_tm, data=data_temp_36)
test = merge(x=data_temp_36, y=data_rad_36, by="obs_tm")
test
plot(obs.x ~ ens_mean.y, data=test)
# Is there correlation?
cor.test(test$obs.x,test$ens_mean.y,method='pearson',use="complete.obs")
# YES (as expected)

## THIS SECTION IS SUPERFLUOUS, BECAUSE ENS MEAN IS USED FOR FORECASTING
# But is there also usage for forecasting? E.g. is there correlation on lag (in this case) 36?
#temp = test$obs.x
#temp = temp[37:length(temp)]
#length(temp)
#rad = test$obs.y
#rad = rad[1:(length(rad)-36)]
#length(rad)
# Is there correlation?
#cor.test(rad,temp,method='pearson',use="complete.obs")
# At least there is correlation! So it might be useful for forecasting!
# Lets also try for horizon 84 (max)
#data_temp_84 = subset(data_temp, fcst_hour==84)
#data_rad_84 = subset(data_aswdir_s, fcst_hour==84)
#test_84 = merge(x=data_temp_84, y=data_rad_84, by="obs_tm")
#temp_84 = test_84$obs.x
#temp_84 = temp_84[37:length(temp_84)]
#length(temp_84)
#rad_84 = test_84$obs.y
#rad_84 = rad_84[1:(length(rad_84)-36)]
#length(rad_84)
# Is there correlation?
#cor.test(rad_84,temp_84,method='pearson',use="complete.obs")
# YES!
# SO IT IS JUSTIFIED TO INCLUDE RADIATION TO REGRESSION

## RATHER TEST: Correlation between ens mean temp und rad
cor.test(test$ens_mean.x,test$ens_mean.y,method='pearson',use="complete.obs")
# Yes, and also much, but slightly less than between ens mean rad and obs temp, so it might be useful for forecasting anyways

# TEST FUNCTIONS
temp_emos_multivariate_double_sd = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  t2m_data_raw = get_hist_temp_data()
  
  #NEW# get rad data historic
  load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
  data_aswdir_s = data_icon_eps
  
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  
  #NEW# get current rad data
  new_fcst_rad = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_aswdir_s_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_rad[,1] = NULL
  new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    
    #NEW# get rad data for forecast horizon
    rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
    rad_data = rad_data[!is.na(rad_data$obs),]
    rad_data$ens_sd = sqrt(rad_data$ens_var)
    
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    
    #NEW# Merge data temp and rad
    test = merge(x=t2m_data, y=rad_data, by="obs_tm")
    
    #CHANGED# evaluate model on full historic data (with corresponding lead_time)
    t2m_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd.x + ens_sd.y, 
                          data = test,
                          dist = "gaussian",
                          link.scale = "log",
                          type = "crps")
    
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    
    #NEW# extract current forecasts of rad
    ens_fc_rad = new_fcst_rad[new_fcst_rad$fcst_hour == lead_time,][2:ncol(new_fcst_rad)]
    ens_fc_rad = as.numeric(ens_fc_rad)
    
    #CHANEGED# forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd.x = sd(ens_fc), ens_mean.y = mean(ens_fc_rad), ens_sd.y = sd(ens_fc_rad))
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
temp_emos_multivariate_one_sd = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  t2m_data_raw = get_hist_temp_data()
  
  #NEW# get rad data historic
  load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
  data_aswdir_s = data_icon_eps
  
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  
  #NEW# get current rad data
  new_fcst_rad = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_aswdir_s_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_rad[,1] = NULL
  new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    
    #NEW# get rad data for forecast horizon
    rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
    rad_data = rad_data[!is.na(rad_data$obs),]
    
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    
    #NEW# Merge data temp and rad
    test = merge(x=t2m_data, y=rad_data, by="obs_tm")
    
    #CHANGED# evaluate model on full historic data (with corresponding lead_time)
    t2m_model = crch(obs.x ~ ens_mean.y + ens_mean.x|ens_sd, 
                     data = test,
                     dist = "gaussian",
                     link.scale = "log",
                     type = "crps")
    
    # extract current forecasts for targeted lead_time
    ens_fc = new_fcst[new_fcst$fcst_hour == lead_time,][2:ncol(new_fcst)]
    ens_fc = as.numeric(ens_fc)
    
    #NEW# extract current forecasts of rad
    ens_fc_rad = new_fcst_rad[new_fcst_rad$fcst_hour == lead_time,][2:ncol(new_fcst_rad)]
    ens_fc_rad = as.numeric(ens_fc_rad)
    
    #CHANEGED# forecast with EMOS model
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

eval = matrix(nrow=1, ncol=3)
eval[1,1] = evaluate_model_temp(temp_emos)
eval[1,2] = evaluate_model_temp(temp_emos_multivariate_double_sd)
eval[1,3] = evaluate_model_temp(temp_emos_multivariate_one_sd)
eval
# Also: Nur Standardabweichung von ens temp!


# WEEK 4: Selection of regressors for multivariate temp EMOS model --------


source('src/model_temp.R')
# Get temp
data_temp = get_hist_temp_data()
# Get cloud cover
data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
load(paste0(data_dir, "icon_eps_clct.RData"))
data_clct = data_icon_eps
# Get mean sea level pressure
load(paste0(data_dir, "icon_eps_mslp.RData"))
data_mslp = data_icon_eps
# Get downward radiation
load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
data_aswdir_s = data_icon_eps
rm(data_icon_eps)

# FIRST CHECK: CORRELATION BETWEEN TEMP OBS AND MEAN ENSEMBLE VALUES FOR THE DIFFERENT VARS

horizons = c(36,48,60,72,84)
cors = matrix(nrow=5,ncol=2)
rownames(cors) = c('Pearson-Correlation', 'p-value')
colnames(cors) = horizons

library('gridExtra')
library('ggplot2')

# Check Correlation of temp and radiation
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  temp = subset(data_temp, fcst_hour==horizon)
  other = subset(data_aswdir_s, fcst_hour==horizon)
  merged = merge(x=temp, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# Of course correlation is stronger at 1200, when sun is shining, but also at night statistically significant (alltough high variance!)

# Check Correlation of temp and cloud cover
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  temp = subset(data_temp, fcst_hour==horizon)
  other = subset(data_clct, fcst_hour==horizon)
  merged = merge(x=temp, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# At night, the variables are statisticalyy uncorrelated! At day, there might be a small correlation, that could be tried to fit into the model

# Check Correlation of temp and mean sea level pressure
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  temp = subset(data_temp, fcst_hour==horizon)
  other = subset(data_mslp, fcst_hour==horizon)
  merged = merge(x=temp, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# At day: Statistically uncorrelated, however at night there i a small correlation, that could be tried to fit into the model

# ALL IN ALL: CHOOSING SHORTWAVE DOWNWARD RADIATION AS FIRST ADDITIONAL REGRESSOR IS RATIONAL
# CLCT MAY BE USED TO ENHANCE FORECASTS AT DAY, MSLP AT NIGHT

# Additionally: Check wind
source('src/model_wind.R')
data_wind = get_hist_wind_data()
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  temp = subset(data_temp, fcst_hour==horizon)
  other = subset(data_wind, fcst_hour==horizon)
  merged = merge(x=temp, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# There is a correlation indeed, so it might be included as additional regressor


# WEEK 4: Models including multiple regressors for temp -------------------


temp_emos_multi_rad = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  t2m_data_raw = get_hist_temp_data()
  # get historic rad data
  load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
  data_aswdir_s = data_icon_eps
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_rad = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_aswdir_s_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_rad[,1] = NULL
  new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    # get rad data for forecast horizon
    rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
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
temp_emos_multi_rad_wind = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  # get historic temp data
  t2m_data_raw = get_hist_temp_data()
  # get historic rad data
  load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
  data_aswdir_s = data_icon_eps
  # get historic wind data
  wind_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_rad = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_aswdir_s_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_rad[,1] = NULL
  new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  # get current wind data
  new_fcst_wind = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_wind[,1] = NULL
  new_fcst_wind[,ncol(new_fcst_wind)] = NULL
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
    t2m_data = t2m_data[!is.na(t2m_data$obs),]
    t2m_data$ens_sd = sqrt(t2m_data$ens_var)
    # get rad data for forecast horizon
    rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
    rad_data = rad_data[!is.na(rad_data$obs),]
    # get wind data for forecast horizon
    wind_data = subset(wind_raw, fcst_hour == lead_time)
    wind_data = wind_data[!is.na(wind_data$obs),]
    # Merge data temp and rad
    merge = merge(x=t2m_data, y=rad_data, by="obs_tm")
    merge = merge(x=merge, y=wind_data, by="obs_tm")
    # evaluate model on full historic data (with corresponding lead_time)
    t2m_model = crch(obs.x ~ ens_mean + ens_mean.y + ens_mean.x|ens_sd, 
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
    # extract current forecasts of wind
    ens_fc_wind = new_fcst_wind[new_fcst_wind$fcst_hour == lead_time,][2:ncol(new_fcst_wind)]
    ens_fc_wind = as.numeric(ens_fc_wind)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_rad), ens_mean = mean(ens_fc_wind))
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

# EVALUATE MODELS FOR ONLY TWO AVAILABLE DATES USING CRPS APPROX
crps = matrix(nrow=1,ncol=4)
crps[1] = suppressWarnings(evaluate_model_weather(temp_emos,'air_temperature'))
crps[2] = suppressWarnings(evaluate_model_weather(temp_emos_multi_rad,'air_temperature'))
crps[3] = suppressWarnings(evaluate_model_weather(temp_emos_multi_rad_wind,'air_temperature'))
crps

# check if model including wind differs greatly
temp_emos_multi_rad('2021-11-03')
temp_emos_multi_rad_wind('2021-11-03')
# no, but approx of CRPS is worse.
# SO WIND SHOULD NOT BE INCLUDED
# Try rad + clct for daytimes
temp_emos_multi_rad_clct = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  # get historic temp data
  t2m_data_raw = get_hist_temp_data()
  # get historic rad data
  load(paste0(data_dir, "icon_eps_aswdir_s.RData"))
  data_aswdir_s = data_icon_eps
  # get historic rad data
  load(paste0(data_dir, "icon_eps_clct.RData"))
  clct_raw = data_icon_eps
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_rad = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_aswdir_s_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_rad[,1] = NULL
  new_fcst_rad[,ncol(new_fcst_rad)] = NULL
  # get current clct data
  new_fcst_clct = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_clct_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_clct[,1] = NULL
  new_fcst_clct[,ncol(new_fcst_clct)] = NULL
  # Prepare Output Data
  fcst_temp = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    if(lead_time == 48 | lead_time == 72){
      # create dataset with ensemble predictions and observations corresponding to current lead time
      t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
      t2m_data = t2m_data[!is.na(t2m_data$obs),]
      t2m_data$ens_sd = sqrt(t2m_data$ens_var)
      # get rad data for forecast horizon
      rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
      rad_data = rad_data[!is.na(rad_data$obs),]
      # get wind data for forecast horizon
      clct_data = subset(clct_raw, fcst_hour == lead_time)
      clct_data = clct_data[!is.na(clct_data$obs),]
      # Merge data temp and rad
      merge = merge(x=t2m_data, y=rad_data, by="obs_tm")
      merge = merge(x=merge, y=clct_raw, by="obs_tm")
      # evaluate model on full historic data (with corresponding lead_time)
      t2m_model = crch(obs.x ~ ens_mean + ens_mean.y + ens_mean.x|ens_sd, 
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
      # extract current forecasts of wind
      ens_fc_clct = new_fcst_clct[new_fcst_clct$fcst_hour == lead_time,][2:ncol(new_fcst_clct)]
      ens_fc_clct = as.numeric(ens_fc_clct)
      # forecast with EMOS model
      pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_rad), ens_mean = mean(ens_fc_clct))
      t2m_model_loc = predict(t2m_model,
                              pred_df,
                              type = "location")
      t2m_model_sc = predict(t2m_model,
                             pred_df,
                             type = "scale")
      t2m_model_pred = qnorm(quantile_levels, t2m_model_loc, t2m_model_sc)
    } else {
      # create dataset with ensemble predictions and observations corresponding to current lead time
      t2m_data = subset(t2m_data_raw, fcst_hour == lead_time)
      t2m_data = t2m_data[!is.na(t2m_data$obs),]
      # get rad data for forecast horizon
      rad_data = subset(data_aswdir_s, fcst_hour == lead_time)
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
      pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_rad))
      t2m_model_loc = predict(t2m_model,
                              pred_df,
                              type = "location")
      t2m_model_sc = predict(t2m_model,
                             pred_df,
                             type = "scale")
      t2m_model_pred = qnorm(quantile_levels, t2m_model_loc, t2m_model_sc)
    }
    # Write to Output Data
    fcst_temp[i,] = t2m_model_pred
    i = i+1
  }
  # Forecasts ready to write to csv
  return(fcst_temp)
}

crps[4] = suppressWarnings(evaluate_model_weather(temp_emos_multi_rad_clct,'air_temperature'))
crps
# Better than adding wind on top, but worse than just adding rad
# SO MSLP FOR NIGHT WONT BE TRIED (EVEN SMALLER CORRELATION)


# WEEK 4: Selection of regressors for multivariate wind EMOS model --------


source('src/model_temp.R')
source('src/model_wind.R')
# Get wind and temp
data_wind = get_hist_wind_data()
data_temp = get_hist_temp_data()
# Get cloud cover
data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
load(paste0(data_dir, "icon_eps_clct.RData"))
data_clct = data_icon_eps
# Get mean sea level pressure
load(paste0(data_dir, "icon_eps_mslp.RData"))
data_mslp = data_icon_eps
# Get downward radiation
load(paste0(data_dir, "icon_eps_direct_rad.RData"))
data_aswdir_s = data_icon_eps
rm(data_icon_eps)
rm(data_dir)

# Prep
horizons = c(36,48,60,72,84)
cors = matrix(nrow=5,ncol=2)
colnames(cors) = c('Pearson-Correlation', 'p-value')
rownames(cors) = horizons
library('gridExtra')
library('ggplot2')

# Check Correlation of wind and radiation
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  wind = subset(data_wind, fcst_hour==horizon)
  other = subset(data_aswdir_s, fcst_hour==horizon)
  merged = merge(x=wind, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# Minimal correlation (altough significant), but probably not strong enough to fucntion as regressor

# Check Correlation of wind and cloud cover
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  wind = subset(data_wind, fcst_hour==horizon)
  other = subset(data_clct, fcst_hour==horizon)
  merged = merge(x=wind, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# There is significant correlation, but not very strong over all

# Check Correlation of wind and mean sea level pressure
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  wind = subset(data_wind, fcst_hour==horizon)
  other = subset(data_mslp, fcst_hour==horizon)
  merged = merge(x=wind, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# There is (neg) correlation, but not very strong

# Check correlation of wind and temp
for (n_horizon in 1:5){
  horizon = horizons[n_horizon]
  wind = subset(data_wind, fcst_hour==horizon)
  other = subset(data_temp, fcst_hour==horizon)
  merged = merge(x=wind, y=other, by="obs_tm")
  test = cor.test(merged$obs.x,merged$ens_mean.y,method='pearson',use="complete.obs")
  cors[n_horizon,1] = test$estimate
  cors[n_horizon,2] = test$p.value
  nam = paste('plot',n_horizon,sep='')
  assign(nam, ggplot(merged, aes(x = ens_mean.y, y = obs.x)) + geom_point() + geom_smooth(col='blue',method='lm'))
}
# cors
cor_table = tableGrob(cors)
grid.arrange(plot1, plot2, plot3, plot4, plot5, cor_table, nrow = 3)
# There also is correlation, but lets see...


# WEEK 4: Model selection wind EMOS ---------------------------------------


source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
source('src/model_enhancements_toolkit.R')
source('src/model_wind.R')
crps = matrix(nrow=1,ncol=4)
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10')
crps[1] = suppressWarnings(evaluate_model_weather(wind_emos_tl,'wind',init_dates=init_dates))

wind_emos_tl_multi_mslp = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # get historic mslp data
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_mslp.RData"))
  data_mslp = data_icon_eps
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_mslp = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_mslp_Berlin.txt"), sep = "|", header = TRUE)
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
                           type = "crps")
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

crps[2] = suppressWarnings(evaluate_model_weather(wind_emos_tl_multi_mslp,'wind',init_dates=init_dates))

wind_emos_tl_multi_clct = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # get historic clct data
  data_dir = "C://dev//Forecasting_Challenge//data//weather_historical//Berlin//"
  load(paste0(data_dir, "icon_eps_clct.RData"))
  data_clct = data_icon_eps
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_clct = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_clct_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_clct[,1] = NULL
  new_fcst_clct[,ncol(new_fcst_clct)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # get rad data for forecast horizon
    clct_data = subset(data_clct, fcst_hour == lead_time)
    # Merge data temp and rad
    merge = merge(x=wind_data, y=clct_data, by="obs_tm")
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
    # extract current forecasts of rad
    ens_fc_clct = new_fcst_clct[new_fcst_clct$fcst_hour == lead_time,][2:ncol(new_fcst_clct)]
    ens_fc_clct = as.numeric(ens_fc_clct)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_clct))
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

crps[3] = suppressWarnings(evaluate_model_weather(wind_emos_tl_multi_clct,'wind',init_dates=init_dates))

wind_emos_tl_multi_temp = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # get historic temp data
  data_temp = get_hist_temp_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
  # get rid of empty first and last column
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
  # get current rad data
  new_fcst_temp = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  new_fcst_temp[,1] = NULL
  new_fcst_temp[,ncol(new_fcst_temp)] = NULL
  # Prepare Output Data
  fcst_wind = matrix(ncol = length(quantile_levels), nrow = 5)
  # MODEL
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # create dataset with ensemble predictions and observations corresponding to current lead time
    wind_data = subset(wind_data_raw, fcst_hour == lead_time)
    wind_data$ens_sd = sqrt(wind_data$ens_var)
    # get rad data for forecast horizon
    temp_data = subset(data_temp, fcst_hour == lead_time)
    temp_data = temp_data[!is.na(temp_data$obs),]
    # Merge data temp and rad
    merge = merge(x=wind_data, y=temp_data, by="obs_tm")
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
    # extract current forecasts of rad
    ens_fc_temp = new_fcst_temp[new_fcst_temp$fcst_hour == lead_time,][2:ncol(new_fcst_temp)]
    ens_fc_temp = as.numeric(ens_fc_temp)
    # forecast with EMOS model
    pred_df = data.frame(ens_mean.x = mean(ens_fc), ens_sd = sd(ens_fc), ens_mean.y = mean(ens_fc_temp))
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

crps[4] = suppressWarnings(evaluate_model_weather(wind_emos_tl_multi_temp,'wind',init_dates=init_dates))
crps
save(crps, file='graphics and tables for elaboration/weather/additional_regressors/wind/scores.RData')


# WEEK 4: DAX GARCH HYPERTUNING -------------------------------------------


evaluate_model_dax(dax_quantreg,quantreg=TRUE)
evaluate_model_dax(dax_ugarch)
# So UGARCH is much better than quantreg!

# How can we enhance UGARCH even further?
dax_ugarch_test = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975),garchorder,armaorder){
  #' DAX Forecast using GARCH(1,1) model with ARMA(1,1) model. Might be modularized further later on. Own GARCH model for each horizon
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Prepare data
  dax_data = get_dax_data(init_date)
  dax_data = dax_data[!is.na(dax_data$ret5),]
  # Model
  spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = garchorder),
                    mean.model = (list(armaOrder = armaorder, include.mean = TRUE)),
                    distribution.model = 'std')
  # Prepare Output
  pred_rq = matrix(NA, ncol=length(quantile_levels), nrow=5)
  for (i in 1:5){
    # Prepare Data for ret_i
    retx = paste0('ret',i)
    dax_df = dax_data[,c('Date',retx)]
    dax_subset = subset(dax_df, as.Date(Date) > '2020-01-01')
    dax_df = data.frame(dax_subset[,retx])
    rownames(dax_df) = dax_subset$Date
    # Fit Model
    ugarch_fit = ugarchfit(spec, data = dax_df)
    # Forecasts
    ugarch_fc = ugarchforecast(ugarch_fit, n.ahead = i)
    for (n_quantile in 1:length(quantile_levels)){
      # fitted(ugarch_fc) and quantile(ugarch_fc, 0.5) yield the same value!
      pred_rq[i,n_quantile] = quantile(ugarch_fc, probs=quantile_levels[n_quantile])[i]
    }
    #pred_rq[i,1] = quantile(ugarch_fc, probs=0.025)[i]
    #pred_rq[i,2] = quantile(ugarch_fc, probs=0.25)[i]
    #pred_rq[i,3] = fitted(ugarch_fc)[i]
    #pred_rq[i,4] = quantile(ugarch_fc, probs=0.75)[i]
    #pred_rq[i,5] = quantile(ugarch_fc, probs=0.975)[i]
  }
  return(pred_rq)
}
evaluate_model_dax(dax_ugarch_test, garchorder=c(8,8), armaorder=c(1,1))
# First: Analyze choice of ARMA order (for simplification here: symmetric oders for GARCH and ARMA)
benchmark = evaluate_model_dax(dax_quantreg,quantreg=TRUE)
hypertuning_arma = matrix(nrow = 2, ncol = 10)
for (n_garch in 1:2){
  for (n_arma in 1:10){
    hypertuning_arma[n_garch, n_arma] = evaluate_model_dax(dax_ugarch_test, garchorder=c(n_garch,n_garch), armaorder=c(n_arma,n_arma))
  }
}
benchmark
hypertuning_arma
# So we see, that ARMA(1,1) is the best choice here
# Lets proceed with the GARCH order (given ARMA(1,1), so we dont have to restrict to symmetric orders any more)
hypertuning_garch = matrix(nrow = 10, ncol = 10)
for (i in 1:10){
  for (j in 1:10){
    hypertuning_garch[i, j] = evaluate_model_dax(dax_ugarch_test, garchorder=c(i,j), armaorder=c(1,1))
    print(paste0(((i-1)*10+j),' % done'))
  }
}
benchmark
hypertuning_garch
plot(diag(hypertuning_garch))
# So (6,6) is the best model with symmetric parameters and more dont enhance the model further
which(hypertuning_garch == min(hypertuning_garch), arr.ind = TRUE)
# The very best model is (10,6), so lets look at (.,6)
plot(hypertuning_garch[,6])
# Not that much better than (6,6)
# SO I CHOOSE GARCH(6,6)
# SAVE RESULTS IN RDATA
save(benchmark, hypertuning_arma, hypertuning_garch, file = "garch_hyperparametertuning.RData")
# TO RESTORE (i moved the file manually)
load('C://dev//Forecasting_Challenge//graphics and tables for elaboration//DAX//GARCH//garch_hyperparametertuning.RData')


# WEEK 5: GARCH history_size ----------------------------------------------


source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
source('src/model_dax.R')
source('src/model_enhancements_toolkit.R')
test_length = 2000
scores = matrix(nrow = test_length+1, ncol = 2)
# Benchmark: Current model starting on 2020-01-01
scores[1,2] = evaluate_model_dax(dax_ugarch, history_size = 0)
scores[1,1] = 0
for(window in 1:test_length){
  number = window + 1
  tryCatch({scores[number,2] = evaluate_model_dax(dax_ugarch, history_size = window)},
           error = function(e){scores[number,2] = NA; print(paste0('Error for window ', window,': ', e))})
  scores[number,1] = window
  print(paste0((number-1)/test_length*100, '% finished'))
}
scores
plot(scores)
colnames(scores) = c('window','score')
ggplot(as.data.frame(scores), aes(x=window, y=score)) + geom_point() + geom_line() +
  xlab("window length") + ylab("average score")
min(scores[,2], na.rm=TRUE)
# Dieses Minimum liegt in einer sehr volatilen Gegend
# Betrachte lokale Minima bei 250, 800 und 1050
min(scores[200:300,], na.rm=TRUE)
which(scores[,2] == min(scores[200:300,], na.rm=TRUE))
min(scores[750:850,], na.rm=TRUE)
which(scores[,2] == min(scores[750:850,], na.rm=TRUE))
min(scores[900:1200,], na.rm=TRUE)
which(scores[,2] == min(scores[900:1200,], na.rm=TRUE))
# Also lokale Minima exakt in 243, 800 und 1030, wobei die Minima absteigend kleiner werden
# Save Scores, i moved them into the correct folder manually!
save(scores, file = "garch_hyperparametertuning_history_size.RData")
# Generate better plot
scores = scores[1:1300,]
scores
ggplot(as.data.frame(scores), aes(x=window, y=score)) + geom_point() + geom_line() +
  xlab("window length") + ylab("average score")
# Saved as avg_scores_to_2000, but GARCH model cant handle more than 1300 days of data
# Implemented function dax_ugarch_combined in model_dax.R that can combine multiple GARCH models
# So sieht ein Aufruf aus:
dax_ugarch_combined('2021-11-03', garchorder=c(6,6), history_sizes=c(243,800,1030))

## Neuer Test mit anderem Solver in ugarchfit: Ändert nichts, außer dass (ab 100) alle Verfahren konvergieren
test_length = 2000
scores = matrix(nrow = test_length+1, ncol = 2)
# Benchmark: Current model starting on 2020-01-01
scores[1,2] = evaluate_model_dax(dax_ugarch, history_size = 0)
scores[1,1] = 0
for(window in 100:(100+test_length-1)){
  number = window - 98
  tryCatch({scores[number,2] = evaluate_model_dax(dax_ugarch, history_size = window, solver='hybrid')},
           error = function(e){scores[number,2] = NA; print(paste0('Error for window ', window,': ', e))})
  scores[number,1] = window
  print(paste0((number-1)/test_length*100, '% finished'))
}
scores
plot(scores)
colnames(scores) = c('window','score')
ggplot(as.data.frame(scores), aes(x=window, y=score)) + geom_point() + geom_line() +
  xlab("window length") + ylab("average score")




# WEEK 5: temp (gradient) boosting ----------------------------------------


source('src/toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
source('src/model_temp.R')
source('src/model_enhancements_toolkit.R')
scores_temp = matrix(nrow=4, ncol=1, 0)
rownames(scores_temp) = c('Multi EMOS', '+ Boosting', 'Mixture', 'Univariate + Boosting')
scores_temp[1] = evaluate_model_weather(temp_emos_multi,'air_temperature')
scores_temp[2] = evaluate_model_weather(temp_emos_multi_boosting,'air_temperature')
# Makes the forecasts worse. Not by that much, but worse.
# Try out mixture:
temp_mixture_test = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  fc1 = temp_emos_multi(init_date, quantile_levels)
  fc2 = temp_emos_multi_boosting(init_date, quantile_levels)
  fc = combine_forecasts(fc1, fc2)
}
scores_temp[3] = evaluate_model_weather(temp_mixture_test,'air_temperature')
temp_univariate_boost = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with normal distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # prepare historical data
  t2m_data_raw = get_hist_temp_data()
  # Get current ensemble forecasts
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_t_2m_Berlin.txt"), sep = "|", header = TRUE)
  # Get rid of empty first and last row
  new_fcst[,1] = NULL
  new_fcst[,ncol(new_fcst)] = NULL
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
                          type = "crps",
                          method = 'boosting')
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
scores_temp[4] = evaluate_model_weather(temp_univariate_boost,'air_temperature')
scores_temp
# THE MIXTURE OUTPERFORMS BOTH INDIVIDUAL MODELS
# What are the optimal weights? -> Next week!


# WEEK 5: wind (gradient) boosting  ---------------------------------------


source('src/model_wind.R')
scores_wind = matrix(nrow=4, ncol=1, 0)
rownames(scores_wind) = c('Multi EMOS', '+ Boosting', 'Mixture', 'Univariate + Boosting')
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')
scores_wind[1] = evaluate_model_weather(wind_emos_tl_multi,'wind',init_dates=init_dates)
scores_wind[2] = evaluate_model_weather(wind_emos_tl_multi_boosting,'wind',init_dates=init_dates)
# Try out mixture:
wind_mixture_test = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  fc1 = wind_emos_tl_multi(init_date, quantile_levels)
  fc2 = wind_emos_tl_multi_boosting(init_date, quantile_levels)
  fc = combine_forecasts(fc1, fc2)
}
scores_wind[3] = evaluate_model_weather(wind_mixture_test,'wind',init_dates=init_dates)
# Test: Univariate wind EMOS with boosting
wind_emos_tl_boost = function(init_date, mode=1, quantile_levels=c(0.025,0.25,0.5,0.75,0.975)){
  #' Function to make forecasts of temp using EMOS with truncated logistic distribution
  #' init_date: String containing date of initialization of forecasts, e.g. "2021-10-23"
  #' mode: Integer indicating wether [1] forecasts or [2] model_parameters are to be returned
  #' quantile_levels: Vector of floats between 0 and 1 containing the quantiles, where forecasts should be made, e.g. c(0.25,0.5,0.75)
  
  # Get historical data
  wind_data_raw = get_hist_wind_data()
  # Get current ensemble forecasts
  # TODO CHANGE DATE when current file has been downloaded to the corresponding folder
  data_dir_daily = "C://dev//Forecasting_Challenge//data//weather_daily//Berlin//"
  date_formatted = gsub('-','',init_date)
  new_fcst = read.table(file = paste0(data_dir_daily, "icon-eu-eps_",date_formatted,"00_wind_mean_10m_Berlin.txt"), sep = "|", header = TRUE)
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
                           type = "crps",
                           method = 'boosting')
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
scores_wind[4] = evaluate_model_weather(wind_emos_tl_boost,'wind',init_dates=init_dates)
scores_wind
# BOOSTING OUTPERFPORMS PURE EMOS AND MIXTURE (and quite heavily to be honest)
save(scores_temp, scores_wind, file = "graphics and tables for elaboration/weather/EMOS_boosting_scores.RData")
load(file = "graphics and tables for elaboration/weather/EMOS_boosting_scores.RData")

# WEEK 6: Evaluate current DAX model --------------------------------------


source('src/model_dax.R')
# Model scores based on the last 4 weeks
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')
scores_dax_4weeks = matrix(nrow=5, ncol=1, 0)
rownames(scores_dax_4weeks) = c('current: garch mixture (3 history sizes)', 'simple garch', 'quantreg hist 150', 
                         'quantreg hist 900', 'NEW: combination of best garch and best quantreg')
scores_dax_4weeks[1] = evaluate_model_dax(dax_ugarch_combined,garchorder=c(6,6), history_sizes = c(245,800,1029),
                                   debug=TRUE, init_dates = init_dates)
scores_dax_4weeks[2] = evaluate_model_dax(dax_ugarch,garchorder=c(6,6), init_dates = init_dates)
scores_dax_4weeks[3] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=150,init_dates = init_dates)
scores_dax_4weeks[4] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=900,init_dates = init_dates)
dax_garch_quantreg_comb = function(init_date, quantile_levels = c(0.025,0.25,0.5,0.75,0.975), garchorder=c(6,6), history_size = 0, solver='solnp',
                                   transpose=FALSE, rolling_window=900){
  #' First row of arguments for GARCH
  #' Second for quantreg
  fcst_garch = dax_ugarch(init_date, quantile_levels, garchorder, history_size, solver)
  fcst_quantreg = dax_quantreg(init_date=init_date, transpose=TRUE, quantile_levels=quantile_levels, rolling_window=rolling_window)
  fcst_out = combine_forecasts(fcst_garch, fcst_quantreg)
  
  return(fcst_out)
}
scores_dax_4weeks[5] = evaluate_model_dax(dax_garch_quantreg_comb,init_dates = init_dates)
scores_dax_4weeks
# Model scores based on the last week that was very volatile
init_dates = c('2021-11-17')
scores_dax_lastweek = matrix(nrow=5, ncol=1, 0)
rownames(scores_dax_lastweek) = c('current: garch mixture (3 history sizes)', 'simple garch', 'quantreg hist 150', 
                                'quantreg hist 900', 'NEW: combination of best garch and best quantreg')
scores_dax_lastweek[1] = evaluate_model_dax(dax_ugarch_combined,garchorder=c(6,6), history_sizes = c(245,800,1029),
                                          debug=TRUE, init_dates = init_dates)
scores_dax_lastweek[2] = evaluate_model_dax(dax_ugarch,garchorder=c(6,6), init_dates = init_dates)
scores_dax_lastweek[3] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=150,init_dates = init_dates)
scores_dax_lastweek[4] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=900,init_dates = init_dates)
scores_dax_lastweek[5] = evaluate_model_dax(dax_garch_quantreg_comb,init_dates = init_dates)
scores_dax_lastweek
# Here you see, that quantile regresion was way better because of larger confidence intervals
# Comparison: Scores in first three weeks, that were less volatile
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10')
scores_dax_first3weeks = matrix(nrow=5, ncol=1, 0)
rownames(scores_dax_first3weeks) = c('current: garch mixture (3 history sizes)', 'simple garch', 'quantreg hist 150', 
                                  'quantreg hist 900', 'NEW: combination of best garch and best quantreg')
scores_dax_first3weeks[1] = evaluate_model_dax(dax_ugarch_combined,garchorder=c(6,6), history_sizes = c(245,800,1029),
                                            debug=TRUE, init_dates = init_dates)
scores_dax_first3weeks[2] = evaluate_model_dax(dax_ugarch,garchorder=c(6,6), init_dates = init_dates)
scores_dax_first3weeks[3] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=150,init_dates = init_dates)
scores_dax_first3weeks[4] = evaluate_model_dax(dax_quantreg,transpose=TRUE,rolling_window=900,init_dates = init_dates)
scores_dax_first3weeks[5] = evaluate_model_dax(dax_garch_quantreg_comb,init_dates = init_dates)
scores_dax_first3weeks
# This is why i chose the mix hist_sizes model. But it seems to be overfitted on weeks like this (they were pretty similar)
# Over all four weeks, the combination of simple garch and quantreg is best. Lets try that!
save(scores_dax_4weeks, scores_dax_lastweek, scores_dax_first3weeks, file = "evaluation_dax_models_week_6.RData")


# WEEK 6: QuantGarch Hyperparametertuning ---------------------------------


source("src/model_dax.R")
init_dates = c('2020-04-02', '2019-08-16', '2005-01-14', '2012-12-07', '2008-03-07', '2006-06-15', 
               '2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')
#init_dates = c('2021-11-03')
grid_garch = c(150, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500)
grid_qr = c(150, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300)#, 1400, 1500)
#grid = c(150, 900)
hyperparam_scores = matrix(NA, nrow = length(grid_qr), ncol = length(grid_garch))
rownames(hyperparam_scores) = paste0('garch',grid_qr)
colnames(hyperparam_scores) = paste0('quant',grid_garch)
#hyperparam_scores
# Take GARCH order of 6 as taken
# Optimize over history sizes for garch and quantreg
for (garch_num in 1:length(grid_garch)){
  garchsize = grid_garch[garch_num]
  for (quant_num in 1:length(grid_qr)){
    quantsize = grid_qr[quant_num]
    hyperparam_scores[garch_num, quant_num] = evaluate_model_dax(dax_quantgarch, init_dates = init_dates, history_size = garchsize, 
                                                                 rolling_window = quantsize)
    print(paste0((100*((garch_num-1)*length(grid_garch)+quant_num) )/(length(grid_qr)*length(grid_garch)),' % done'))
  }
}
hyperparam_scores
min(hyperparam_scores)

x = paste0(grid_garch,'') # if taken as numerical values plot gets ugly, so trick the system by making it strings
y = paste0(grid_qr,'')
df_grid = expand.grid(X=x,Y=y)
df_grid

scores = hyperparam_scores # rename for plot
rownames(scores) = paste0(grid_garch,'')
colnames(scores) = paste0(grid_qr,'')
ggplot(df_grid, aes(X,Y, fill=scores)) + geom_tile() + scale_fill_gradient(low="black", high="bisque") +
  labs(title = 'scores hyperparameter grid search') + xlab('rolling window GARCH(6,6)') + ylab('rolling window quantile regression')

x_zoom = paste0(grid_garch[c(5:9,11:15)],'')
df_grid_zoom = expand.grid(X=x_zoom,Y=y)

scores = hyperparam_scores[c(5:9,11:15),]
ggplot(df_grid_zoom, aes(X,Y, fill=scores)) + geom_tile() + scale_fill_gradient(low="black", high="bisque")+
  labs(title = 'scores hyperparameter grid search (zoomed in)') + xlab('rolling window GARCH(6,6)') + 
  ylab('rolling window quantile regression')

# Comparison: NOT ON THE SAME TEST DATA! SHOULDN'T DO THAT! SEE WEEK 9 MODEL SCORES!
#init_dates = c('2020-04-02', '2019-08-16', '2005-01-14', '2012-12-07', '2008-03-07', '2006-06-15')
#scores_compare = matrix(NA, nrow=4, ncol=1)
#rownames(scores_compare) = c('hypertuned quantgarch model', 'base garch', 'garch mixture', 'base quantreg')
#scores_compare[1] = min(hyperparam_scores)
#scores_compare[2] = evaluate_model_dax(dax_ugarch, init_dates = init_dates, garchorder=c(6,6), history_size=1400)
#scores_compare[3] = evaluate_model_dax(dax_ugarch_combined, init_dates = init_dates, garchorder=c(6,6), history_sizes=c(243,800,1030))
#scores_compare[4] = evaluate_model_dax(dax_quantreg, quantreg=TRUE, init_dates = init_dates, rolling_window = 800)
#scores_compare

# Compare simple GARCH and quantgarch for last week 
#evaluate_model_dax(dax_ugarch, init_dates = c('2021-11-17'), garchorder=c(6,6), history_size=800)
#evaluate_model_dax(dax_quantgarch, init_dates = c('2021-11-17'), history_size = 800, 
#                   rolling_window = 800)

save(hyperparam_scores, hyperparam_scores_old, file = "graphics and tables for elaboration/DAX/gridsearch_quantgarch_scores.RData")


# WEEK 6: Test Support Vector Regression ----------------------------------


library('e1071')
init_date = '2021-11-10'
data = get_dax_data(init_date)
test_data = get_dax_data(as.Date(init_date)+7)
test_data = subset(test_data, as.Date(Date) > init_date)
model1 = svm(ret1 ~ Date, data, type='eps-regression', kernel='radial', cost=0.01, epsilon=0.01, probability=TRUE)
names(model1)
plot(predict(model1))
predict(model1, test_data)
model5 = svm(ret5 ~ Date, data)
plot(predict(model5, data))



# WEEK 6: Optimal weights quantgarch? -------------------------------------


set.seed(999)
init_dates = sample(seq(as.Date('1995/01/01'), as.Date('2021/11/17'), by="day"), 20)
weight_scores = matrix(NA, nrow = 11, ncol = 1)
weights = (0:10)/10
rownames(weight_scores) = weights
for (n_weight in 1:length(weights)){
  weight = weights[n_weight]
  weight_scores[n_weight] = evaluate_model_dax(dax_quantgarch, init_dates = init_dates, history_size = 1400, rolling_window = 800, 
                                               weight_garch = weight)
  print(paste0(n_weight*9,'% done'))
}
weight_scores


# WEEK 6: Optimal weights temp? -------------------------------------------


source('src/model_temp.R')
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')
weight_scores_temp_boost = matrix(NA, nrow = 11, ncol = 1)
weights = (0:10)/10
rownames(weight_scores_temp_boost) = weights
for (n_weight in 1:length(weights)){
  weight_emos = weights[n_weight]
  weight_scores_temp_boost[n_weight] = evaluate_model_weather(temp_emos_multi_boosting_mixture, init_dates = init_dates, 
                                                   weights = c(weight_emos, 1-weight_emos), variable='air_temperature')
  print(paste0(n_weight*9,'% done'))
}
weight_scores_temp_boost
save(weight_scores_temp_boost, file='opt_weights_temp_boost_mixture.RData')


# WEEK 7: quantregForests wind --------------------------------------------


library(quantregForest)
library(moments)
source("src/model_wind.R")
# First: just for lead time 36
# Feature Engineering
df = get_hist_wind_data() %>% na.omit
qrf_feature_eng_train = function(df, lt){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_sd = sqrt(df_lt$ens_var)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[7:46], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[7:46], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[7:46], 1, kurtosis, na.rm=T)
  df_lt$mon = month(df_lt$obs_tm)
  df_pred = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon)
  return(df_pred)
}
df_pred_train = qrf_feature_eng_train(df, lt=36)
df_obs_train = subset(df, fcst_hour == 36)$obs
head(df_pred_train)
head(df_obs_train)
# Model
qrf = quantregForest(df_pred_train, df_obs_train, nthreads = 4)
names(qrf)
importance(qrf)
# Predict
qrf_feature_eng_predict = function(df, lt, init_date){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_sd = apply(df_lt[3:42], 1, sd, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[3:42], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[3:42], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[3:42], 1, kurtosis, na.rm=T)
  df_lt$mon = month(init_date)
  df_pred = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez09, iqr, skew, kurt, mon)
  return(df_pred)
}
df_test = get_current_wind_data('2021-11-03')
df_pred_test = qrf_feature_eng_predict(df_test, lt = 36, '2021-11-03')
head(df_pred_test)
predict(qrf, newdata = df_pred_test, what = c(0.025,0.25,0.5,0.75,0.975))
# Now i have added the function wind_qrf to model_wind.R based on this work, so lets test it
# Also added the feature engineering functions on better fashion in toolkit:
source("src/toolkit.R")
fc = wind_qrf('2021-10-27')
fc
plot_forecasts_weather('2021-10-27', fc, history_size=14, ylim=c(0,40),
                       model_name="QRF Test", 'wind')
score = evaluate_model_weather(wind_qrf, 'wind', init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24'))
score
#load("C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/EMOS_boosting_scores.RData")
#scores_wind


init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24')
scores_wind = matrix(nrow=6, ncol=1, 0)
rownames(scores_wind) = c('Multi EMOS', '+ Boosting', 'QRF', 'QRF Var1', 'QRF Var2', 'QRF Var 3')
scores_wind[1] = evaluate_model_weather(wind_emos_tl_multi,'wind',init_dates=init_dates)
scores_wind[2] = evaluate_model_weather(wind_emos_tl_multi_boosting,'wind',init_dates=init_dates)
scores_runs_3 = matrix(NA,5,1)
scores_runs_4 = matrix(NA,5,1)
scores_runs_5 = matrix(NA,5,1)
scores_runs_6 = matrix(NA,5,1)
for(run in 1:5){
  scores_runs_3[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates)
  scores_runs_4[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates, ntree=2000)
  scores_runs_5[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates, nodesize=3)
  scores_runs_6[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates, ntree=2000, nodesize=3)
}
scores_wind[3] = mean(scores_runs_3)
scores_wind[4] = mean(scores_runs_4)
scores_wind[5] = mean(scores_runs_5)
scores_wind[6] = mean(scores_runs_6)
scores_wind

# Check if combination of boosting and QRF makes the model better
boosting_qrf_comb = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), weights=c(0.5,0.5)){
  fc1 = wind_emos_tl_multi_boosting(init_date, quantile_levels)
  fc2 = wind_qrf(init_date, quantile_levels)
  return(combine_forecasts(fc1, fc2, weights))
}
test_score = evaluate_model_weather(boosting_qrf_comb,'wind',init_dates=init_dates)
test_score
#test_score = evaluate_model_weather(boosting_qrf_comb,'wind',init_dates=init_dates, weights=c(0.8,0.2))
#test_score
save(scores_wind, test_score, file='graphics and tables for elaboration/weather/qrf_wind_base_scores.RData')


# WEEK 7: QRF with additional variables -----------------------------------


# Feature Engineering
df = get_hist_wind_data() %>% na.omit
df_training = qrf_feature_eng_train(df, lt=36, addmslp=TRUE, addrad=TRUE, addclct=TRUE)
head(df_training)
# Seperate observations from predictors
df_training_target = df_training[,10]
df_training_predictors = df_training[,-10]
head(df_training_predictors)
# Ready to train
qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4)
# Feature Engineering for new predictors
init_date = '2021-11-03'
df = get_current_wind_data(init_date)[,-1]
df_new_predictors = qrf_feature_eng_predict(df, 36, init_date, addmslp=TRUE, addclct=TRUE, addrad=TRUE)
# Predict
predict(qrf, newdata = df_new_predictors, what = c(0.025,0.25,0.5,0.75,0.975))
# Updated wind_qrf, test it
wind_qrf('2021-11-03')
wind_qrf('2021-11-03',addclct=TRUE,addmslp=TRUE,addrad=TRUE)
# Now test it
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24')
scores_wind = matrix(nrow=11, ncol=1, 0)
rownames(scores_wind) = c('Base EMOS', 'Multiple EMOS', '+ Boosting', 'Base QRF', '+ mslp', 
                          '+ rad', '+ clct', '++ rad, clct', '++ rad, mslp', '++ clct, mslp', '+++ all')
# deterministic models
scores_wind[1] = evaluate_model_weather(wind_emos_tl,'wind',init_dates=init_dates)
scores_wind[2] = evaluate_model_weather(wind_emos_tl_multi,'wind',init_dates=init_dates)
scores_wind[3] = evaluate_model_weather(wind_emos_tl_multi_boosting,'wind',init_dates=init_dates)
# qrf has randomness in prediction process, so use mean of 10 runs for each model
for (model in 4:11){
  scores_runs = matrix(nrow = 1, ncol = 5, NA)
  for (run in 1:5){
    print(paste0('model ',model,' run ',run))
    if (model==4){
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates)
    } else if (model==5) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addmslp=TRUE)
    } else if (model==6) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addrad=TRUE)
    } else if (model==7) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addclct=TRUE)
    } else if (model==8) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addclct=TRUE,addrad=TRUE)
    } else if (model==9) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addmslp=TRUE,addrad=TRUE)
    } else if (model==10) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addmslp=TRUE,addclct=TRUE)
    } else if (model==11) {
      scores_runs[run] = evaluate_model_weather(wind_qrf,'wind',init_dates=init_dates,addmslp=TRUE,addclct=TRUE,addrad=TRUE)
    }
  }
  scores_wind[model] = mean(scores_runs)
  print(scores_wind)
}
scores_wind
save(scores_wind, file='graphics and tables for elaboration/weather/qrf_wind_scores_additional_regressors.RData')

# WEEK 7: QRF for temp ----------------------------------------------------


library('quantregForest')
library('moments')
source('src/model_temp.R')
# I directly implemented qrf for temp, as i wrote the functions for wind qrf in a way i can easily reuse them for temp
temp_qrf('2021-11-03')
temp_emos_multi_boosting_mixture('2021-11-03', weights=c(0.8,0.2)) 
# Now test it
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24')
scores_temp = matrix(nrow=11, ncol=1, 0)
rownames(scores_temp) = c('Base EMOS', 'Multiple EMOS', '+ Boosting (Mixture)', 'Base QRF', '+ mslp', 
                          '+ rad', '+ clct', '++ rad, clct', '++ rad, mslp', '++ clct, mslp', '+++ all')
# deterministic models
scores_temp[1] = evaluate_model_weather(temp_emos,'air_temperature',init_dates=init_dates)
scores_temp[2] = evaluate_model_weather(temp_emos_multi,'air_temperature',init_dates=init_dates)
scores_temp[3] = evaluate_model_weather(temp_emos_multi_boosting_mixture,'air_temperature',init_dates=init_dates)
# qrf has randomness in prediction process, so use mean of 10 runs for each model
for (model in 4:11){
  scores_runs = matrix(nrow = 1, ncol = 5, NA)
  for (run in 1:5){
    if (model==4){
      print(paste0('model ',model,' run ',run))
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates)
    } else if (model==5) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addmslp=TRUE)
    } else if (model==6) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addrad=TRUE)
    } else if (model==7) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addclct=TRUE)
    } else if (model==8) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addclct=TRUE,addrad=TRUE)
    } else if (model==9) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addmslp=TRUE,addrad=TRUE)
    } else if (model==10) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addmslp=TRUE,addclct=TRUE)
    } else if (model==11) {
      scores_runs[run] = evaluate_model_weather(temp_qrf,'air_temperature',init_dates=init_dates,addmslp=TRUE,addclct=TRUE,addrad=TRUE)
    }
  }
  scores_temp[model] = mean(scores_runs)
  print(scores_temp)
}
scores_temp

save(scores_wind_old, scores_temp_old, scores_wind, scores_temp, file='qrf_scores.RData')

load('C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/qrf_scores.RData')
scores_temp
scores_wind
scores_temp_old
scores_wind_old

#load('C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/EMOS_boosting_scores.RData')
#scores_wind


# WEEK 8: QRF WEATHER -----------------------------------------------------


## FIRST: TEMP
# Get variable importance for temp model
df = get_hist_temp_data() %>% na.omit
qrf_feature_eng_train_many = function(df, lt){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_sd = sqrt(df_lt$ens_var)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez03 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.3)
  df_lt$dez07 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.7)
  df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[7:46], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[7:46], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[7:46], 1, kurtosis, na.rm=T)
  df_lt$mon = month(df_lt$obs_tm)
  df_pred = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez03, dez07, dez09, iqr, skew, kurt, mon, obs)
  return(df_pred)
}
importances_temp = array(0, dim=c(11,2,5))
dimnames(importances_temp)[[3]] = c("36h", "48h", "60h", "72h", "84h")
index = 1
for (lead_time in c(36,48,60,72,84)){
  df_features = qrf_feature_eng_train_many(df, lt=lead_time)
  df_pred_train = df_features[,-12]
  df_obs_train = df_features[,12]
  qrf = quantregForest(df_pred_train, df_obs_train, nthreads = 4)
  importances_temp[,1,index] = rownames(qrf$importance)
  importances_temp[,2,index] = qrf$importance
  index = index + 1
}
importances_temp
# So it seems like everything of higher order than 1, e.g. ens_sd, iqr, skew and kurt, dont matter that much. 
# So lets test with and without them
qrf_feature_eng_train_less = function(df, lt){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez03 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.3)
  df_lt$dez07 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.7)
  df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$mon = month(df_lt$obs_tm)
  df_pred = select(df_lt, ens_mean, ens_med, dez01, dez03, dez07, dez09, mon, obs)
  return(df_pred)
}
importances_temp_less = array(0, dim=c(7,2,5))
dimnames(importances_temp_less)[[3]] = c("36h", "48h", "60h", "72h", "84h")
index = 1
for (lead_time in c(36,48,60,72,84)){
  df_features = qrf_feature_eng_train_less(df, lt=lead_time)
  df_pred_train = df_features[,-8]
  df_obs_train = df_features[,8]
  qrf = quantregForest(df_pred_train, df_obs_train, nthreads = 4)
  importances_temp_less[,1,index] = rownames(qrf$importance)
  importances_temp_less[,2,index] = qrf$importance
  index = index + 1
}
importances_temp_less
# mon isnt important now too, but lets include it anyways (fow now)

## TEST QRF FUNCTION LESS VARS
qrf_feature_eng_predict_less = function(df, lt, init_date){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez03 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.3)
  df_lt$dez07 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.7)
  df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$mon = month(as.Date(init_date)+floor(lt/24))
  df_working = select(df_lt, ens_mean, ens_med, dez01, dez03, dez07, dez09, mon)
  return(df_working)
}
temp_qrf_test_less = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
  df = get_hist_temp_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train_less(df=df, lt=lead_time)
    df_training_target = df_training[,8]
    df_training_predictors = df_training[,-8]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_temp_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_less(df_new, lead_time, init_date=init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
# Working:
temp_qrf_test_less('2021-11-03')
## TEST QRF FUNCTION MANY VARS
qrf_feature_eng_predict_many = function(df, lt, init_date){
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_sd = apply(df_lt[3:42], 1, sd, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  df_lt$dez03 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.3)
  df_lt$dez07 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.7)
  df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  df_lt$iqr = apply(df_lt[3:42], 1, IQR, na.rm=T)
  df_lt$skew = apply(df_lt[3:42], 1, skewness, na.rm=T)
  df_lt$kurt = apply(df_lt[3:42], 1, kurtosis, na.rm=T)
  df_lt$mon = month(as.Date(init_date)+floor(lt/24))
  df_working = select(df_lt, ens_mean, ens_med, ens_sd, dez01, dez03, dez07, dez09, iqr, skew, kurt, mon)
  return(df_working)
}
temp_qrf_test_many = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
  df = get_hist_temp_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train_many(df=df, lt=lead_time)
    df_training_target = df_training[,12]
    df_training_predictors = df_training[,-12]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_temp_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_many(df_new, lead_time, init_date=init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
# Working:
temp_qrf_test_many('2021-11-10')

## Score these two models 
scores_temp_additional = matrix(NA, 2, 1)
rownames(scores_temp_additional) = c('QRF less vars', 'QRF many vars')
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24')
for(model in 1:2){
  scores_runs = matrix(NA, 5, 1)
  for(run in 1:5){
    print(paste0('model ',model,' run ',run))
    if(model==1){
      scores_runs[run] = evaluate_model_weather(temp_qrf_test_less,'air_temperature',init_dates=init_dates)
    }
    if(model==2){
      scores_runs[run] = evaluate_model_weather(temp_qrf_test_many,'air_temperature',init_dates=init_dates)
    }
  }
  scores_temp_additional[model] = mean(scores_runs)
}
scores_temp_additional

# Get scores of old models
load('C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/qrf_scores.RData')
scores_temp

# So even if the variables seem not to be that important, models including them score better
# Because of the random sampling in Random Forests, too many variables seem to not hurt that mutch

## NOW: SAME FOR WIND
source('src/model_wind.R')

wind_qrf_test_many = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
  df = get_hist_wind_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train_many(df=df, lt=lead_time)
    df_training_target = df_training[,12]
    df_training_predictors = df_training[,-12]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_wind_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_many(df_new, lead_time, init_date=init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
wind_qrf_test_less = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
  df = get_hist_wind_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train_less(df=df, lt=lead_time)
    df_training_target = df_training[,8]
    df_training_predictors = df_training[,-8]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_wind_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_less(df_new, lead_time, init_date=init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}
scores_wind_additional = matrix(NA, 2, 1)
rownames(scores_wind_additional) = c('QRF less vars', 'QRF many vars')
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24')
for(model in 1:2){
  scores_runs = matrix(NA, 5, 1)
  for(run in 1:5){
    print(paste0('model ',model,' run ',run))
    if(model==1){
      scores_runs[run] = evaluate_model_weather(wind_qrf_test_less,'wind',init_dates=init_dates)
    }
    if(model==2){
      scores_runs[run] = evaluate_model_weather(wind_qrf_test_many,'wind',init_dates=init_dates)
    }
  }
  scores_wind_additional[model] = mean(scores_runs)
}
scores_wind_additional
scores_wind

# And assess variable importance:
df = get_hist_wind_data() %>% na.omit
importances_wind = array(0, dim=c(11,2,5))
dimnames(importances_wind)[[3]] = c("36h", "48h", "60h", "72h", "84h")
index = 1
for (lead_time in c(36,48,60,72,84)){
  df_features = qrf_feature_eng_train_many(df, lt=lead_time)
  df_pred_train = df_features[,-12]
  df_obs_train = df_features[,12]
  qrf = quantregForest(df_pred_train, df_obs_train, nthreads = 4)
  importances_wind[,1,index] = rownames(qrf$importance)
  importances_wind[,2,index] = qrf$importance
  index = index + 1
}
importances_wind

save(scores_wind, scores_temp, scores_wind_additional, scores_temp_additional, 
     importances_temp, importances_temp_less, importances_wind,
     file='C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/qrf_scores_more.RData')

#load(file='C:/dev/Forecasting_Challenge/graphics and tables for elaboration/weather/qrf_scores_more.RData')


# WEEK 9: Feature Engineering DAX QRF -------------------------------------


library('TTR')
source('src/model_dax.R')
#testdata = get_dax_data_directly('2021-12-18') %>% na.omit
data = getSymbols('^GDAXI',src='yahoo', from = as.Date('2021-12-18')-1000, to = as.Date('2021-12-18')+1, auto.assign=FALSE)
data$RSI = RSI(data$GDAXI.Adjusted)
data$Stoch_Oscill = stoch(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
data$MACD = MACD(data$GDAXI.Adjusted)
data$ROC = ROC(data$GDAXI.Adjusted)
data$WPR = WPR(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
data$CCI = CCI(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
data$ADX = ADX(data[,c("GDAXI.High","GDAXI.Low","GDAXI.Close")])
data$OBV = OBV(data$GDAXI.Adjusted, data$GDAXI.Volume)
data$MA200 = SMA(data$GDAXI.Adjusted, n=200)
data$ret1 = compute_return(matrix(data$GDAXI.Adjusted), h = 1)
data$ret2 = compute_return(matrix(data$GDAXI.Adjusted), h = 2)
data$ret3 = compute_return(matrix(data$GDAXI.Adjusted), h = 3)
data$ret4 = compute_return(matrix(data$GDAXI.Adjusted), h = 4)
data$ret5 = compute_return(matrix(data$GDAXI.Adjusted), h = 5)
data = data[,c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200", 
               "ret1", "ret2", "ret3", "ret4", "ret5")] %>% na.omit
head(data)
dim(data)

# TRAIN-TEST-SPLIT!
train_data = data[1:400,]
test_data = data[401:495]

# QRF for horizon of 1 day
df_predict_train = train_data[,c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200")]
df_predict_train = df_predict_train[-dim(df_predict_train)[1],] # Leave out last predictors
df_obs_train = train_data[,c("ret1")]
df_obs_train = df_obs_train[-1,] # Leave out first obs -> Predict ret1 based on variables measured a day before
qrf = quantregForest(df_predict_train, df_obs_train, nthreads = 4)
qrf$importance

# Test
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)
df_predict_test = test_data[,c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200")]
df_predict_test = df_predict_test[-dim(df_predict_test)[1],] # Leave out last predictors
df_obs_test = test_data[,c("ret1")]
df_obs_test = df_obs_test[-1,] # Leave out first obs -> Predict ret1 based on variables measured a day before
forecasts = predict(qrf, newdata = df_predict_test, what = quantile_levels)
tail(forecasts)

# Evaluate
scores = matrix(NA, length(df_obs_test), 1)
rownames(scores) = index(df_obs_test)
scores
for(i in 1:length(scores)){
  quantile_scores = matrix(NA, 5, 1)
  for(q_num in 1:5){
    quantile_scores[q_num] = quantile_score(quantile_levels[q_num], forecasts[i, q_num], df_obs_test[i])
  }
  scores[i] = mean(quantile_scores)
}
scores
mean(scores)

# Compare to different models (they do it for all horizons..)
load('graphics and tables for elaboration/DAX/evaluation_dax_models_week_6.RData')
mean(scores)
scores_dax_4weeks
# QRF outperforms all of them!
# But lets compare it to 5-day forecast of QRF to validate this

# QRF for horizon of 5 days
df_predict_train = train_data[,c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200")]
df_predict_train = df_predict_train[-((dim(df_predict_train)[1]-4):dim(df_predict_train)[1]),] # Leave out last 5 predictors
df_obs_train = train_data[,c("ret5")]
df_obs_train = df_obs_train[-(1:5),] # Leave out first 5 obs -> Predict ret5 based on variables measured 5 day before
qrf = quantregForest(df_predict_train, df_obs_train, nthreads = 4)
qrf$importance

# Test
quantile_levels = c(0.025,0.25,0.5,0.75,0.975)
df_predict_test = test_data[,c("RSI", "Stoch_Oscill", "MACD", "ROC", "WPR", "CCI", "ADX", "OBV", "MA200")]
df_predict_test = df_predict_test[-((dim(df_predict_train)[1]-4):dim(df_predict_train)[1]),] # Leave out last predictors
df_obs_test = test_data[,c("ret5")]
df_obs_test = df_obs_test[-(1:5),] # Leave out first obs -> Predict ret1 based on variables measured a day before
forecasts = predict(qrf, newdata = df_predict_test, what = quantile_levels)
tail(forecasts)

# Evaluate
scores_5d = matrix(NA, length(df_obs_test), 1)
rownames(scores_5d) = index(df_obs_test)
scores_5d
for(i in 1:length(scores_5d)){
  quantile_scores = matrix(NA, 5, 1)
  for(q_num in 1:5){
    quantile_scores[q_num] = quantile_score(quantile_levels[q_num], forecasts[i, q_num], df_obs_test[i])
  }
  scores_5d[i] = mean(quantile_scores)
}
scores_5d
mean(scores_5d)
# Ok, so lets implement the model and evaluate it afterwards!
# Implemented:
dax_qrf('2021-11-03')
dax_quantgarch('2021-11-03')


# WEEK 9: Evaluate QRF DAX ------------------------------------------------


source('src/model_dax.R')
init_dates = c('2020-04-02', '2019-08-16', '2005-01-14', '2012-12-07', '2008-03-07', '2006-06-15', 
                            '2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')

# QRF Base
qrf_scores_per_horizon = evaluate_model_dax(dax_qrf, init_dates=init_dates, per_horizon=TRUE)
qrf_scores_per_horizon

# QRF including DAX Futures
qrf_futures_scores_per_horizon = evaluate_model_dax(dax_qrf, init_dates=init_dates, per_horizon=TRUE, add_futures=TRUE)
qrf_futures_scores_per_horizon

# QuantGARCH
quantgarch_scores_per_horizon = evaluate_model_dax(dax_quantgarch, init_dates=init_dates, per_horizon=TRUE)
quantgarch_scores_per_horizon

# GARCH using three history_sizes
garch_scores_per_horizon = evaluate_model_dax(dax_ugarch_combined, init_dates=init_dates, per_horizon=TRUE)
garch_scores_per_horizon

# Quantreg
quantreg_scores_per_horizon = evaluate_model_dax(dax_quantreg, init_dates=init_dates, per_horizon=TRUE, quantreg=TRUE)
quantreg_scores_per_horizon

# Baseline
baseline_scores_per_horizon = evaluate_model_dax(dax_baseline, init_dates=init_dates, per_horizon=TRUE)
baseline_scores_per_horizon

# QRF including DAX Futures and MSCI World (ETF)
qrf_futures_msci_scores_per_horizon = evaluate_model_dax(dax_qrf, init_dates=init_dates, 
                                                         per_horizon=TRUE, add_futures=TRUE, add_msci=TRUE)
qrf_futures_msci_scores_per_horizon

# QRF including DAX Futures and UCITS (DAX ETF)
qrf_futures_ucits_scores_per_horizon = evaluate_model_dax(dax_qrf, init_dates=init_dates, 
                                                          per_horizon=TRUE, add_futures=TRUE, add_ucits=TRUE)
qrf_futures_ucits_scores_per_horizon

# QRF including DAX Futures and Dow Jones Futures
qrf_futures_usfutures_scores_per_horizon = evaluate_model_dax(dax_qrf, init_dates=init_dates, 
                                                          per_horizon=TRUE, add_futures=TRUE, add_us_futures=TRUE)
qrf_futures_usfutures_scores_per_horizon

# QRF and GARCH mixture
qrfgarch_scores_per_horizon = evaluate_model_dax(dax_qrfgarch, init_dates=init_dates, per_horizon=TRUE, add_futures=TRUE)
qrfgarch_scores_per_horizon

# Aggregate in table
model_scores = cbind(qrf_scores_per_horizon, qrf_futures_scores_per_horizon, quantgarch_scores_per_horizon, 
                     garch_scores_per_horizon, quantreg_scores_per_horizon, baseline_scores_per_horizon, 
                     qrf_futures_msci_scores_per_horizon, qrf_futures_ucits_scores_per_horizon, 
                     qrf_futures_usfutures_scores_per_horizon)
colnames(model_scores) = c("QRF Base", "QRF including DAX Futures", "QuantGARCH", "GARCH three history_sizes", "Quantile Regression",
                           "Baseline", "QRF including DAX Futures and MSCI World", "QRF including DAX Futures and UCITS", 
                           "QRF including DAX Futures and Dow Jones Futures", "QRFGarch")
model_scores

# So: QRFGarch seems to be slightly better than QuantGARCH. Next section: Optimal weighting?


# WEEK 9: Optimal weights QRFGARCH ----------------------------------------


source('src/model_dax.R')
init_dates = c('2020-04-02', '2019-08-16', '2005-01-14', '2012-12-07', '2008-03-07', '2006-06-15', 
               '2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17')

scores_weights_1200 = matrix(NA, nrow=6, ncol=11)
rownames(scores_weights_1200) = c('Overall','1 day','2 days','3 days', '6 days', '7 days')
weights_garch = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
colnames(scores_weights_1200) = weights_garch
for(weight_num in 1:11){
  print(paste0('Iteration: ', weight_num, ' of 11'))
  weight_garch = weights_garch[weight_num]
  scores_runs = matrix(NA, nrow=6, ncol=5)
  for(run in 1:5){
    print(paste0('Run: ', run, ' of 5'))
    scores_runs[,run] = evaluate_model_dax(dax_qrfgarch, init_dates=init_dates, history_size=1200,
                                           per_horizon=TRUE, add_futures=TRUE, weight_garch=weight_garch)
    print(paste0(((weight_num-1)*5+run)/55*100, "% done"))
  }
  scores_weights_1200[,weight_num] = apply(scores_runs, 1, mean, na.rm=TRUE)
}
scores_weights_1200
scores_weights

# So: Weighting GARCH with 0.9 and QRF with 0.1 yields the best results!

save(model_scores, scores_weights, scores_weights_1200, file='graphics and tables for elaboration/DAX/week9_modelscores.RData')
#load('graphics and tables for elaboration/DAX/week9_modelscores.RData')


# WEEK 10: Score all weather models ---------------------------------------


# WIND
source('src/model_wind.R')
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24', '2021-12-01', '2021-12-08', '2021-12-15')

# Baselines
wind_scores_baseline = evaluate_model_weather(wind_baseline, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_scores_baseline

# EMOS TN
wind_scores_emos_tn = evaluate_model_weather(wind_emos_tn, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_scores_emos_tn

# EMOS TL
wind_scores_emos_tl = evaluate_model_weather(wind_emos_tl, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_scores_emos_tl

# EMOS TL Multivariate
wind_scores_emos_tl_multi = evaluate_model_weather(wind_emos_tl_multi, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_scores_emos_tl_multi

# EMOS TL Multivariate + Boosting
wind_scores_emos_tl_multi_boosting = evaluate_model_weather(wind_emos_tl_multi_boosting, 'wind', 
                                                            init_dates=init_dates, per_horizon=TRUE)
wind_scores_emos_tl_multi_boosting


# QRF
wind_scores_qrf = evaluate_model_weather(wind_qrf, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_scores_qrf

# QRF + CLCT
wind_scores_qrf_clct = evaluate_model_weather(wind_qrf, 'wind', init_dates=init_dates, per_horizon=TRUE, addclct=TRUE)
wind_scores_qrf_clct

wind_model_scores = cbind(wind_scores_baseline, wind_scores_emos_tn, wind_scores_emos_tl, 
                          wind_scores_emos_tl_multi, wind_scores_emos_tl_multi_boosting, wind_scores_qrf, 
                          wind_scores_qrf_clct)
colnames(wind_model_scores) = c("Baseline", "EMOS TN", "EMOS TL", "EMOS TL Multi", "EMOS TL Multi Boosting", 
                                "QRF", "QRF + CLCT")
wind_model_scores

# TEMP
source('src/model_temp.R')

# Baselines
temp_scores_baseline = evaluate_model_weather(temp_baseline, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)
temp_scores_baseline

# EMOS
temp_scores_emos = evaluate_model_weather(temp_emos, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)
temp_scores_emos

# EMOS Multivariate
temp_scores_emos_multi = evaluate_model_weather(temp_emos_multi, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)
temp_scores_emos_multi

# EMOS Multivariate + Boosting
temp_scores_emos_multi_boosting = evaluate_model_weather(temp_emos_multi_boosting, 'air_temperature', 
                                                            init_dates=init_dates, per_horizon=TRUE)
temp_scores_emos_multi_boosting

# EMOS Multivariate + Boosting MIXTURE
temp_scores_emos_multi_boosting_mix = evaluate_model_weather(temp_emos_multi_boosting_mixture, 'air_temperature', 
                                              init_dates=init_dates, per_horizon=TRUE)
temp_scores_emos_multi_boosting_mix

# QRF
temp_scores_qrf = evaluate_model_weather(temp_qrf, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)
temp_scores_qrf

temp_model_scores = cbind(temp_scores_baseline, temp_scores_emos, temp_scores_emos_multi, 
                          temp_scores_emos_multi_boosting, temp_scores_emos_multi_boosting_mix, temp_scores_qrf)
colnames(temp_model_scores) = c("Baseline", "EMOS", "EMOS Multi", "EMOS Multi Boosting", 
                                "EMOS Multi Boost Mix", "QRF")
temp_model_scores

save(temp_model_scores, wind_model_scores, file="graphics and tables for elaboration/weather/week10_modelscores.RData")
#load("graphics and tables for elaboration/weather/week10_modelscores.RData")


# WEEK 10: Test Weather QRF with less predictors --------------------------


init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24', '2021-12-01', '2021-12-08', '2021-12-15')
#load("graphics and tables for elaboration/weather/week10_modelscores.RData")

qrf_feature_eng_train_TEST = function(df, lt){
  #' Function that makes feature engineering for weather FALSEile regression forests training, returns ensemble statistics used for qrf
  #' It gets summary statistics for target variable and adds simple summary statistics for additional regressors, if wanted
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  
  # First do summary statistics for target variable
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_sd = sqrt(df_lt$ens_var)
  df_lt$ens_med = apply(df_lt[7:46], 1, median, na.rm=T)
  #df_lt$dez01 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.1)
  #df_lt$dez09 = apply(df_lt[7:46], 1, quantile, na.rm=T, probs= 0.9)
  #df_lt$iqr = apply(df_lt[7:46], 1, IQR, na.rm=T)
  #df_lt$skew = apply(df_lt[7:46], 1, skewness, na.rm=T)
  #df_lt$kurt = apply(df_lt[7:46], 1, kurtosis, na.rm=T)
  #df_lt$mon = month(df_lt$obs_tm)
  df_working = select(df_lt, obs_tm, ens_mean, ens_med, ens_sd, obs)#, dez01, dez09, iqr, skew, kurt, mon)
  # now add summary statistics for additional variables that should be included
  # Omit obs_tm, just needed for matching the rows
  df_pred = df_working[,-1]
  return(df_pred)
}

qrf_feature_eng_predict_TEST = function(df, lt, init_date){
  #' Function that makes feature engineering for weather quantile regression forests predictions.
  #' df: data frame containing the raw training data
  #' lt: lead time we are currently training
  #' init_date: date on which the prediction is to be made
  
  # First do summary statistics for target variable
  df_lt = subset(df, fcst_hour == lt)
  df_lt$ens_mean = apply(df_lt[3:42], 1, mean, na.rm=T)
  df_lt$ens_sd = apply(df_lt[3:42], 1, sd, na.rm=T)
  df_lt$ens_med = apply(df_lt[3:42], 1, median, na.rm=T)
  #df_lt$dez01 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.1)
  #df_lt$dez09 = apply(df_lt[3:42], 1, quantile, na.rm=T, probs= 0.9)
  #df_lt$iqr = apply(df_lt[3:42], 1, IQR, na.rm=T)
  #df_lt$skew = apply(df_lt[3:42], 1, skewness, na.rm=T)
  #df_lt$kurt = apply(df_lt[3:42], 1, kurtosis, na.rm=T)
  #df_lt$mon = month(as.Date(init_date)+floor(lt/24))
  df_working = select(df_lt, ens_mean, ens_med, ens_sd)#, dez01, dez09, iqr, skew, kurt, mon)
  return(df_working)
}

temp_qrf_TEST = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
  #' Function that predicts temp based on a quantile regression forest
  #' init_date: as all the time
  #' quantile_levels: as all the time
  #' ntree: number of trees in random forest (see randomForest Docu)
  #' nodesize: minimum size of terminal nodes (see randomForest Docu)
  
  df = get_hist_temp_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train_TEST(df=df, lt=lead_time)
    df_training_target = df_training[,4]
    df_training_predictors = df_training[,-4]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_temp_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_TEST(df_new, lead_time, init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}

wind_qrf_TEST = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), ntree=500, nodesize=5){
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
    df_training = qrf_feature_eng_train_TEST(df=df, lt=lead_time)
    df_training_target = df_training[,4]
    df_training_predictors = df_training[,-4]
    # Quantile Regression Forest
    qrf = quantregForest(df_training_predictors, df_training_target, nthreads = 4, ntree=ntree, nodeseize=nodesize)
    # Predict
    df_new = get_current_wind_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict_TEST(df_new, lead_time, init_date)
    fcst[i,] = predict(qrf, newdata = df_new_predictors, what = quantile_levels)
    i = i + 1
  }
  return(fcst)
}


temp_score_qrf_tiny = evaluate_model_weather(temp_qrf_TEST, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)
temp_score_qrf_tiny

temp_model_scores

wind_score_qrf_tiny = evaluate_model_weather(wind_qrf_TEST, 'wind', init_dates=init_dates, per_horizon=TRUE)
wind_score_qrf_tiny

wind_model_scores

save(temp_score_qrf_tiny, wind_score_qrf_tiny, file="graphics and tables for elaboration/weather/week10_qrf_tiny.RData")


# WEEK 10: Explore CatBoost -----------------------------------------------


# Check if 64 bit version is installed (necessary for CatBoost)
Sys.getenv("R_ARCH")

#install.packages('devtools')
#devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
library(catboost)
load("graphics and tables for elaboration/weather/week10_modelscores.RData")
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24', '2021-12-01', '2021-12-08', '2021-12-15')

wind_catboost = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), addmslp=FALSE, addclct=FALSE, addrad=FALSE){
  #' Function that predicts wind based on decision tree boosting
  #' init_date: as all the time
  #' quantile_levels: as all the time
  
  df = get_hist_wind_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train(df=df, lt=lead_time, addmslp=addmslp, addclct=addclct, addrad=addrad)
    df_training_target = df_training[,10]
    df_training_predictors = df_training[,-10]
    # Prepare Training
    pool = catboost.load_pool(df_training_predictors, label = df_training_target)
    # Prepare Predicting
    df_new = get_current_wind_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict(df_new, lead_time, init_date, addmslp=addmslp, addclct=addclct, addrad=addrad)
    pool_pred = catboost.load_pool(df_new_predictors)
    j = 1
    # Has to be modeled per quantile
    for (quantile_level in quantile_levels){
      fit_params = list(iterations = 100, loss_function=paste0('Quantile:alpha=',quantile_level))
      model = catboost.train(pool, params = fit_params)
      fcst[i,j] = catboost.predict(model, pool = pool_pred, prediction_type = 'RawFormulaVal')
      j = j + 1
    }
    i = i + 1
  }
  return(fcst)
}

catboost_scores_wind  = evaluate_model_weather(wind_catboost, 'wind', init_dates=init_dates, per_horizon=TRUE)

catboost_scores_wind 
wind_model_scores

temp_catboost = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), addmslp=FALSE, addclct=FALSE, addrad=FALSE){
  #' Function that predicts wind based on decision tree boosting
  #' init_date: as all the time
  #' quantile_levels: as all the time
  
  df = get_hist_temp_data() %>% na.omit
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train(df=df, lt=lead_time, addmslp=addmslp, addclct=addclct, addrad=addrad)
    df_training_target = df_training[,10]
    df_training_predictors = df_training[,-10]
    # Prepare Training
    pool = catboost.load_pool(df_training_predictors, label = df_training_target)
    # Prepare Predicting
    df_new = get_current_temp_data(init_date)[,-1]
    df_new_predictors = qrf_feature_eng_predict(df_new, lead_time, init_date, addmslp=addmslp, addclct=addclct, addrad=addrad)
    pool_pred = catboost.load_pool(df_new_predictors)
    j = 1
    # Has to be modeled per quantile
    for (quantile_level in quantile_levels){
      fit_params = list(iterations = 100, loss_function=paste0('Quantile:alpha=',quantile_level))
      model = catboost.train(pool, params = fit_params)
      fcst[i,j] = catboost.predict(model, pool = pool_pred, prediction_type = 'RawFormulaVal')
      j = j + 1
    }
    i = i + 1
  }
  return(fcst)
}

catboost_scores_temp  = evaluate_model_weather(temp_catboost, 'air_temperature', init_dates=init_dates, per_horizon=TRUE)

catboost_scores_temp
temp_model_scores

save(catboost_scores_temp, catboost_scores_wind, file='graphics and tables for elaboration/catboost_scores.RData')


# WEEK 10: Explore GBM ----------------------------------------------------


# CatBoost should be optimal for categorical gradient boosting, so lets try gbm
#install.packages('gbm')
library(gbm)
init_dates = c('2021-10-27', '2021-11-03', '2021-11-10', '2021-11-17', '2021-11-24', '2021-12-01', '2021-12-08', '2021-12-15')

wind_gbm = function(init_date, quantile_levels=c(0.025,0.25,0.5,0.75,0.975), addmslp=FALSE, addclct=FALSE, addrad=FALSE, 
                    training_data, n.trees=1000, shrinkage=0.001, interaction.depth=1){
  #' Function that predicts wind based on decision tree boosting
  #' init_date: as all the time
  #' quantile_levels: as all the time
  
  if(missing(training_data)){
    # Get historical data
    df = get_hist_wind_data() %>% na.omit
    df_new = get_current_wind_data(init_date)[,-c(1,43)]
  }
  else{
    df = training_data %>% na.omit
    df_new = subset(get_hist_wind_data(), init_tm==as.Date(init_date))[,c(4,7:46)]
  }
  fcst = matrix(nrow = 5, ncol = length(quantile_levels))
  i = 1
  for (lead_time in c(36,48,60,72,84)){
    # Feature Engineering
    df_training = qrf_feature_eng_train(df=df, lt=lead_time, addmslp=addmslp, addclct=addclct, addrad=addrad)
    # Feature Engineering Predictions
    df_new_predictors = qrf_feature_eng_predict(df_new, lead_time, init_date, addmslp=addmslp, addclct=addclct, addrad=addrad)
    # Has to be fit per horizon
    for (j in 1:length(quantile_levels)){
      quantile = quantile_levels[j]
      # Train
      gbm.fit = gbm(
        formula = obs ~ .,
        distribution = list(name = "quantile", alpha = quantile),
        data = df_training,
        n.trees = n.trees,
        interaction.depth = interaction.depth,
        shrinkage = shrinkage,
        #cv.folds = 5,
        verbose = FALSE
      )
      # Predict
      fcst[i,j] = predict.gbm(gbm.fit, df_new_predictors, n.trees=gbm.fit$n.trees)
    }
    i = i + 1
  }
  return(fcst)
}

# Test Function
wind_gbm('2021-11-03')

# Evaluate Model (out-of-the-box)
score_gbm_ootb = evaluate_model_weather(wind_gbm, 'wind', init_dates=init_dates, per_horizon=TRUE)
score_gbm_ootb

# Compare to other models
load("graphics and tables for elaboration/weather/week10_modelscores.RData")
wind_model_scores

# Cross-Validate
cv_scores_gbm_tuning = matrix(NA, nrow=12, ncol=6)
colnames(cv_scores_gbm_tuning ) = c('Overall', '36h', '48h', '60h', '72h', '84h')

i = 1
for(n.trees in c(100, 1000, 5000)){
  for(shrinkage in c(0.001, 0.01)){
    for(interaction.depth in c(1,2)){
      start_time = Sys.time()
      message(paste0('Evaluation parameter combination number ', i))
      cv_scores_gbm_tuning[i,] = apply(cross_validate_weather(wind_gbm, 'wind', n.trees=n.trees, shrinkage=shrinkage, 
                                                              interaction.depth=interaction.depth), 2, mean)
      cat(paste0('\nTime Taken: ', Sys.time()-start_time, '\n\n'))
      i = i + 1
    }
  }
}
cv_scores_gbm_tuning


save(cv_scores_gbm_tuning, file='graphics and tables for elaboration/weather/week10_gbm_cv.RData')


# Compare CV Results
load('graphics and tables for elaboration/weather/week10_cross_validation.RData')
cv_scores_wind
#cv_scores_temp

# Check for possible combinations per horizon:
cv_scores_wind[3,]
min(cv_scores_gbm_tuning[,3])
min(cv_scores_gbm_tuning[,5])
# -> EMOS TL + MSLP for 12:00 forecasts, gbm for night forecasts
# which gbm parameters? horizon 3: 1000, 0.01, 2; horizon 5: 1000, 0.01, 1
# Lets take 1000, 0.01, 1 because the gap between the other scores (horizon 3) is pretty small
wind_gbm_emos_mix('2022-01-12')


# WEEK 10: Cross-Validation for weather models ----------------------------


# Wind
source('src/model_wind.R')
cross_validate_weather(wind_emos_tn, 'wind', kfold=2)
cross_validate_weather(wind_emos_tl, 'wind', kfold=2)
cross_validate_weather(wind_emos_tl_multi, 'wind', kfold=2)
cross_validate_weather(wind_emos_tl_multi_boosting, 'wind', kfold=2)
cross_validate_weather(wind_qrf, 'wind', kfold=2)

cv_scores_wind  = matrix(NA, nrow=5, ncol=6)
rownames(cv_scores_wind ) = c('EMOS TN', 'EMOS TL', 'EMOS TL MSLP', 'EMOS TL MSLP Boosting', 'QRF')
colnames(cv_scores_wind ) = c('Overall', '36h', '48h', '60h', '72h', '84h')

cv_scores_wind [1,] = apply(cross_validate_weather(wind_emos_tn, 'wind', kfold=10), 2, mean)
cv_scores_wind [2,] = apply(cross_validate_weather(wind_emos_tl, 'wind', kfold=10), 2, mean)
cv_scores_wind [3,] = apply(cross_validate_weather(wind_emos_tl_multi, 'wind', kfold=10), 2, mean)
cv_scores_wind [4,] = apply(cross_validate_weather(wind_emos_tl_multi_boosting, 'wind', kfold=10), 2, mean)
cv_scores_wind [5,] = apply(cross_validate_weather(wind_qrf, 'wind', kfold=10), 2, mean)
cv_scores_wind


# Temp
source('src/model_temp.R')
cross_validate_weather(temp_emos, 'air_temperature', kfold=2)
cross_validate_weather(temp_emos_multi, 'air_temperature', kfold=2)
cross_validate_weather(temp_emos_multi_boosting, 'air_temperature', kfold=2)
cross_validate_weather(temp_emos_multi_boosting_mixture, 'air_temperature', kfold=2)
cross_validate_weather(temp_qrf, 'air_temperature', kfold=2)

cv_scores_temp = matrix(NA, nrow=5, ncol=6)
rownames(cv_scores_temp) = c('EMOS', 'EMOS Multi', 'EMOS Multi Boosting', 'EMOS Multi Boosting Mix', 'QRF')
colnames(cv_scores_temp) = c('Overall', '36h', '48h', '60h', '72h', '84h')

cv_scores_temp[1,] = apply(cross_validate_weather(temp_emos, 'air_temperature', kfold=10), 2, mean)
cv_scores_temp[2,] = apply(cross_validate_weather(temp_emos_multi, 'air_temperature', kfold=10), 2, mean)
cv_scores_temp[3,] = apply(cross_validate_weather(temp_emos_multi_boosting, 'air_temperature', kfold=10), 2, mean)
cv_scores_temp[4,] = apply(cross_validate_weather(temp_emos_multi_boosting_mixture, 'air_temperature', kfold=10), 2, mean)
cv_scores_temp[5,] = apply(cross_validate_weather(temp_qrf, 'air_temperature', kfold=10), 2, mean)
cv_scores_temp

save(cv_scores_temp, cv_scores_wind, file='graphics and tables for elaboration/weather/week10_cross_validation.RData')

# Look at scores:
load('graphics and tables for elaboration/weather/week10_cross_validation.RData')
load('graphics and tables for elaboration/weather/week10_modelscores.RData')
# Temp
cv_scores_temp
t(temp_model_scores)
# -> Should use EMOS Multi
# Wind
cv_scores_wind
t(wind_model_scores)
# -> Should use EMOS TL Multi