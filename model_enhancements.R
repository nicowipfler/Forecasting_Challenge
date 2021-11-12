# Script containing several exploratory analyses aiming to improve existing models or to find new ones
source('toolkit.R')
source('model_enhancements_toolkit.R')
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
source('model_dax.R')
source('model_enhancements_toolkit.R')
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
source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2'))
source('model_dax.R')
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
source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2','rugarch'))
source('model_wind.R')
source('model_temp.R')
source('visual_checks.R')
source('model_enhancements_toolkit.R')

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


source('toolkit.R')
load_libs(libs = c('dplyr', 'lubridate', 'tidyr', 'quantreg', 'scoringRules', 'crch', 'rdwd', 'ggplot2'))
source('model_temp.R')
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


source('model_temp.R')
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
source('model_wind.R')
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