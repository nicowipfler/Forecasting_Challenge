# Script containing several exploratory analyses aiming to improve existing models or to find new ones


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
length_scores = matrix(nrow=191,ncol=2)
# Quantile Levels
quantile_levels = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 
# First implementation: c(0.05,0.25,0.5,0.75,0.975), but for this analysis they should be equidistant

# For each rolling_window length:
for (window_length in 10:200){
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
dax_data = dax_data[!is.na(dax_data$ret1),]
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
qqPlot(res_std_test, dist='norm')
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