# Prep --------------------------------------------------------------------


library(ggplot2)
library(rdwd)
dwd_url = selectDWD(
  name = "Rheinstetten",
  res = "hourly",
  per = "recent",
  var = "wind"
)
dataDWD(dwd_url)
obs_data_wind = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_wind_recent_stundenwerte_FF_04177_akt.zip')
dwd_url = selectDWD(
  name = "Rheinstetten",
  res = "hourly",
  per = "recent",
  var = "air_temperature"
)
dataDWD(dwd_url)
obs_data_temp = readDWD('C:/dev/Forecasting_Challenge/DWDdata/hourly_air_temperature_recent_stundenwerte_TU_04177_akt.zip')


# WIND --------------------------------------------------------------------


obs_data_recent = subset(obs_data_wind, MESS_DATUM > "2021-10-13", select = c("MESS_DATUM","F"))
# convert to kmh
obs_data_recent$F = obs_data_recent$F * 3.6
obs_data_recent
ggplot(obs_data_recent, aes(x = MESS_DATUM, y = F)) + 
  geom_line(size = I(.4), color="darkgrey") + 
  geom_point(shape=21,size=1.5)
#TODO Fix why data ends at 2021-10-20 -> DWD is having problems
#TODO Plot Forecasts after the data


# TEMP --------------------------------------------------------------------


obs_data_recent = subset(obs_data_temp, MESS_DATUM > "2021-10-13", select = c("MESS_DATUM","TT_TU"))
obs_data_recent
test = rbind(obs_data_recent, fcst_temp_df[c(3,8,13,18,23),]) # use median forecasts as points
ggplot(test, aes(x = MESS_DATUM, y = TT_TU)) + 
  geom_line(size = I(.4), color="darkgrey") + 
  geom_point(shape=21,size=1.5)

#TODO Fix why data ends at 2021-10-20 -> DWD is having problems
#TODO Plot Forecasts after the data