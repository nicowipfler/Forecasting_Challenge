# Preparations ------------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyr)
library(quantreg)


# Forecasts ---------------------------------------------------------------


fcst_dax = dax_quantreg("2021-10-23", TRUE)
fcst_dax
plot_forecasts_dax('2021-10-23', fcst_dax, 100, 'quantile regression (basic)')

# hier wurden fcst_temp und fcst_wind über die zugehörigen Skripte erstellt, da noch nicht als Funktion implementiert
create_csv("2021-10-23", fcst_dax, fcst_temp, fcst_wind)