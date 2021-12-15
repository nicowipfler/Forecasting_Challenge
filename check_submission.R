# Instructions: Replace the path in read.csv with the path to the file you want to check
# Then run the rest of the script. If error messages appear try to fix your file and re-try.
# If the file is formally correct, only a series of statements on the checked properties
# will be printed.
# in case of problems email johannes.bracher@kit.edu

out_dir = "C://dev//Forecasting_Challenge//forecasts//"
dat <- read.csv(paste0(out_dir,"20211215_ObiWanKenobi.csv"),stringsAsFactors=FALSE)

col_names <- colnames(dat)
expected_colnames <- c("forecast_date", "target", "horizon", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
cols_quantiles <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")

# check number of columns
if(length(col_names) != length(expected_colnames)){
  stop("data contain ", length(col_names), " columns, required are ", length(expected_colnames), " columns.")
}else{
  cat("checked: number of columns correct.\n")
}

# check column names
if(any(col_names != expected_colnames)){
  stop("At least one column name is not as expected or in the wrong order.
       Column names should be: ", paste(expected_colnames, collapse = ", "))
}else{
  cat("checked:  column names correct.\n")
}

# check data types:

dat$forecast_date <- as.Date(dat$forecast_date, format = "%Y-%m-%d")
if(any(is.na(dat$forecast_date))){
  stop("forecast_date needs to be date in format YYYY-MM-DD")
}else{
  cat("checked: date format correct.")
}

if(!is.character(dat$target)){
  stop("target need to be a character string.")
}else{
  cat("checked: correct data type in column 'target' (string).")
}

if(!is.character(dat$horizon)){
  stop("horizon need to be a character string.")
}else{
  cat("checked: correct data type in column 'horizon' (string).")
}

if(!is.numeric(dat$q0.025)){
  stop("q0.025 need to be numeric.")
}else{
  cat("checked: Correct data type in column 'q0.025' (numeric).")
}

if(!is.numeric(dat$q0.25)){
  stop("q0.25 need to be numeric.")
}else{
  cat("checked: correct data type in column 'q0.25' (numeric).")
}

if(!is.numeric(dat$q0.5)){
  stop("q0.5 need to be numeric.")
}else{
  cat("checked: correct data type in column 'q0.5' (numeric).")
}

if(!is.numeric(dat$q0.75)){
  stop("q0.75 need to be numeric.")
}else{
  cat("checked:  correct data type in column 'q0.75' (numeric).")
}

if(!is.numeric(dat$q0.975)){
  stop("q0.975 need to be numeric.")
}else{
  cat("checked: Correct data type in column 'q0.975' (numeric).")
}

# check date is the same value in all rows
if(length(unique(dat$forecast_date)) > 1){
  stop("forecast_date needs to be the same in all rows.")
}else{
  cat("checked: all entries in forecast_date column are the same.")
}

# check date is not in the past
if(dat$forecast_date[1] < Sys.Date()){
  warning("Forecast_date should not be in the past.")
}else{
  cat("checked: forecast_date is not in the past.")
}

# check forecast_date is a Wednesday
if(!weekdays(dat$forecast_date[1]) %in% c("Wednesday", "Mittwoch")){
  stop("forecast_date needs to be a Wednesday.")
}else{
  cat("checked: forecast_date is a Wednesday.")
}

# check only allowed targets are there
if(!all(dat$target %in% c("DAX", "temperature", "wind"))){
  stop("target can only contain values 'DAX', 'temperature' and 'wind'")
}else{
  cat("checked: target only contains allowed values.")
}

# check entries where target is "DAX"
sub_DAX <- subset(dat, target == "DAX")

if(nrow(sub_DAX) != 5){
  stop("Exactly 5 rows need to have target == 'DAX")
}else{
  cat("checked: correct number of forecasts for DAX.")
}

if(any(sort(sub_DAX$horizon) != c("1 day", "2 day", "5 day", "6 day", "7 day"))){
  stop("For target == 'DAX', horizons need to be '1 day', '2 day', '5 day', '6 day', '7 day'")
}else{
  cat("checked: horizons for DAX are correct.")
}

# plausibility of DAX values:
if(any(sub_DAX[, cols_quantiles] > 20 | sub_DAX[, cols_quantiles] < -20)){
  warning("Implausible values for DAX detected. You may want to re-check them.")
}else{
  cat("checked: no major implausibilities in values for DAX.")
}

# check entries where target is "temperature"
sub_temperature <- subset(dat, target == "temperature")

# completeness:
if(nrow(sub_temperature) != 5){
  stop("Exactly 5 rows need to have target == 'temperature")
}else{
  cat("checked: correct number of forecasts for temperature")
}

# horizons of temperature forecasts:
if(any(sort(sub_temperature$horizon) != c("36 hour", "48 hour", "60 hour", "72 hour", "84 hour"))){
  stop("For target == 'temperature', horizons need to be '36 hour', '48 hour', '60 hour', '72 hour', '84 hour'")
}else{
  cat("checked: horizons for DAX are correct.")
}

# plausibility of temperature values:
if(any(sub_temperature[, cols_quantiles] > 50 | sub_temperature[, cols_quantiles] < -30)){
  warning("Implausible values for temperature detected. You may want to re-check them.")
}else{
  cat("checked: no major implausibilities in values for temperature.")
}

# check entries where target is "wind"
sub_wind <- subset(dat, target == "wind")

# completeness:
if(nrow(sub_wind) != 5){
  stop("Exactly 5 rows need to have target == 'wind")
}else{
  cat("checked: correct number of forecasts for wind.")
}

# horizons of wind forecasts:
if(any(sort(sub_wind$horizon) != c("36 hour", "48 hour", "60 hour", "72 hour", "84 hour"))){
  stop("For target == 'wind', horizons need to be '36 hour', '48 hour', '60 hour', '72 hour', '84 hour'")
}else{
  cat("checked: horizons for wind are correct.")
}

# plausibility of temperature values:
if(any(sub_wind[, cols_quantiles] > 300 | sub_wind[, cols_quantiles] < 0)){
  warning("Implausible values for wind detected. You may want to re-check them.")
}else{
  cat("checked: no major implausibilities in values for wind.")
}

# helper function to check if quantiles are ordered
is_ordered <- function(x) all(diff(x) >= 0)

# check that quantiles are ordered:
for(i in 1:nrow(dat)){
  if(!is_ordered(unlist(dat[i, cols_quantiles]))){
    stop("Predictive quantiles in row ", i, " are not ordered correctly (need to be non-decreasing).")
  }else{
    if(i == nrow(dat)) cat("checked: all quantiles ordered correctly.")
  }
}
