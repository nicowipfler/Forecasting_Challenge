# Preparations ------------------------------------------------------------


library(dplyr)
library(lubridate)
library(tidyr)
library(quantreg)


# FUNCTION computate return -----------------------------------------------


compute_return = function(y, type = "log", h = 1){
  n <- length(y)
  y2 <- y[-(1:h)] # exclude first h observations
  y1 <- y[-(n:(n-h+1))] # exclude last h observations
  # compute h-step cumulative returns
  if (type == "log"){
    ret <- c(rep(NA, h), 100*(log(y2)-log(y1)))
  } else {
    ret <- c(rep(NA, h), 100*(y2-y1)/y1)
  }
  ret
}


# Quantreg Model ----------------------------------------------------------


dax_quantreg = function(init_date, t=FALSE){
  #' ALL INPUTS NEED TO BE IN THE CORRECT FORMAT
  #' init_date: String containing the date of initialization of the forecasts, e.g. "2021-10-27"
  #' t: Boolean, expresses weather the results should be transposed or not, e.g. TRUE
  
  # get data
  data_dir = "C://dev//Forecasting_Challenge//data//dax//"
  dat = read.table(paste0(data_dir,init_date,"-dax.csv"), sep = ",", header = TRUE,
                   na.strings = "null") %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1),
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5),
           Date = ymd(Date))
  # prep
  quantile_levels = c(0.025,0.25,0.5,0.75,0.975)
  pred_rq = matrix(NA, length(quantile_levels), 5)
  rownames(pred_rq) = c("q0.025","q0.25","q0.5","q0.75","q0.975")
  colnames(pred_rq) = c("1 day", "2 days", "5 days", "6 days", "7 days")
  # Run separate quantile regression for each h (see p. 19-21 on slides)
  for (h in 1:5){
    # Select returns at steps h and 1
    if (h > 1){
      df <- dat[, paste0("ret", c(1, h))]
    } else {
      df <- data.frame(ret1 = dat$ret1)
    }
    # Use lagged one-step return as regressor
    df$lag_abs_ret <- lag(abs(df$ret1), h)
    # Select data, drop NA values
    df <- df[, c(paste0("ret", h), "lag_abs_ret")] %>% na.omit
    # Fit quantile regression for current value of h
    fit <- rq(formula(paste0("ret", h, "~ lag_abs_ret")),
              data = df,
              tau = quantile_levels)
    # Most recent value of regressor (used for prediction)
    new_data <- data.frame(lag_abs_ret = tail(abs(na.omit(dat$ret1)), 1))
    # Enter prediction into matrix
    pred_rq[,h] <- predict(fit, newdata = new_data)
  }
  if(t){
    pred_rq = t(pred_rq)
  }
  return(pred_rq)
}
