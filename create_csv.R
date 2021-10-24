headers = c("forecast_date", "target", "horizon", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
testdata = data.frame(matrix(ncol = 8, nrow = 15))
colnames(testdata) = headers
testdata
#TODO Write prediction data to dataframe here
#TODO Thus, it has to be safed first
out_dir = "C://dev//Forecasting_Challenge//forecasts//"
write.csv(testdata, paste0(out_dir,'20211024_Obi_Wan_Kenobi.csv'), row.names = FALSE, quote = FALSE)