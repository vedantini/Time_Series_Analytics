CP <- function(data.ts)
{
  #Developing the ar1 model to check the predictability of the historical data
  hist_data_ar1 <- Arima(data.ts, order = c(1,0,0))
  model_summary <- summary(hist_data_ar1)
  print(model_summary)
  
  #Developing hypothesis test to check the predictability of data at alph=0.05
  ar1 <- round(model_summary$coef[1],4)
  se <- round(sqrt(diag(model_summary$var.coef)),4)
  hypo_mean <- 1
  alpha = 0.05
  zstats <- (ar1-hypo_mean)/se
  pvalue <- pnorm(zstats)
  if(pvalue[1] < alpha){
    print('Reject null hypothesis, Time series is predictable')
  }else{
    print('Can not reject null hypothesis, Time series is random walk')
  }
  
  #Developing the difference series with hist data and hist data lagged-1
  hist_data_diff_series <- diff(data.ts, lag = 1)
  hist_data_diff_series
  
  #Developing autocorrelation for difference series to check the predictability
  Acf(hist_data_diff_series, lag.max = 8, 
      main = "Autocorrelation for lagged 1 diff_series")
}