# Importing required libraries
library(forecast)
library(zoo)

# Setting up the working directory
setwd("/Users/swanandjamadagni/Documents/MSBA/BAN-673/Project")

# Importing required scripts
source("DataPreProcessing.R")
source("Generate_Time_Series.R")
source("Check_Predictability.R")
source("Model_Plot.R")

# Importing the data
data <- read.csv("GOOG.csv")

# Pre-processing the data to get the monthly stock value
processed_data <- DP(data)

# Displaying 6 top and bottom records of the data
head(processed_data)
tail(processed_data)

# Converting the data into a time series data format.
data.ts <- Gen_TS(processed_data,2012,1,2022,12)

# Displaying time series data.
data.ts

#Creating plot of historical time series data.
plot(data.ts, ylab = "Voulme")

# Displaying autocorrelation within historical data
ac12 <- Acf(data.ts, lag.max = 12, 
            main = "Autocorrelation for historical Stock Volume")

# Displaying the autocorrelation of data with all the 12 lags
Lag <- round(ac12$lag, 0)
ACF <- round(ac12$acf, 3)
acf_df <- data.frame(Lag, ACF)
acf_df

# Displaying plots for time series components.
data.stl <- stl(data.ts, s.window = "periodic")
autoplot(data.stl, main = "Time Series Components")

#Checking predictability of historical data.
CP(data.ts)

# Partitioning the Time Series into Training and Validation partitions.
Validation_length <- 24
Training_length <- length(data.ts) - Validation_length

train.ts <- window(data.ts, start = c(2012, 1), end = c(2012, Training_length))
valid.ts <- window(data.ts, start = c(2012, Training_length + 1), 
                        end = c(2012, Training_length + Validation_length))

train.ts
valid.ts

# MA models with k = 4,7,10
tma_4 <- rollmean(train.ts, k = 4, align = "right")
tma_7 <- rollmean(train.ts, k = 7, align = "right")
tma_10 <- rollmean(train.ts, k = 10, align = "right")

# Making predictions for the validation period using MA models
tma_4.pred <- forecast(tma_4, h = Validation_length, level = 0)
tma_4.pred
mplot(tma_4.pred,"Trailing MA 4")

tma_7.pred <- forecast(tma_7, h = Validation_length, level = 0)
tma_7.pred
mplot(tma_7.pred,"Trailing MA 7")

tma_10.pred <- forecast(tma_10, h = Validation_length, level = 0)
tma_10.pred
mplot(tma_10.pred,"Trailing MA 10")

# Holt's Winter model with all additive components and optimal parameters
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA

# Making predictions for the validation period using HW AAA model
hw.AAA.pred <- forecast(hw.AAA, h = Validation_length, level = 0)
hw.AAA.pred

mplot(hw.AAA.pred,"HW(A,A,A) Model")

# Holt's Winter optimal model with optimal parameters.
hw.opt <- ets(train.ts, model = "ZZZ")
hw.opt

# Making predictions for the validation period using HW optimal model
hw.opt.pred <- forecast(hw.opt, h = Validation_length, level = 0)
hw.opt.pred

mplot(hw.opt.pred,"Optimal HW Model")

# Two level forecast with Regression and MA model

#Level-1: Regression model with linear trend and seasonality
reg.train.seas <- tslm(train.ts ~ trend + season)
summary(reg.train.seas)

# Predictions of 1st level 
reg.ts.prediction <- forecast(reg.train.seas, h = Validation_length, level = 0)
reg.ts.prediction

mplot(reg.ts.prediction,"Regression model with linear trend and seasonality")

#Level-2: MA model with k = 6 for residuals.
reg.train.res <- reg.train.seas$residuals
reg.train.res

tma_res <- rollmean(reg.train.res, k = 6, align = "right")
tma_res

# Predictions of 2nd level
tma.res.pred <- forecast(tma_res, h = Validation_length, level = 0)
tma.res.pred

# Combining forecast from level-1 and level-2
two_level_forecast_tslm_ma <- reg.ts.prediction$mean + tma.res.pred$mean
two_level_forecast_tslm_ma

mplot(two_level_forecast_tslm_ma,"Combined Forecast Regression plus MA")

reg_ma_2_level_forecast.df <- round(data.frame(valid.ts, reg.ts.prediction$mean, 
                                  tma.res.pred$mean, 
                                  two_level_forecast_tslm_ma), 3)
names(reg_ma_2_level_forecast.df) <- c("Pricing", "Regression_Forecast", 
                          "Trailing MA Residual", "Two Level Forecast")
reg_ma_2_level_forecast.df

# Auto regressive model with oreder 1
AR1 <- Arima(train.ts, order = c(1,0,0))
summary(AR1)

# Making predictions for the validation period using AR(1) model
AR1.pred <- forecast(AR1, h = Validation_length, level = 0)
AR1.pred

mplot(AR1.pred,"AR(1) Model")

# Two level forecast with Regression and AR model

# Using previously developed regression model with linear trend and seasonality as level 1 

# level-2: AR(1) model
AR1.res <- arima(reg.train.res, order=c(1,0,0))
summary(AR1.res)

AR1.res.pred <- forecast(AR1.res, h=Validation_length, level = 0)
AR1.res.pred

# Combining forecast from level-1 and level-2
two_level_forecast_tslm_ar <- reg.ts.prediction$mean + AR1.res.pred$mean
two_level_forecast_tslm_ar

mplot(two_level_forecast_tslm_ar,"Combined Forecast Regression plus AR")

reg_ar_2_level_forecast.df <- round(data.frame(valid.ts, reg.ts.prediction$mean, 
                                               AR1.res.pred$mean, 
                                               two_level_forecast_tslm_ar), 3)
names(reg_ar_2_level_forecast.df) <- c("Pricing", "Regression_Forecast", 
                          "AR1 residuals", "Two Level Forecast")
reg_ar_2_level_forecast.df

# Developing optimal ARIMA model
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Making predictions for the validation period using optimal ARIMA model
train.auto.arima.pred <- forecast(train.auto.arima, h = Validation_length, level = 0)
train.auto.arima.pred

mplot(train.auto.arima.pred,"ARIMA(0,1,2)(2,0,0)[12]")

# Comparing Accuracy of the models
round(accuracy(tma_4.pred$mean, valid.ts), 3)
round(accuracy(tma_7.pred$mean, valid.ts), 3)
round(accuracy(tma_10.pred$mean, valid.ts), 3)
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)
round(accuracy(hw.opt.pred$mean, valid.ts), 3)
round(accuracy(reg.ts.prediction$mean, valid.ts), 3)
round(accuracy(two_level_forecast_tslm_ma, valid.ts), 3)
round(accuracy(AR1.res.pred$mean, valid.ts), 3)
round(accuracy(two_level_forecast_tslm_ar, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
#-------------------------------------------------------------------------------
# Building models for future period forecasting.
#-------------------------------------------------------------------------------

# Hoslt's Winter optimal model with optimal parameters for entire dataset
hw.opt.ed <- ets(data.ts, model = "ZZZ")
hw.opt.ed

# Making predictions for the validation period using HW optimal model
hw.opt.future.pred <- forecast(hw.opt.ed, h = 12, level = 0)
hw.opt.future.pred

mplot(hw.opt.future.pred,"Future Predictions with optimal HW Model")

# Two level forecast with Regression and MA model for entire dataset

#Level-1: Regression model with linear trend and seasonality for entire data
reg.train.seas_ed <- tslm(data.ts ~ trend + season)
summary(reg.train.seas_ed)

# Predictions of 1st level 
reg.ts.future.prediction <- forecast(reg.train.seas_ed, h = 12, level = 0)
reg.ts.future.prediction

mplot(reg.ts.future.prediction,"Future Predictions with Regression model")

#Level-2: MA model with k = 6 for residuals.
reg.train.res_ed <- reg.train.seas_ed$residuals
reg.train.res_ed

tma_res_ed <- rollmean(reg.train.res_ed, k = 6, align = "right")
tma_res_ed

# Predictions of 2nd level
tma.res.future.pred <- forecast(tma_res_ed, h = 12, level = 0)
tma.res.future.pred

# Combining forecast from level-1 and level-2
two_level_future_forecast_tslm_ma <- reg.ts.future.prediction$mean + tma.res.future.pred$mean
two_level_future_forecast_tslm_ma

mplot(two_level_future_forecast_tslm_ma,"Future Forecast with Regression plus MA")


reg_ma_2_level_future_forecast.df <- round(data.frame(reg.ts.future.prediction$mean, 
                                                      tma.res.future.pred$mean, 
                                                      two_level_future_forecast_tslm_ma), 3)
names(reg_ma_2_level_future_forecast.df) <- c("Regression_Forecast", 
                                       "Trailing MA Residual", "Two Level Forecast")
reg_ma_2_level_future_forecast.df

# ARIMA model for entire dataset

auto.arima_ed <- auto.arima(data.ts)
summary(auto.arima_ed)

# Making predictions for the validation period using optimal ARIMA model
auto.arima.future.pred <- forecast(auto.arima_ed, h = 12, level = 0)
auto.arima.future.pred

mplot(auto.arima.future.pred,"Future Prediction with ARIMA(0,1,2)(2,0,0)[12]")

# Comparing Accuracy of the models for future predictions
round(accuracy(hw.opt.future.pred$fitted, data.ts), 3)
round(accuracy(reg.ts.future.prediction$fitted, data.ts), 3)
round(accuracy(reg.ts.future.prediction$fitted + tma.res.future.pred$fitted, data.ts), 3)
round(accuracy(auto.arima.future.pred$fitted, data.ts), 3)
round(accuracy((naive(data.ts))$fitted, data.ts), 3)
round(accuracy((snaive(data.ts))$fitted, data.ts), 3)
#--------------------------------THE END----------------------------------------