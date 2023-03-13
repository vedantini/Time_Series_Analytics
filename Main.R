# Importing required libraries
library(forecast)
library(zoo)

# Setting up the working directory
setwd("/Users/swanandjamadagni/Documents/MSBA/BAN-673/Project")

# Importing required scripts
source("DataPreProcessing.R")
source("Generate_Time_Series.R")
source("Check_Predictability.R")

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
plot(data.ts)

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
tma_7.pred <- forecast(tma_7, h = Validation_length, level = 0)
tma_7.pred
tma_10.pred <- forecast(tma_10, h = Validation_length, level = 0)
tma_10.pred

# Holt's Winter model with all additive components and optimal parameters
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA

# Making predictions for the validation period using HW AAA model
hw.AAA.pred <- forecast(hw.AAA, h = Validation_length, level = 0)
hw.AAA.pred

# Holt's Winter optimal model with optimal parameters.
hw.opt <- ets(train.ts, model = "ZZZ")
hw.opt

# Making predictions for the validation period using HW optimal model
hw.opt.pred <- forecast(hw.opt, h = Validation_length, level = 0)
hw.opt.pred

# Two level forecast with Regression and MA model

#Level-1: Regression model with linear trend and seasonality
reg.train.seas <- tslm(train.ts ~ trend + season)
summary(reg.train.seas)

# Predictions of 1st level 
reg.ts.prediction <- forecast(reg.train.seas, h = Validation_length, level = 0)
reg.ts.prediction

#Level-2: MA model with k = 4 for residuals.
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