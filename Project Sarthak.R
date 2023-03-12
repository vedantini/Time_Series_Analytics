library(forecast)
# Setting up the working directory
setwd("/Users/sarthakmeliwal/Desktop/BAN 673 - Time Series Analytics/Project")

# Importing required scripts
source("DataPreProcessing.R")
source("Generate_Time_Series.R")
source("Check_Predictability.R")

# Importing the data
data <- read.csv("GOOG.csv")

# Pre-processing the data to get the monthly stock value
data <- DP(data)

# Displaying 6 top and bottom records of the data
head(data)
tail(data)

# Converting the data into a time series data format.
data.ts <- Gen_TS(data,2013,1,2022,12)

# Displaying time series data.
data.ts

#Creating plot of historical time series data.
plot(data.ts)

#Checking predictability of historical data.
CP(data.ts)
#--------------------------------------------------
library(forecast)
library(zoo)
Stock.data <- read.csv("BROS.csv")
stock.ts <- ts(Stock.data$Close, 
              start = c(2012, 1), end = c(2022, 12), freq = 12)

# Displaying time series data
stock.ts

plot(stock.ts, 
     xlab = "Time", ylab = "Valuation", 
     ylim = c(500, 1000), xaxt = 'n',
     main = "Stock Closing Price")
axis(1, at = seq(2012, 2022, 1), labels = format(seq(2012, 2022, 1)))


ac12 <- Acf(stock.ts, lag.max = 12, 
            main = "Autocorrelation for Stock Pricing")

# Displaying the autocorrelation of data with all the 12 lags
Lag <- round(ac12$lag, 0)
ACF <- round(ac12$acf, 3)
data.frame(Lag, ACF)


nValid <- 48
nTrain <- length(stock.ts) - nValid
training.ts <- window(stock.ts, start = c(2012, 1), end = c(2012, nTrain))
validation.ts <- window(stock.ts, start = c(2012, nTrain + 1), 
                        end = c(2012, nTrain + nValid))


tma_4 <- rollmean(training.ts, k = 4, align = "right")
tma_7 <- rollmean(training.ts, k = 7, align = "right")
tma_10 <- rollmean(training.ts, k = 10, align = "right")


tma_4_prediction <- forecast(tma_4, h = nValid, level = 0)
tma_4_prediction
tma_7_prediction <- forecast(tma_7, h = nValid, level = 0)
tma_7_prediction
tma_10_prediction <- forecast(tma_10, h = nValid, level = 0)
tma_10_prediction


round(accuracy(tma_4_prediction$mean, validation.ts), 3)
round(accuracy(tma_7_prediction$mean, validation.ts), 3)
round(accuracy(tma_10_prediction$mean, validation.ts), 3)


reg.train.seas <- tslm(training.ts ~ trend + season)
summary(reg.train.seas)

reg.ts.prediction <- forecast(reg.train.seas, h = nValid, level = 0)
reg.ts.prediction


reg.train.res <- reg.train.seas$residuals
reg.train.res

tma_res <- rollmean(reg.train.res, k = 4, align = "right")
tma_res

tma.res.pred <- forecast(tma_res, h = nValid, level = 0)
tma.res.pred

two_level_forecast <- reg.ts.prediction$mean + tma.res.pred$mean
two_level_forecast

validation.df <- round(data.frame(validation.ts, reg.ts.prediction$mean, 
                                  tma.res.pred$mean, 
                                  two_level_forecast), 3)
names(validation.df) <- c("Pricing", "Regression_Forecast", 
                          "Trailing MA Residual", "Two Level Forecast")
validation.df

# Apply the accuracy() function to compare accuracy of the regression model with linear trend and seasonality and the two-level (combined) model with regression and trailing MA for residuals.
round(accuracy(reg.ts.prediction$mean, validation.ts), 3)
round(accuracy(two_level_forecast, validation.ts), 3)


stock.trend.seas <- tslm(stock.ts ~ trend + season)
summary(stock.trend.seas)

#Getting residuals from 1 level
stock.trend.seas.pred <- stock.trend.seas$residuals
stock.trend.seas.pred


tma_res_ed <- rollmean(stock.trend.seas.pred, k = 4, align = "right")
tma_res_ed

#Forecasting for 12 months in future with regression model
reg_ts_ed_prediction <- forecast(stock.trend.seas, h = 12, level = 0)
reg_ts_ed_prediction

tma_res_ed_prediction <- forecast(tma_res_ed, h = 12, level = 0)
tma_res_ed_prediction

two_level_forecast_ed <- reg_ts_ed_prediction$mean + tma_res_ed_prediction$mean
two_level_forecast_ed

future.forecast.df <- round(data.frame(reg_ts_ed_prediction$mean, 
                                       tma_res_ed_prediction$mean, 
                                       two_level_forecast_ed), 3)
names(future.forecast.df) <- c("Regression Forecast", 
                               "MA Residual Forecast", "Combined Forecast")
future.forecast.df

snaive.forecast <- snaive(stock.ts, h = 12)
snaive.forecast

round(accuracy((snaive(stock.ts))$fitted, stock.ts), 3)
round(accuracy(reg_ts_ed_prediction$fitted, stock.ts), 3)
round(accuracy(reg_ts_ed_prediction$fitted+tma_res_ed, stock.ts), 3)

#Holt's Winter Model
hw.model <- ets(training.ts, model = "ZZZ")
hw.model

hw.model.pred <- forecast(hw.model, h = nValid, level = 0)
hw.model.pred


hw.stock.model <- ets(stock.ts, model = "ZZZ")
hw.stock.model

#Developing forecast for 12 months in future with HW optimal model
hw.stock.model.pred <- forecast(hw.stock.model, h = 12, level = 0)
hw.stock.model.pred

#Developing HW ANA model based on entire data.
hw.stock.model.ANA <- ets(stock.ts, model = "ANA")
hw.stock.model.ANA

#Developing forecast for 12 months in future with HW ANA model
hw.stock.model.ANA.pred <- forecast(hw.stock.model.ANA, h = 12, level = 0)
hw.stock.model.ANA.pred

round(accuracy((snaive(stock.ts))$fitted, stock.ts), 3)
round(accuracy(hw.stock.model.pred$fitted, stock.ts), 3)
round(accuracy(hw.stock.model.ANA.pred$fitted, stock.ts), 3)

round(accuracy((snaive(stock.ts))$fitted, stock.ts), 3)
round(accuracy(reg_ts_ed_prediction$fitted, stock.ts), 3)
round(accuracy(reg_ts_ed_prediction$fitted+tma_res_ed, stock.ts), 3)
round(accuracy(hw.stock.model.pred$fitted, stock.ts), 3)
round(accuracy(hw.stock.model.ANA.pred$fitted, stock.ts), 3)