library(forecast)
# Setting up the working directory
setwd("C:/Users/STSC/Desktop/Time Series/Time_Series_Analytics-main/Time_Series_Analytics-main")

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
data.ts <- Gen_TS(data,2012,1,2022,12)

# Displaying time series data.
data.ts

#Creating plot of historical time series data.
plot(data.ts)

#Checking predictability of historical data.
CP(data.ts)

nValid <- 12
nTrain <- length(data.ts) - nValid
train.ts <- window(data.ts, start = c(2012, 1), end = c(2012, nTrain))
valid.ts <- window(data.ts, start = c(2012, nTrain + 1), 
                   end = c(2012, nTrain + nValid))
train.ts
valid.ts



AR1 <- Arima(train.ts, order = c(1,0,0))
model_summary <- summary(AR1)
model_summary

# Make predictions on the test set
AR1.pred <- forecast(AR1, h = nValid, level = 0)
AR1.pred

round(accuracy(AR1.pred$mean, valid.ts), 3)
# AR1
# Plot predicted values and validation set
options(scipen = 999)
plot(AR1.pred$mean, type="l", xlab="Time", ylab="Value", col="blue", main="ARIMA Predictions vs. Validation Set")
lines(valid.ts, col="red")
legend("topleft", c("ARIMA Predictions", "Validation Set"), col=c("blue", "red"), lty=1)
valid.ts



# two level
# TSLM + AR1

# First level: TSLM model
tslm <- tslm(train.ts ~ trend + season)
tslm.pred <- forecast(tslm, h=nValid, level = 0)
tslm.pred

tslm.res <- tslm$residuals
tslm.res


# second level: AR(1) model
arima.res <- arima(tslm.res, order=c(1,0,0))
arima.res.pred <- forecast(arima.res, h=nValid, level = 0)
arima.res.pred


two_level_forecast <- tslm.pred$mean + arima.res.pred$mean
two_level_forecast

validation.df <- round(data.frame(valid.ts, tslm.pred$mean, 
                                  arima.res.pred$mean, 
                                  two_level_forecast), 3)
names(validation.df) <- c("Pricing", "TSLM forecast", 
                          "AR1 residuals", "Two Level Forecast")
validation.df


round(accuracy(tslm.pred$mean, valid.ts), 3)
round(accuracy(two_level_forecast, valid.ts), 3)

# Plot predicted values and validation set
plot(two_level_forecast, type="l", xlab="Time", ylab="Value", col="blue", main="two level vs. Validation Set")
lines(valid.ts, col="red")
legend("bottomleft", c("ARIMA Predictions", "Validation Set"), col=c("blue", "red"), lty=1)




#forecasting into 12 month in future

# First level: TSLM model
tslm.td <- tslm(data.ts ~ trend + season)
tslm.td.pred <- forecast(tslm.td, h=nValid, level = 0)
tslm.td.pred

tslm.td.res <- tslm.td$residuals
tslm.td.res


# second level: AR(1) model
arima.td.res <- arima(tslm.td.res, order=c(1,0,0))
arima.td.res.pred <- forecast(arima.td.res, h=nValid, level = 0)
arima.td.res.pred


two_level_forecast <- tslm.td.pred$mean + arima.td.res.pred$mean
two_level_forecast

future.df <- round(data.frame(valid.ts, tslm.td.pred$mean, 
                                  arima.td.res.pred$mean, 
                                  two_level_forecast), 3)
names(future.df) <- c("Pricing", "TSLM forecast", 
                          "AR1 residuals", "Two Level Forecast")
future.df

par(mar=c(5, 5, 4, 2), xpd=TRUE)

plot(data.ts, type="l", xlab="Time", ylab="Value", col="blue", main="data.ts and + future prediction" ,ylim=c(min(data.ts, two_level_forecast), max(data.ts, two_level_forecast)))
lines(two_level_forecast, col="red")
legend("bottomleft", c("ARIMA Predictions", "Validation Set"), col=c("blue", "red"), lty=1)



