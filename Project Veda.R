library(forecast)
# Setting up the working directory
setwd("C:/Users/STSC/OneDrive - horizon.csueastbay.edu/Documents/Semester II/Time Series Analytics/Time_Series_Analytics Project")

# Importing required scripts
source("DataPreProcessing.R")
source("Generate_Time_Series.R")
source("Check_Predictability.R")

# Importing the data
data <- read.csv("GOOG.csv")
head(data)

# Pre-processing the data to get the monthly stock value
data <- DP(data)

# Displaying 6 top and bottom records of the data
head(data)
tail(data)
# Converting the data into a time series data format.
data.ts <- Gen_TS(data,2012,01,2022,12)

# Displaying time series data.
data.ts

#Creating plot of historical time series data.
plot(data.ts)

#Checking predictability of historical data.
CP(data.ts)
#---------------------------------------------------------------------------------------------------------
#                         Regression Model with Linear trend and seasonality
#---------------------------------------------------------------------------------------------------------


#divide into validation and training partition
nValid <- 12
nTrain <- length(data.ts) - nValid
train.ts <- window(data.ts, start = c(2012, 1), end = c(2012,nTrain))
valid.ts <- window(data.ts, start = c(2012, nTrain + 1), 
                   end = c(2012, nTrain + nValid))
# comments : We have used 1 year as a validation set and remaining avalable data
#will train the model to forecast in that 1 years validation window.
#using tslm() for linear trend and seasonal reg model.



trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)
#Comment:
#The model incorporates both linear trean and seasonality.From the summary we can see 
#the intercept value = 93407233 , Trend = -592451 and 11 dummy variables 
#each denoting 1 season and holding binary values of 1 or 0. If all 11 variables are 0 then we would be
#considering the month of January
#PUT EQUATION HERE

# predictions with trend +seasonality in validation set.  
train.trend.season.pred <- forecast(trend.seas, h = nValid, level = 0)
train.trend.season.pred

plot(train.trend.season.pred)

#plot(data.ts)
lines(data.ts, col = "green", lty = 1, lwd = 3)

#Comment:
#from the plot we can see that our model has under forecasted the values.The green
#line is the plot of our original data set.Visually we can say that this is not a 
#good model to use by itself and we can try using this model in a 2 level 
#model to see if it can predict better.We can also check if the model is a good
#fit with our data from the accuracy measures which are show below.

#accuracy for regression model with linear trend and seasonality
round(accuracy(train.trend.season.pred$mean, valid.ts), 3) 
#----------------------------------------------------------------------------
#AUTO-ARIMA
#-----------------------------------------------------------------------------  

#auto.arima()
train.auto.arima <- auto.arima(train.ts) #Separate function from ARIMA function
summary(train.auto.arima)

#comment:
#The ARIMA model generated is ARIMA(0,1,2)(2,0,0)
#it means that for the non seasonal part we have 0 order auto-regression,autoregressive model AR(0), no autoregressive model
# order 1 differencing to remove linear trend and order 1 moving average
# order 2 autoregressive model AR(0) for seasonality and 0 order for differencing and moving average

#Forecast
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred


plot(train.auto.arima.pred)

#plot(data.ts)
lines(data.ts, col = "green", lty = 1, lwd = 3)
#comment:
#

#accuracy measures auto arima validation data
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3) 

#-----------------------------------------------------------------------------
#AUTO-ARIMA for entire data set
#----------------------------------------------------------------------------
auto.arima <- auto.arima(data.ts)
summary(auto.arima)

#forecast
auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred


plot(auto.arima.pred)
lines(data.ts, col = "green", lty = 1, lwd = 3)

#Accuracy measure auto arima entire data set
round(accuracy(auto.arima.pred$fitted, data.ts), 3)

