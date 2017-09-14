rm(list=ls(all=TRUE))
setwd("~/Sridhar_CDrive/Desktop/Desktop_20150323/Batch 22/CSE 7202c/HoltWinters-ARIMA-ARIMAX")

#TIME SERIES FORECASTING

library("forecast")
library("stats")
library(TTR)

#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).

# Read in the US Air Carrier Traffic - Revenue Passenger Miles dataset
#   Unit: Thousand Miles
#   Data Source: http://www.bts.gov/xml/air_traffic/src/index.xml
#   and https://datamarket.com/data/set/281x/us-air-carrier-traffic-statistics-revenue-passenger-miles 

miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles

milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow=c(1,1))
plot(milestimeseries)

#Decomposition

milestimeseriescomponents <- 
  decompose(milestimeseries, type = "additive")

plot(milestimeseriescomponents)
milestimeseriescomponents$seasonal
milestimeseriescomponents$trend
milestimeseriescomponents$figure

#ACF and PACF of real world data

par(mfrow=c(1,3))
plot.ts(milestimeseries)
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)
#acf(milestimeseries, lag.max=20, ci.type="ma")

# Differencing and ACF, PACF on
# Stationary and Non-Stationary Data

ndiffs(milestimeseries)
nsdiffs(milestimeseries)

par(mfrow=c(1,2))
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)

milestimeseriesdiff1 <- diff(milestimeseries, differences=1)
milestimeseriesdiff1

acf(milestimeseriesdiff1, lag.max=20)
pacf(milestimeseriesdiff1, lag.max=20)

#Holt-Winters
#Additive, trend and seasonality models
#using HoltWinters() function in "stats" package
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
milesHW1 <- 
  HoltWinters(milestimeseries)
milesHW1
milesHW1$fitted

par(mfrow=c(1,1))
plot(milesHW1)
milesHW1$SSE
milesresidualsHW1 <- residuals(milesHW1)
milesresidualsHW1
plot(milesresidualsHW1)
par(mfrow = c(1,2))
acf(milesresidualsHW1)
pacf(milesresidualsHW1)

library("forecast")

#it predicts seasonal peaks well
milesforecastHW1 <- 
  forecast.HoltWinters(milesHW1, 
                       h=36)

milesforecastHW1
par(mfrow = c(1, 1))
plot.forecast(milesforecastHW1,
              shadecols="oldstyle")

# forecast with NO trend and seasonality

milesHW2 <- 
  HoltWinters(milestimeseries, 
              beta=FALSE, 
              gamma=FALSE)
milesforecastHW2 <- 
  forecast.HoltWinters(milesHW2,
                       h=8)
milesforecastHW2

plot.forecast(milesforecastHW2,
              shadecols="oldstyle")

#ARIMA
#Step-by-step ARIMA model building
#Manual Step for ARIMA Model building
# Model 1
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow = c(1, 1))
plot(milestimeseries)

# Step 2: Plot ACF and PACF to get preliminary understanding of the process
par(mfrow = c(1, 2))
acf(milestimeseries)
pacf(milestimeseries)

# Step 3: The suspension bridge pattern in ACF suggests both nonstationarity
# and strong seasonality.  Perform a non-seasonal difference to give an ARIMA(0,1,0) model
par(mfrow = c(1, 1))
milestimeseriesdiff1 <- diff(milestimeseries, differences = 1)
milestimeseriesdiff1
plot(milestimeseriesdiff1)

# Step 4: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesdiff1)
pacf(milestimeseriesdiff1)

# Step 5: The differenced series looks stationary but has strong seasonal lags
# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)
par(mfrow = c(1, 1))
milestimeseriesseasonaldiff1 <- 
  diff(milestimeseries, lag = 12, differences=1)
milestimeseriesseasonaldiff1
plot(milestimeseriesseasonaldiff1)

# Step 6: Check ACF and PACF for seasonally differenced data
#to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesseasonaldiff1)
pacf(milestimeseriesseasonaldiff1)

# Step 7: Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Perform a non-seasonal differencing.
# ARIMA(0,1,0)(0,1,0)12
par(mfrow = c(1, 1))
milestimeseriesSeasNoSeasdiff1 <- 
  diff(milestimeseriesseasonaldiff1, differences=1)
milestimeseriesSeasNoSeasdiff1
plot(milestimeseriesSeasNoSeasdiff1)

# Step 8: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesSeasNoSeasdiff1)
pacf(milestimeseriesSeasNoSeasdiff1)

# Step 9: ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term
milesArima1 <- Arima(milestimeseries, order = c(1,1,1),
                    seasonal = c(0,1,1), include.drift = FALSE)
milesArima1

# Step 10: Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(milesArima1$residuals, lag.max = 24)
pacf(milesArima1$residuals, lag.max = 24)
Box.test(milesArima1$residuals, lag=24, type="Ljung-Box")

# Step 11: Start forecasting
par(mfrow = c(1, 1))
milestimeseriesforecastsArima1 <- forecast.Arima(milesArima1, 
                  h=36)
plot.forecast(milestimeseriesforecastsArima1)
milestimeseriesforecastsArima1

# Automated functions are available
milesAutoArima <- auto.arima(milestimeseries,ic='aic')
milesAutoArima
milestimeseriesforecastsAutoArima <- forecast.Arima(milesAutoArima, 
                                                 h=36)
plot.forecast(milestimeseriesforecastsAutoArima)
milestimeseriesforecastsAutoArima



# Manufacturing Case Study
# Step 1: Convert data into Time Series
par(mfrow = c(1, 1))
TractorSales<-read.csv("Tractor-Sales.csv")
TractorSalesTS<-ts(TractorSales[,2],start = c(2003,1),frequency = 12)
TractorSalesTS
plot(TractorSalesTS, xlab="Years", ylab = "Tractor Sales")
TractorSalesComponents <- decompose(TractorSalesTS, type = "multiplicative")
TractorSalesComponents
plot(TractorSalesComponents)

# Step 2: Difference data to make it stationary
ndiffs(TractorSalesTS)
plot(diff(TractorSalesTS),ylab="Differenced Tractor Sales")

# Step 3: As the time series is not stationary on variance, log transform the data
plot(log10(TractorSalesTS),ylab="Log (Tractor Sales)")

# Step 4: Difference log transformed data to check for stationarity
plot(diff(log10(TractorSalesTS)),ylab="Differenced Log (Tractor Sales)")

# Step 5: Plot ACF/PACF to identify candidates for AR, MA or ARMA models
par(mfrow = c(1,2))
acf(ts(diff(log10(TractorSalesTS))),main="ACF Tractor Sales")
pacf(ts(diff(log10(TractorSalesTS))),main="PACF Tractor Sales")

# Step 6: Build ARIMA model
TractorSalesARIMA <- auto.arima(TractorSalesTS)
TractorSalesARIMA
LogTractorSalesARIMA <- auto.arima(log10(TractorSalesTS))
LogTractorSalesARIMA

# Step 8: Check residuals to ensure they are white noise
par(mfrow=c(1,2))
acf(ts(LogTractorSalesARIMA$residuals),main="ACF Residual")
pacf(ts(LogTractorSalesARIMA$residuals),main="PACF Residual")
Box.test(LogTractorSalesARIMA$residuals, lag=24, type="Ljung-Box")

# Step 8: Forecast Tractor Sales
par(mfrow = c(1, 1))
TractorSalesForecasts <- forecast.Arima(LogTractorSalesARIMA, 
                                        h=36)
plot.forecast(TractorSalesForecasts, shadecols = "oldstyle")
TractorSalesForecasts

# Alternative method
pred <- predict(LogTractorSalesARIMA, n.ahead=36)
pred

plot(TractorSalesTS,type="l",xlim=c(2004,2018),ylim=c(1,1600),
     xlab = "Year",ylab = "Tractor Sales")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+1.96*pred$se),col="red")
lines(10^(pred$pred-1.96*pred$se),col="green")
