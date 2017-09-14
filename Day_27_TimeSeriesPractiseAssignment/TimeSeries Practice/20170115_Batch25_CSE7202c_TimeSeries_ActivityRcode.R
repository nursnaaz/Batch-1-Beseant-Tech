# Clear Environment variables
rm(list = ls(all.names = T))

# Read the dataset
hdfc<-  read.csv(choose.files())
str(hdfc)

# Extract month and year to aggregate
library(lubridate)
day = mdy(hdfc$day)
mth = month(day)
yr = year(day)
hdfc = data.frame(hdfc,yr,mth)

# library(dplyr)
# hdfc = select(hdfc, Price = Close, yr, mth)
# a1 = group_by(hdfc, yr, mth)
# hdfc = summarise(a1, Stock_price = mean(Price))
# rm(list = c("a1","hdfc_web","day","mth","yr"))

# group by month and year
hdfc=data.frame("Price"=hdfc$Close,yr,mth)
hdfc=aggregate(Price~mth+yr,hdfc,mean)

# Leave out last 4 records for testing
par(mfrow=c(1,1))
hdfctimeseries = ts(hdfc$Price[1:52], start = c(2011,1), frequency = 12)

plot(hdfctimeseries)
hdfc_decomp = decompose(hdfctimeseries)
plot(hdfc_decomp$seasonal)

#moving average models for smoothing
library(TTR)
par(mfrow=c(1,1))
sma_hdfc = SMA(hdfctimeseries,n=7)
sma_hdfc

#moving_average = forecast(ma(hdfctimeseries[49:56], order=2))

wma_hdfc = WMA(hdfctimeseries,n=7)
wma_hdfc

ema_hdfc = EMA(hdfctimeseries,n=7)
ema_hdfc

par(mfrow=c(1,1))
plot(hdfctimeseries,type = 'l', col = "blue")
lines(sma_hdfc, col = "black")
lines(wma_hdfc, col = "red")
lines(ema_hdfc, col = "green")

errorsma = mean(abs(hdfctimeseries[7:52]-sma_hdfc[7:52]))
errorwma = mean(abs(hdfctimeseries[7:52]-wma_hdfc[7:52]))
errorema = mean(abs(hdfctimeseries[7:52]-ema_hdfc[7:52]))

errorsma
errorwma
errorema

#########################################################333333
#using Holt's exponential smoothing, as additive model with trend
#alpha and beta are only used to estimate level and slope, as seasonality is not pronounced
hdfc_hw_forecasts = HoltWinters(hdfctimeseries, gamma = F)
hdfc_hw_forecasts



hdfc_hw_forecasts$fitted # fitted values for the current time period
plot(hdfc_hw_forecasts)

hdfc_hw_forecasts$SSE

# make predictions
library(forecast)
hdfc_hw_forecasts1 = forecast(hdfc_hw_forecasts,h=4)
plot.forecast(hdfc_hw_forecasts1)
hdfc_hw_forecasts1$residuals    #the in-sample forecast errors 

acf(hdfc_hw_forecasts1$residuals,na.action = na.omit)   #there shouldn't be any significant lags, else model can be improved further

#######################################################
#Arima
par(mfrow=c(1,2))
acf(hdfctimeseries)
pacf(hdfctimeseries)

#decomposing timeseries
hdfc_decompose_ts = decompose(hdfctimeseries)
par(mfrow=c(1,1))
plot(hdfc_decompose_ts)

#differencing to make it stationary
par(mfrow=c(1,4))
plot.ts(hdfctimeseries,main="Actual Data")
timeseriesdiff1 <-  diff(hdfctimeseries, differences=1)
plot.ts(timeseriesdiff1,main="Data with one difference")
#Differencing the time series and make it stationary
timeseriesdiff2 <- diff(hdfctimeseries, differences=2)
plot.ts(timeseriesdiff2,main="Data with two differences")
#Differencing the time series and make it stationary
timeseriesdiff3 <- diff(hdfctimeseries, differences=3)
plot.ts(timeseriesdiff3,main="Data with three differences")

ndiffs(hdfctimeseries)

hdfc_stationary_ts = diff(hdfctimeseries,differences = 1)
par(mfrow=c(1,1))
plot.ts(hdfc_stationary_ts)

# Plot acf and pacf on stationary data to find p and q
par(mfrow=c(1,2))
acf(hdfc_stationary_ts) # q=0
pacf(hdfc_stationary_ts) # p=0

tsdisplay(hdfctimeseries)

#differencing to make it stationary
nsdiffs(hdfctimeseries)
ndiffs(hdfctimeseries)
timeseriesdiff1 <-  diff(hdfctimeseries, differences=1)
tsdisplay(timeseriesdiff1) #p=0,d=1,q=0 (random walk)
fit_arima = auto.arima(hdfctimeseries)
acf(fit_arima$residuals) # no correlation among residuals

library("forecast")
# Forecasting the pick rate for the next 4 months 
arimatimeseriesforecasts <- forecast.Arima(fit_arima, h=4)
#arimatimeseriesforecasts$Forecast
plot.forecast(arimatimeseriesforecasts)
summary(fit_arima)

# Check model performance
library(DMwR)
regr.eval(hdfc$Price[53:56],arimatimeseriesforecasts$mean)
