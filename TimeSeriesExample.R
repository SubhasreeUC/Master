#Load Libraries
library("fUnitRoots")
library(lmtest)
library("forecast")
library(FitAR)

#import data
data<-read.csv("data.csv",header=TRUE)
#convert to time series
tsData<- ts(data[2:35,2],start=c(2011,1),frequency=12)
plot(tsData)
#decompose into time series components
timeseriescomponents <- decompose(tsData)
plot(timeseriescomponents)
#detemine stationarity of data
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary<-diff(tsData, differences=1)
plot(tsstationary)
acf(tsData,lag.max=34)
#remove seasonality
timeseriesseasonallyadjusted <- tsData- timeseriescomponents$seasonal
plot(timeseriesseasonallyadjusted)
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)
par(mfrow=c(2,1))
acf(tsstationary, lag.max=34) 
pacf(tsstationary, lag.max=34)
#fit the model
fitARIMA<-arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
fitARIMA
#significance of coefficients
coeftest(fitARIMA)
par(mfrow=c(1,1))
acf(fitARIMA$residuals)
#residual diagnostics
boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1) # residual?? or the original series?
par(mfrow=c(2,1))
plot(boxresult[,3],main="Ljung-Box Q Test", ylab="P-values", xlab="Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)

auto.arima(tsData, trace=TRUE)

#forcast future values
par(mfrow=c(1,1))
predict(fitARIMA,n.ahead = 5)
futurVal <- forecast.Arima(fitARIMA,h=10, level=c(99.5))
plot.forecast(futurVal)

