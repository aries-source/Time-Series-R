#Forecasting for the Petrol prices in USD.

require(readxl)
data= read_excel("C:/Users/perso/OneDrive/Desktop/Materials/PetrolP.ts.xlsx")

prodata = data[,2];

petrol.ts=ts(prodata,start = c(2003,9),frequency = 52)
summary(petrol.ts)
plot.ts(petrol.ts,main="A Time Series Plot of Petrol Prices in USD", xlab="Time(Weeks)",ylab="Prices(USD)")
#Checking for stationarity diagramatically

par(mar=c(1,1,1,1),mfrow=c(1,3))
hist(petrol.ts,xlab = "Time(Weeks)",main = "A Histogram of the Petrol Prices(USD)")
pacf(petrol.ts,xlab="Lags",ylab="PACF",main="PACF Plot of the Series")
acf(petrol.ts,xlab="Lags",ylab="ACF",main="ACF Plot of the Series")

require(fpp2)
require(tseries)
#Testing for stationarity for the series
adf.test(petrol.ts)
#Checking for the number of differencing needed to stationarize the series
ndiffs(petrol.ts)

#Auto search of the appropriate model
auto.arima(petrol.ts)
#Model fitting
petrol.fit=arima(petrol.ts,order = c(1,1,1))

#Forecasting
petrol.pred=predict(petrol.fit,n.ahead=30)
summary(petrol.pred)
#Viewing predicted data
View(petrol.pred)

#Combining the observed data to the predicted data
petrol.combine = ts(c(petrol.ts,petrol.pred$pred), start = start(petrol.ts),frequency = frequency(petrol.ts))
View(petrol.combine)
summary(petrol.combine)
par(mfrow=c(1,1))
#Plotting the predicted data
plot.ts(petrol.pred$pred,main="Predicted prices(USD) for the next 30 weeks",xlab="Time(weeks)",ylab="Prices(USD)")

#Plotting the combined data, observed and forecasted
plot.ts(petrol.combine,main="Observed and forecasted time series plot for the petrol prices(USD)",xlab="Time(Weeks)",ylab="Prices(USD)")
