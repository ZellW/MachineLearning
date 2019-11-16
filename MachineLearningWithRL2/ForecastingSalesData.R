#-----------Forecasting---------------
library(quantmod)
sales <-scan("http://robjhyndman.com/tsdldata/data/sales.dat")
sales
# store in timeseries Object
salestimeseries<-ts(sales,frequency=12,start=c(1987,1))
salestimeseries

# Check whether it is an additive/ multiplicative series
plot.ts(salestimeseries)

 # Log transformation to additive timeseries
logsalestimeseries<-log(salestimeseries)
plot.ts(logsalestimeseries)

#Decompose a timeseries into parts
plot(decompose(logsalestimeseries))

# plot a timeseries forecasting graph
library(forecast)
ets1<-ets(logsalestimeseries,model="MMM")
fcast<-forecast(ets1)
plot(fcast,lwd=3,col="red",type="o")
