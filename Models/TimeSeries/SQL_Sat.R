kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
#-Convert to Time Series Object
kingTSObj<-ts(kings)
#-Plot Time series 
plot.ts(kingTSObj)

#another dataset for birth  rate 
#- read data http://robjhyndman.com/tsdldata/data/nybirths.dat
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthTSObj<-ts(births,frequency=12, start=c(1946,1))
plot.ts(birthTSObj)
#Dcompose TS seasonal one
birthTSDecompose <- decompose(birthTSObj)
plot(birthTSDecompose)

#Dcompose not seasonal data

library("TTR")
kingsTSSMA<-SMA(kingTSObj,n=3)
plot.ts(kingsTSSMA)
kingsTSSMA<-SMA(kingTSObj,n=5)
plot.ts(kingsTSSMA)
kingsTSSMA<-SMA(kingTSObj,n=8)
plot.ts(kingsTSSMA)

#see Forecast trend - no trend no seasoality, constant level
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainTSObj <- ts(rain)
plot.ts(rainTSObj)
rainForecast<-HoltWinters(rainTSObj,beta=FALSE,gamma = FALSE)
rainForecast$fitted
plot(rainForecast)

#Try to forecast 
library("forecast")
rainForecast<-forecast::HoltWinters(rainForecast,h=8)
plot.forecast(rainForecast)

#analysis the error based on the current data 
rainForecast$residuals

#correlogram of the forecast errors using the “acf()” function 
acf(na.omit(rainForecast$residuals),lag.max =20)
plot.ts(rainForecast$residuals)

#ACF chart for a chart with trends
#Aside - see https://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/  (Seel below for function)
# https://robjhyndman.com/hyndsight/estimation2/
library(stats)
kingforecast<-HoltWinters(kingTSObj,gamma = FALSE)
plot(kingforecast)
kingforecatchart<-forecast(kingforecast,h=10)
plot(kingforecatchart)
acf(na.omit(kingforecatchart$residuals),lag.max = 20)

#ACF chart for both seasonality and 
birthForecast<-HoltWinters(birthTSObj)
plot(birthForecast)
birthForecastchart<-forecast(birthForecast,h=10)
plot(birthForecastchart)
acf(na.omit(birthForecastchart$residuals),lag.max = 20)
plot.ts(birthForecastchart$residuals)

#ARIMA model
plot.ts(kingTSObj)
plot.ts(kingTSObj)
kingArimaDiff<-diff(kingTSObj,kingTSObj=1)
plot.ts(kingArimaDiff)

#ARIMA Volcano
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20)

#ARIMA Forecast(0,1,1)
kingArimaforecast<-arima(kingTSObj,order=c(0,1,1))
KingArimaTSForecast<-forecast(kingArimaforecast,h=5)
plot(KingArimaTSForecast)

#ARIMA volcano forecast (2,0,0)
volcanoArima<-arima(volcanodustseries,order=c(2,0,0))
VOlcanoForecast<-forecast(volcanoArima,h=31)
plot(VOlcanoForecast)

plot.ts(birthTSObj)
birthdiff1<-diff(birthTSObj,birthTSObj=1)
plot.ts(birthdiff1)
acf(na.omit(birthdiff1),lag.max = 48)
pacf(na.omit(birthdiff1),lag.max = 48)
birthArima<-arima(birthTSObj,order=c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
birthforecsatArima<-forecast(birthArima,h=21)
plot(birthforecsatArima)

###############################

#https://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/
#Modified ggplot code to let it work
#Example data

demand <- ts(BJsales, start = c(2000, 1), frequency = 12)
HWplot(demand, n.ahead = 12)

#HWplot.R

library(ggplot2)
library(reshape)

HWplot <- function(ts_object, n.ahead = 4, CI = .95, error.ribbon = 'green', line.size = 1){
     hw_object <- HoltWinters(ts_object)
     forecast <- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
     for_values <- data.frame(time = round(time(forecast), 3), value_forecast = as.data.frame(forecast)$fit,  
             dev = as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
     fitted_values <- data.frame(time = round(time(hw_object$fitted), 3),  
             value_fitted = as.data.frame(hw_object$fitted)$xhat)
     actual_values <- data.frame(time = round(time(hw_object$x), 3), Actual = c(hw_object$x))
     
     graphset <- merge(actual_values, fitted_values, by='time', all=TRUE)
     graphset <- merge(graphset, for_values, all = TRUE, by = 'time')
     graphset[is.na(graphset$dev), ]$dev <- 0
     graphset$Fitted <-c (rep(NA, NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),
           fitted_values$value_fitted,  for_values$value_forecast)
     graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')

     
     p <- ggplot(graphset.melt, aes(x = time, y = value)) + 
          geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev), alpha=.2, 
                      fill=error.ribbon) + 
          geom_line(aes(colour=variable), size=line.size) + xlab('Time') + ylab('Value') + 
          theme(legend.position='bottom') + scale_colour_hue('') +
          geom_vline(xintercept = max(actual_values$time), lty=2)
     
     return(p)
}

#See http://ggplot2.tidyverse.org/reference/theme.html

#It's also very easy to adjust the graph after it is returned by the function;
graph <- HWplot(demand, n.ahead = 12, error.ribbon = "red")

# add a title
graph <- graph + labs(title="An example Holt-Winters (gg)plot")

# change the x scale a little
graph <- graph + scale_x_continuous(breaks = seq(1998, 2015))

# change the y-axis title
graph <- graph + ylab("Demand ($)")

# change the colour of the lines
graph <- graph + scale_color_brewer("Legend", palette = "Set1")

# the result:
graph
