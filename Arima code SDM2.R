library(tseries)
library(forecast)
library(lubridate)
data = read.csv(file.choose(), header = TRUE)
new_data = data[4408:5483,] #Data from Jan 2017 to Jan 2021
close_prices = new_data[,5]

par(mfrow=c(1,2))
acf(new_data$Close, main='acf of differened series')
pacf(new_data$Close, main='pacf of differened series')

auto.arima(,seasonal=TRUE)

fit_model = auto.arima(new_data$Close,seasonal = TRUE)
tsdisplay(residuals(fit_model),lag.max=40, main = '(0,1,1) model')

fit_2 = arima(close_prices, order = c(1,1,0))
tsdisplay(residuals(fit_model),lag.max=40, main = '(1,1,0) model')

fcast <- forecast(fit_model,270)
plot(fcast)

fcast2 = forecast(fit_2,270)
plot(fcast2)

library(dplyr) 


Fcast_df <- fcast %>% 
  sweep::sw_sweep(.) %>% 
  filter(key == "forecast") %>% 
  select(-key)
compare_data = data[4408:5723,]
closing_compare = compare_data[,5]
fcast
#par(mfrow=c(1,2))

#plot(closing_compare,type='l',ylim=c(0,4500),main='Actual values')
plot(fcast2,col='blue',ylab='Closing Price',xlab='Date',type ='l')
lines(compare_data$Close,col="red",type='l')
