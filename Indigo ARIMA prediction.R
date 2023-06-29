# Data loading
indigo_data = read.csv("C://Users//amith//Documents//final2.csv")
indigo_data = ts(indigo_data,start =1, end =25)
print(indigo_data)
plot(indigo_data)

#plotting acf and pacf graph
acf(indigo_data)
pacf(indigo_data)

#remove non-stationarity of the data
plot(diff(indigo_data))

#best ARIMA model c(0,1,1)
arima(indigo_data,order = c(1,1,1))
arima(indigo_data,order = c(0,1,1))
arima(indigo_data,order = c(1,1,0))
arima(indigo_data,order = c(0,1,0))

fit = arima(indigo_data,order =c(0,0,1))
summary(fit)
predict(fit,n.ahead =7)

#checking seasonal ARIMA
arima(indigo_data,seasonal = list(order = c(1,1,1),period =4))
arima(indigo_data,seasonal = list(order = c(2,1,2),period =4))
arima(indigo_data,seasonal = list(order = c(0,1,1),period =4))
arima(indigo_data,seasonal = list(order = c(1,1,0),period =4))


#best c(0,1,1)
fit  = arima(indigo_data,seasonal = list(order = c(0,1,1),period =4))
summary(fit)
predict(fit, n.ahead =7)

#command for model validation - box test
Box.test(fit$residuals, lag=5)  # checking p value of the previous value
