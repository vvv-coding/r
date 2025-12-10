library(forecast)
library(ggplot2)
library(TSA)
library(tseries)

perform_eda <- function(ts_data,dataset_name){
  plot(ts_data,main=paste(dataset_name,"Time Series"),ylab="Values",xlab="Time")
  acf(ts_data,main=paste("ACF of",dataset_name))
  pacf(ts_data,main=paste("PACF of",dataset_name))
}

decompose_ts <- function(ts_data,dataset_name){
  decomposition <- decompose(ts_data)
  plot(decomposition)
  return(decomposition)
}

fit_arima <- function(ts_data,dataset_name){
  adf_test <- adf.test(ts_data,alternative="stationary")
  if(adf_test$p.value>0.05){
    ts_data <- diff(ts_data)
    plot(ts_data,main=paste("Differential Time Series"))
  }
  auto_model <- auto.sarima(ts_data,seasonal=FALSE)
  forecast_result <- forecast(auto_model,h=12)
  plot(forecast_result,main=paste("ARIMA Forecast"))
  return(auto_model)
}

fit_sarima <- function(ts_data,dataset_name){
  auto_sarima <- auto.sarima(ts_data,seasonal=TRUE)
  sarima_forecast <- forecast(auto_sarima,h=12)
  plot(sarima_forecast,main=paste("Sarima"))
  return(auto_sarima)
}

compare_models <- function(arima_model,sarima_model,ts_data){
  h <- min(12,length(ts_data))
  arima_forecast <- forecast(arima_model,h=h)
  sarima_forecast <- forecast(sarima_model,h=h)
  actual_values <- ts_data[(length(ts_data)-h+1):length(ts_data)]
  arima_accuracy <- accuracy(arima_forecast$mean,actual_values)
  sarima_accuracy <- accuracy(sarima_forecast$mean,actual_values)
}

plot_forecast_comparison <- function(actual_values,arima_forecast,sarima_forecast,time_points){
  arima_rmse <- sqrt(mean(arima_forecast-actual_values)^2)
  sarima_rmse <- sqrt(mean(sarima_foreacst-actual_values)^2)
  better_color <- ifelse(arima_rmse<sarima_rmse,"red","green")
  worse_color <- ifelse(arima_rmse<sarima_rmse,"green","red")
  plot(time_points,actual_values,type="o",col="blue",pch=16,lty=1,xlab="Time",ylab="Values",main="Forecast Comparison")
  lines(time_points,arima_forecast,col=better_color,lty=2,lwd=2)
  line(time_points,sarima_forecast,col=worse_color,lty=3,lwd=2)
  legend("topright",legend=c("Actual values(RMSE=",arima_rmse),paste("SARIMA"),col=c("blue",better_color,worse_color,lty=c(1,2,3),
                                                                                     lwd=c(1,2,2),pch=c(16,NA,NA)))
}

data("AirPassengers")
air_data <- AirPassengers
perform_eda(air_data,"Air Passengers")
decompose_ts(air_data,"Air Passengers")
arima_air <- fit_arima(air_data,"Air Passengers")
sarima_air <- fit_sarima(air_data,"Air Passengers")
compare_models(arima_air,sarima_air,air_data)

h <- 12
air_actual_values <- air_data[(length(air_data)-h_air+1):length(air_data)]
arima_air_forecast <- foreacast(arima_air,h=h_air)$mean
sarima_air_forecast <- forecast(sarima_air,h=h_air)$mean
time_points_air <- time(air_data)[(length(air_data)-h_air+1):length(air_data)]
plot_forecast_comparison(air_actual_values,arima_air_forecast,sarima_air_forecast,time_points_air)