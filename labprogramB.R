library(tseries)
library(ggplot2)
library(forecast)

data("AirPassengers")
str(AirPassengers)

summary(AirPassengers)
head(AirPassengers,12)
class(AirPassengers)
frequency(AirPassengers)

start(AirPassengers); end(AirPassengers)

cycle(AirPassengers)

sum(is.na(AirPassengers))

plot(AirPassengers,
     main="Monthly Airline Passengers (1949-1960)",
     ylab="Passengers (thousands)",
     xlab="Year",col="blue",lwd=2)

adf_result <- adf.test (AirPassengers,alternative="stationary")
kpss_result <- kpss.test(AirPassengers)
kpss_result

log_ap <- log(AirPassengers)
diff_seasonal <- diff(log_ap,lag=12)
diff_full <- diff(diff_seasonal,difference=1)

par(mfrows=c(3,1))
plot(log_ap,main="Log Transformed Series",col="darkred")
plot(diff_seasonal,main="After Seasonal Differencing",col="darkgreen")
plot(diff_full,main="After regular and Seasonal Differencing",col="blue")

fit_sarima = auto.arima(AirPassengers,seasonal=TRUE,stepwise=FALSE,
                        approximation=FALSE,trace=TRUE)

summary(fit_sarima)
checkresiduals(fit_sarima)

forecast_sarima <- forecast(fit_sarima,h=36)

plot(forecast_sarima,main="Air Passengers Forecast:1961-1963",
     xlab="Year",ylab="Passengers (Thousands)",col="blue",lwd=2)
lines(fitted(fit_sarima),col="red",lty=2)
legend("topleft",legend=c("Observed","Fitted","Forecast"),col=c("black","red","blue"),lty=c(1,2,1),cex=0.8)

accuracy(forecast_sarima)