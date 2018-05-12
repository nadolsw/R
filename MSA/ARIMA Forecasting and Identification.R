#-------------------------------------#
#           ARIMA Forecasting         #
#           & Identification          #
#                                     #
#            Dr Aric LaBarr           #
#-------------------------------------#

# Needed Libraries for Analysis #
library(foreign)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(caschrono)
library(TSA)

# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

SimARMA <- read.ssd(file.path(timehome), "SimARMA", sascmd=file.path(sashome, "sas.exe"))
USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(SimARMA)
names(SimARMA)

attach(USAirlines)
names(USAirlines)

Passenger <- ts(PASSENGE, start=1990, frequency=12)

Y <- ts(Y)


# Building an Autoregressive Moving Average Model #
Acf(Y, lag=10)
Pacf(Y, lag=10)

ARMA.Model <- Arima(Y, order=c(1,0,1))

Acf(ARMA.Model$residuals, main="")$acf
Pacf(ARMA.Model$residuals, main="")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(ARMA.Model$residuals, lag=i, type="Ljung", fitdf=1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")


Acf(Passenger, lag=40)
Pacf(Passenger, lag=40)

nsdiffs(Passenger)
ndiffs(diff(Passenger, lag=12))

adf.test(diff(Passenger,12), alternative="stationary", k=0) 

Pass.Model <- Arima(diff(Passenger,12), order=c(2,0,0), seasonal=c(1,0,1), method="ML")

Acf(Pass.Model$residuals, main="")$acf
Pacf(Pass.Model$residuals, main="")$acf

White.LB <- rep(NA, 40)
for(i in 1:40){
  White.LB[i] <- Box.test(Pass.Model$residuals, lag=i, type="Ljung", fitdf=12)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")


# Automatic Model Identification #
adf.test(diff(Passenger,12), alternative="stationary", k=0) 

auto.arima(diff(Passenger,12))

armaselect(diff(Passenger,12), max.p=12, max.q=12)

eacf(diff(Passenger,12), ar.max=12, ma.max=12) 


# Forecasting #
forecast(Pass.Model, h=24)
plot(forecast(Pass.Model, h=24))

# Better Forecast Plot #
Pass.Forecast <- rep(NA, 24)

for(i in 1:12){
  Pass.Forecast[i] <- Passenger[length(Passenger)-12+i]+forecast(Pass.Model, h=24)$mean[i]
}

for(i in 13:24){
  Pass.Forecast[i] <- Pass.Forecast[i-12]+forecast(Pass.Model, h=24)$mean[i]
}

Pass.Forecast <- ts(Pass.Forecast, start=c(2008, 4), frequency=12)

plot(Passenger, main="US Airline Passengers ARIMA Model Forecasts", xlab="Date", ylab="Passengers (Thousands)", xlim=c(1990, 2010), ylim=c(30000,75000))
lines(Pass.Forecast, col="blue")
abline(v=2008.25, col="red", lty="dashed")


