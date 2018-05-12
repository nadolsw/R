#-------------------------------------#
#         Autoregressive Models       #
#                                     #
#            Dr Aric LaBarr           #
#           MSA Class of 2015         #
#-------------------------------------#

# Needed Libraries for Analysis #
library(foreign)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(dyn)

# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))
AR2 <- read.ssd(file.path(timehome), "ar2", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(USAirlines)
names(USAirlines)

attach(AR2)
names(AR2)

Passenger <- ts(PASSENGE, start=1990, frequency=12)
Y <- ts(Y)

# Building an Autoregressive Model - AR Data #
AR.Model <- Arima(Y, order=c(2,0,0))
summary(AR.Model)

Acf(AR.Model$residuals, main="")$acf
Pacf(AR.Model$residuals, main="")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(AR.Model$residuals, lag=i, type="Ljung", fitdf=2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")

# Building an Autoregressive Model - US Airline Data #
AR.Model1 <- Arima(Passenger, order=c(1,0,0))
AR.Model2 <- Arima(Passenger, order=c(6,0,0))
summary(AR.Model1)
summary(AR.Model2)

Acf(AR.Model$residuals, main="")$acf
Pacf(AR.Model$residuals, main="")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(AR.Model$residuals, lag=i, type="Ljung", fitdf=2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")

