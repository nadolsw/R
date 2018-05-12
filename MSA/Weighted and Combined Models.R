#-------------------------------------#
#     Weighted and Combined Models    #  
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
library(quantmod)


# Creating a MAPE Function #
MAPE <- function(y, yhat){
  mean(abs((y - yhat)/y))}


# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(USAirlines)
names(USAirlines)

Passenger <- ts(PASSENGE[1:195], start=1990, frequency=12)


# Building an ARIMA Model #
Sep11 <- rep(0,183)
Sep11[129] <- 1

Sep11.L1 <- rep(0,183)
Sep11.L1[130] <- 1

Sep11.L2 <- rep(0,183)
Sep11.L2[131] <- 1

Sep11.L3 <- rep(0,183)
Sep11.L3[132] <- 1

Sep11.L4 <- rep(0,183)
Sep11.L4[133] <- 1

Sep11.L5 <- rep(0,183)
Sep11.L5[134] <- 1

Sep11.L6 <- rep(0,183)
Sep11.L6[135] <- 1

Anniv <- rep(0,183)
Anniv[141] <- 1

Pass.ARIMA <- Arima(diff(Passenger,12), order=c(2,0,0), seasonal=c(0,0,1),xreg=cbind(Sep11,Sep11.L1,Sep11.L2,Sep11.L3,Sep11.L4,Sep11.L5,Sep11.L6,Anniv), method="ML")
summary(Pass.ARIMA)

Sep11.For <- rep(0,24)
Anniv.For <- rep(0,24)
forecast(Pass.ARIMA, h=24, xreg=cbind(Sep11.For,Anniv.For))

Pass.For.ARIMA <- rep(NA, 24)

for(i in 1:12){
  Pass.For.ARIMA[i] <- Passenger[length(Passenger)-12+i]+forecast(Pass.ARIMA, h=24, xreg=cbind(Sep11.For,Anniv.For))$mean[i]
}

for(i in 13:24){
  Pass.For.ARIMA[i] <- Pass.Forecast[i-12]+forecast(Pass.ARIMA, h=24, xreg=cbind(Sep11.For,Anniv.For))$mean[i]
}

Pass.For.ARIMA <- ts(Pass.For.ARIMA, start=c(2008, 4), frequency=12)
Pass.Validation <- ts(PASSENGE[196:219], start=c(2008, 4), frequency=12)

MAPE(Pass.Validation, Pass.For.ARIMA)
