#------------------------------------#
#        Exponential Smoothing       #
#               Models               #
#                                    #
#           Dr Aric LaBarr           #
#------------------------------------#

# Needed Libraries for Analysis #
library(foreign)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)

# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))
Steel <- read.ssd(file.path(timehome), "steel", sascmd=file.path(sashome, "sas.exe"))

# Drilling Down into the Needed Data Sets for Easy Variable Naming #
# and Creating Time Series Data Objects #
attach(USAirlines)
names(USAirlines)

Passenger <- ts(PASSENGE, start=1990, frequency=12)

attach(Steel)
names(Steel)

SteelShp <- ts(STEELSHP, start=1984, frequency=12)

# Building a Single Exponential Smoothing Model - Steel Data #
SES.Steel <- ses(SteelShp, initial="optimal", h=24)
summary(SES.Steel)
plot(SES.Steel, main="US Steel Shipments with Simple ESM Forecast", xlab="Date", ylab="Shipments (Thousands of Net Tons)")
abline(v=1992, col="red", lty="dashed")

# Ljung-Box Test for Steel ES Model #
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(SES.Steel$residuals, lag=i, type="Ljung", fitdf=1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")

# Building a Linear Exponential Smoothing Model - Steel Data #
LES.Steel <- holt(SteelShp, initial="optimal", h=24)
summary(LES.Steel)
plot(LES.Steel, main="US Steel Shipments with Linear ESM Forecast", xlab="Date", ylab="Shipments (Thousands of Net Tons)")
abline(v=1992, col="red", lty="dashed")

LDES.Steel <- holt(SteelShp, initial="optimal", h=24, damped=TRUE)
summary(LDES.Steel)
plot(LDES.Steel, main="US Steel Shipments with Linear Damped ESM Forecast", xlab="Date", ylab="Shipments (Thousands of Net Tons)")
abline(v=1992, col="red", lty="dashed")

# Building a Linear Exponential Smoothing Model - US Airlines Data #
LES.USAir <- holt(Passenger, initial="optimal", h=24)
summary(LES.USAir)
plot(LES.USAir, main="US Airline Passengers with Linear ESM Forecast", xlab="Date", ylab="Passengers (Thousands)")
abline(v=2008.25, col="red", lty="dashed")

LDES.USAir <- holt(Passenger, initial="optimal", h=24, damped=TRUE)
summary(LDES.USAir)
plot(LDES.USAir, main="US Airline Passengers with Linear Damped ESM Forecast", xlab="Date", ylab="Passengers (Thousands)")
abline(v=2008.25, col="red", lty="dashed")

# Building a Holt-Winters ESM - US Airlines Data #
HWES.USAir <- hw(Passenger, seasonal="additive")
summary(HWES.USAir)
plot(HWES.USAir, main="US Airline Passengers with Holt-Winters ESM Forecast", xlab="Date", ylab="Passengers (Thousands)")
abline(v=2008.25, col="red", lty="dashed")

HWES.USAir <- hw(Passenger, seasonal="multiplicative")
summary(HWES.USAir)
plot(HWES.USAir, main="US Airline Passengers with Holt-Winters ESM Forecast", xlab="Date", ylab="Passengers (Thousands)")
abline(v=2008.25, col="red", lty="dashed")