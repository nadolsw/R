#------------------------------------#
#        Correlation Functions       #
#                                    #
#           Dr Aric LaBarr           #
#          MSA Class of 2015         #
#------------------------------------#

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

# Drilling Down into the Needed Data Sets for Easy Variable Naming #
# and Creating Time Series Data Objects #
attach(USAirlines)
names(USAirlines)

Passenger <- ts(PASSENGE, start=1990, frequency=12)

attach(AR2)
names(AR2)

Y <- ts(Y)

# Lag Plot of Y #
lag.plot(Y, lag=2, main="Scatterplots of Y with First 2 Lags", diag=FALSE, layout=c(1,2))

# Correlation Functions #
Acf(Y, lag=10)$acf
Pacf(Y, lag=10)$acf

Acf(Passenger, lag=40, main="Autocorrelation Plot for US Airline Passengers")$acf
Pacf(Passenger, lag=40, main="Partial Autocorrelation Plot for US Airline Passengers")$acf


