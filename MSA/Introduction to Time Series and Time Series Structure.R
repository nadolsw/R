#------------------------------------#
#    Introduction to Time Series     #
#      & Time Series Structure       #
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

# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))
AR2 <- read.ssd(file.path(timehome), "ar2", sascmd=file.path(sashome, "sas.exe"))

# Drilling Down into US Airline Data Set for Easy Variable Naming #
attach(USAirlines)
names(USAirlines)

# Creation of Time Series Data Object #
Passenger <- ts(PASSENGE, start=1990, frequency=12)

# Time Series Decomposition #
model <- stl(Passenger, s.window=5)
plot(model)

plot(Passenger, col="grey", main="US Airline Passengers - Trend/Cycle", xlab="", ylab="Number of Passengers (Thousands)", lwd=2)
lines(model$time.series[,2],col="red", lwd=2)

plot(Passenger, col="grey", main="US Airline Passengers - Seasonally Adjusted", xlab="", ylab="Number of Passengers (Thousands)", lwd=2)
lines(seasadj(model),col="red", lwd=2)

monthplot(model$time.series[,"seasonal"], main="US Airline Passengers - Monthly Effects", ylab="Seasonal Sub Series", xlab="Seasons (Months)", lwd=2)

# White Noise Tests #
attach(AR2)
names(AR2)

Box.test(Y, lag=6, type="Ljung")

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(Y, lag=i, type="Ljung")$p.value
}

barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.5))
abline(h=0.01, lty="dashed", col="grey")
abline(h=0.05, lty="dashed", col="grey")

Y.Model <- Arima(Y, order=c(2,0,0))

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(Y.Model$residuals, lag=i, type="Ljung", fitdf=2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")
