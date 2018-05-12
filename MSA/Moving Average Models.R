#-------------------------------------#
#         Moving Average Models       #
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

MA1 <- read.ssd(file.path(timehome), "simma1", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(MA1)
names(MA1)

Y <- ts(Y)

# Building a Moving Average Model - MA Data #
MA.Model <- Arima(Y, order=c(0,0,1))
summary(MA.Model)

Acf(MA.Model$residuals, main="")$acf
Pacf(MA.Model$residuals, main="")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(MA.Model$residuals, lag=i, type="Ljung", fitdf=1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")
