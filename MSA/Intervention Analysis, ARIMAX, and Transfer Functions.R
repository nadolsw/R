#-------------------------------------#
#    Intervention Analysis, ARIMAX    #
#         & Transfer Functions        #  
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
library(tsa)


# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4/"
timehome <- "E:/Courses/Time Series/Data"

Deer <- read.ssd(file.path(timehome), "DEER", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(Deer)
names(Deer)

Deer.Accidents <- ts(DEER, start=2003, frequency=12)

plot(Deer.Accidents)


# Point (Pulse) Intervention - Deterministic #
Deer.Model1 <- Arima(Deer.Accidents, xreg=NOV, method="ML")
summary(Deer.Model1)

For.Month <- rep(0,24) + 1:12
For.Nov <- rep(0,24)
for(i in 1:24){
  if(For.Month[i]==11){For.Nov[i]=1}else{For.Nov[i]=0}
}

forecast(Deer.Model1, h=24, xreg=For.Nov)
plot(forecast(Deer.Model1, h=24, xreg=For.Nov))

# Point (Pulse) Intervention - Stochastic + ARIMA Model #
Deer.Model2 <- arimax(Deer.Accidents, order=c(12,0,0), xtransf=NOV, transfer=list(c(1,0)), method='ML')
summary(Deer.Model2)

# ARIMAX in R Doesn't have any Forecasting Ability :( #

# Building a Simple Transfer Function Model #
Housing <- read.ssd(file.path(timehome), "HOUSING", sascmd=file.path(sashome, "sas.exe"))

detach(Deer)
attach(Housing)
names(Housing)

Starts <- ts(STARTS, start=1974)

ADF.Pvalues <- rep(NA, 6)
for(i in 0:5){
  ADF.Pvalues[i+1] <- adf.test(Starts, alternative="stationary", k=i)$p.value
}
ADF.Pvalues

Acf(diff(Starts))
Pacf(diff(Starts))

Starts.Model <- Arima(diff(Starts), order=c(4,0,3), method="ML")
summary(Starts.Model)
For.Starts <- forecast(Starts.Model, h=8)

Sales <- ts(SALES, start=1974)

ADF.Pvalues <- rep(NA, 6)
for(i in 0:5){
  ADF.Pvalues[i+1] <- adf.test(Sales, alternative="stationary", k=i)$p.value
}
ADF.Pvalues

Acf(diff(Sales))
Pacf(diff(Sales))

ccf(diff(Starts), diff(Sales))

Sales.Model <- Arima(diff(Sales), order=c(0,0,0), xreg=diff(Starts))
summary(Sales.Model)

Acf(Sales.Model$residuals)
Pacf(Sales.Model$residuals)

Sales.Model2 <- Arima(diff(Sales), order=c(0,0,1), xreg=diff(Starts))
summary(Sales.Model2)

For.Sales <- forecast(Sales.Model2, h=8, xreg=For.Starts$mean)
plot(For.Sales)


# Building a General Transfer Function Model #

##############################################
Transfer <- read.ssd(file.path(timehome), "Transfer", sascmd=file.path(sashome, "sas.exe"))

detach(River)
attach(Transfer)
names(Transfer)

acf(X, lag=10, main="Series Autocorrelations for X")
pacf(X, lag=10, main="Series Partial Autocorrelations for X")

X.Model <- Arima(X, order=c(1,0,0), method="ML")
White.Y <- filter(Y, filter=c(1, -0.5089), sides=1)


ccf(X.Model$residuals[2:500], White.Y[2:500], main="Crosscorrelations of Y and X on Prewhitened Series")


River <- read.ssd(file.path(timehome), "River", sascmd=file.path(sashome, "sas.exe"))

detach(Transfer)
attach(River)
names(River)

Log.Kins <- ts(LKINS)
Log.Gold <- ts(LGOLD)

X.Model <- Arima(Log.Gold, order=c(2,1,2), method="ML")
White.Y <- filter(diff(Log.Kins), filter=c(1, (-1-1.482), (1.482+0.6164),-0.6164), sides=1)


ccf(White.Y[4:399], X.Model$residuals[4:399])
