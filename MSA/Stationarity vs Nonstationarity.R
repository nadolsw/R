#-------------------------------------#
#  Stationarity vs. Non-stationarity  #
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
library(dyn)

# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "E:/Courses/Time Series/Data"

USAirlines <- read.ssd(file.path(timehome), "usair", sascmd=file.path(sashome, "sas.exe"))
Lead.Year <- read.ssd(file.path(timehome), "leadyear", sascmd=file.path(sashome, "sas.exe"))
Ebay <- read.ssd(file.path(timehome), "ebay9899", sascmd=file.path(sashome, "sas.exe"))

# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(Lead.Year)
names(Lead.Year)

Lead <- ts(Lead.Year, start=1972)

# Linear Trend #
Lin.Model <- lm(PRIMARY ~ TIME)
summary(Lin.Model)
plot(Lin.Model)

Lin.Model2 <- Arima(PRIMARY, order=c(0,0,0), xreg=TIME)
summary(Lin.Model2)

Acf(residuals(Lin.Model2))$acf
Pacf(residuals(Lin.Model2))$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(Lin.Model2$residuals, lag=i, type="Ljung", fitdf=0)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")

# Quadratic Trend #
Quad.Model <- Arima(PRIMARY, order=c(0,0,0), xreg=cbind(TIME, I(TIME^2)))
summary(Quad.Model)

Acf(residuals(Quad.Model))$acf
Pacf(residuals(Quad.Model))$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(Quad.Model$residuals, lag=i, type="Ljung", fitdf=0)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags", ylim=c(0,0.2))
abline(h=0.01, lty="dashed", col="black")
abline(h=0.05, lty="dashed", col="black")

# Stochastic Trend - Differencing #
plot.ts(diff(PRIMARY), main="Plot of Difference of Primary", ylab="Difference of Primary Production")
Acf(diff(PRIMARY), main="")$acf
Pacf(diff(PRIMARY), main="")$acf

# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
detach(Lead.Year)
attach(USAirlines)
names(USAirlines)

Passenger <- ts(PASSENGE, start=1990, frequency=12)

# Stochastic Seasons - Differencing #
plot.ts(diff(Passenger, lag=12), main="US Airline Passengers", ylab="Annual Change of US Airline Passengers", xlab="Year")
Acf(diff(Passenger, lag=12), main="")$acf
Pacf(diff(Passenger, lag=12), main="")$acf

plot.ts(diff(diff(Passenger, lag=12),lag=1), main="US Airline Passengers", ylab="Monthly Change of Annual Change of US Airline Passengers", xlab="Year")
Acf(diff(diff(Passenger, lag=12),lag=1), main="")$acf
Pacf(diff(diff(Passenger, lag=12),lag=1), main="")$acf

# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
detach(USAirlines)
attach(Ebay)
names(Ebay)

Daily.High <- ts(na.omit(DAILYHIG))

# Augmented Dickey-Fuller Testing #
adf.test(Daily.High, alternative="stationary", k=0)

ADF.Pvalues <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(Daily.High, alternative="stationary", k=i)$p.value
}

# Automated Differencing Test Function #
ndiffs(Daily.High)

# Automated Seasonal Differencing Test Function #
nsdiffs(Passenger)
ndiffs(diff(Passenger, lag=12))
