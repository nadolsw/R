#----------------------------------#
#   Estimation Using Delta Normal  #
#      & Historical Simulation     #
#                                  #
#           Dr Aric LaBarr         #
#----------------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(haven)
library(ks)
library(scales)
library(read_sas)
library(sas7bdat)

# Load Stock Data From SAS #
stocks <- sas7bdat('C:/Users/William/Desktop/NCSU MSA/Fall 2015/Simulation & Risk/Code/get_stocks.sas')
stocks <- read_sas('E:/Courses/Simulation and Risk/Data/stocks.sas7bdat')

# Stock Information #
msft.holding <- 1700
aapl.holding <- 2500
VaR.percentile <- 0.05

# Calculate Needed Variances and Covariances #
var.msft <- var(stocks$msft_r, na.rm=TRUE)
var.aapl <- var(stocks$aapl_r, na.rm=TRUE)
cov.m.a <- cov(stocks$msft_r, stocks$aapl_r, use="pairwise.complete.obs")
cor.m.a <- cor(stocks$msft_r, stocks$aapl_r, use="pairwise.complete.obs")

# Calculate Current Total Value of Holdings (Portfolio) #
msft.position <- msft.holding*stocks$msft_p[length(stocks$msft_p)]
aapl.position <- aapl.holding*stocks$aapl_p[length(stocks$aapl_p)]

# Calculate Portfolio Holding Weights #
msft.p.weight <- msft.position/(aapl.position + msft.position)
aapl.p.weight <- aapl.position/(aapl.position + msft.position)

# Calculate Portfolio Variance #
var.port <-  var.msft*(msft.p.weight)**2 + var.aapl*(aapl.p.weight)**2 + 2*aapl.p.weight*msft.p.weight*cov.m.a
sd.port <- sqrt(var.port)

# Confidence Intervals for Portfolio Standard Deviation #
sigma.low <- sqrt(var.port*(length(stocks$msft_p)-1)/qchisq((1-(VaR.percentile/2)),length(stocks$msft_p)-1) )
sigma.up <- sqrt(var.port*(length(stocks$msft_p)-1)/qchisq((VaR.percentile/2),length(stocks$msft_p)-1) )

# Calculate Portfolio's Value at Risk, VaR CI, and Conditional Value at Risk #
VaR.normal <- (msft.position + aapl.position)*qnorm(VaR.percentile)*sqrt(var.port)
VaR.L <-  (msft.position + aapl.position)*qnorm(VaR.percentile)*(sigma.low)
VaR.U <- (msft.position + aapl.position)*qnorm(VaR.percentile)*(sigma.up)

dollar(VaR.L)
dollar(VaR.normal)
dollar(VaR.U)

ES.normal <- -(msft.position + aapl.position)*sqrt(var.port)*exp(-0.5*(qnorm(VaR.percentile))**2)/(VaR.percentile*sqrt(2*pi));
dollar(ES.normal)

# Historical Simulation Approach #
portfolio.value <- msft.position*stocks$msft_r + aapl.position*stocks$aapl_r
portfolio.value <- portfolio.value[order(portfolio.value)]
VaR.historical <- quantile(portfolio.value, VaR.percentile, na.rm=TRUE)
dollar(VaR.historical)

ES.historical <- mean(portfolio.value[portfolio.value < VaR.historical], na.rm=TRUE)
dollar(ES.historical)
