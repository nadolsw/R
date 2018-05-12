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


# Load Stock Data From SAS #
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

# Calculate Current Price of Holdings (Portfolio) #
msft.p <- stocks$msft_p[length(stocks$msft_p)]
aapl.p <- stocks$aapl_p[length(stocks$aapl_p)]

# Monte Carlo Simulation Approach #
n.simulations <- 10000
R <- matrix(data=cbind(1,cor.m.a, cor.m.a, 1), nrow=2)
U <- t(chol(R))

msft.r <- rnorm(n=n.simulations, mean=0, sd=sqrt(var.msft))
aapl.r <- rnorm(n=n.simulations, mean=0, sd=sqrt(var.aapl))
Both.r <- cbind(msft.r, aapl.r)
port.r <- U %*% t(Both.r)
port.r <- t(port.r)
  
value <- msft.holding*(exp(msft.r + log(msft.p))) + aapl.holding*(exp(aapl.r + log(aapl.p)))
value.change = value - (msft.holding*msft.p + aapl.holding*aapl.p)

VaR <- quantile(value.change, VaR.percentile, na.rm=TRUE)
VaR.label <- dollar(VaR)

hist(value.change, breaks=50, main='1 Day Value Change Distribution', xlab='Value Change')
abline(v = VaR, col="red", lwd=2)
mtext(paste("Value at Risk",VaR.label, sep=" = "), at=VaR, col="red")

ES <- mean(value.change[value.change < VaR], na.rm=TRUE)
dollar(ES)

# Confidence Intervals for Value at Risk & Expected Shortfall - Bootstrap Approach #
n.bootstraps <- 1000
sample.size <- 1000

VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(value.change, size=sample.size)
  VaR.boot[i] <- quantile(bootstrap.sample, VaR.percentile, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)
dollar(VaR.boot.L)
dollar(VaR)
dollar(VaR.boot.U)

ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
dollar(ES.boot.L)
dollar(ES)
dollar(ES.boot.U)
