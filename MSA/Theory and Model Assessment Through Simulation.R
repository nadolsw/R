#-----------------------------#
# Theory and Model Assessment #
#      Through Simulation     #
#                             #
#           Dr LaBarr         #
#-----------------------------#

# Needed Libraries for Analysis #
library(graphics)

# Theory Assessment - CLT #
sample.size <- 100
simulation.size <- 10000

X1 <- matrix(data=rnorm(n=(sample.size*simulation.size), mean=2, sd=5), nrow=simulation.size, ncol=sample.size, byrow=TRUE)
X2 <- matrix(data=runif(n=(sample.size*simulation.size), min=5, max=105), nrow=simulation.size, ncol=sample.size, byrow=TRUE)
X3 <- matrix(data=(rexp(n=(sample.size*simulation.size)) + 3), nrow=simulation.size, ncol=sample.size, byrow=TRUE)

Mean.X1 <- apply(X1,1,mean)
Mean.X2 <- apply(X2,1,mean)
Mean.X3 <- apply(X3,1,mean)

hist(Mean.X1, breaks=50, main='Sample Distribution of Means for Normal Distribution', xlab='Sample Means')
hist(Mean.X2, breaks=50, main='Sample Distribution of Means for Uniform Distribution', xlab='Sample Means')
hist(Mean.X3, breaks=50, main='Sample Distribution of Means for Exponential Distribution', xlab='Sample Means')

# Target Shuffling #
Fake <- data.frame(matrix(rnorm(n=(100*8)), nrow=100, ncol=8))
Err <- rnorm(n=100, mean=0, sd=8)
Y <- 5 + 2*Fake$X2 - 3*Fake$X8 + Err
Fake <- cbind(Fake, Err, Y)

sim <- 1000

Y.Shuffle <- matrix(0, nrow=100, ncol=sim)
for(j in 1:sim){
  Uniform <- runif(100)
  Y.Shuffle[,j] <- Y[order(Uniform)]
}

Y.Shuffle <- data.frame(Y.Shuffle)
colnames(Y.Shuffle) <- paste('Y.',seq(1:sim),sep="")

Fake <- data.frame(Fake, Y.Shuffle)

R.sq.A <- rep(0,sim)
for(i in 1:sim){
  R.sq.A[i] <- summary(lm(Fake[,10+i] ~ Fake$X1 + Fake$X2 + Fake$X3 + Fake$X4
                          + Fake$X5 + Fake$X6 + Fake$X7 + Fake$X8))$adj.r.squared
}
True.Rsq.A <- summary(lm(Fake$Y ~ Fake$X1 + Fake$X2 + Fake$X3 + Fake$X4
            + Fake$X5 + Fake$X6 + Fake$X7 + Fake$X8))$adj.r.squared

hist(c(R.sq.A,True.Rsq.A), breaks=50, main='Distribution of Adjusted R-Squared Values', xlab='Adjusted R-Squared')
abline(v = True.Rsq.A, col="red", lwd=2)
mtext("True Model", at=True.Rsq.A, col="red")

