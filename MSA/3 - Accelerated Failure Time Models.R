#-------------------------------------#
#      Accelerated Failure Time       #
#                 Models              #
#                                     #
#            Matthew Austin           #
#           MSA Class of 2016         #
#-------------------------------------#

# Needed Libraries for Analysis #
library(survival)
library(foreign)


# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
survhome <- "E:/Courses/Survival Analysis/Data"

Loyalty <- read.ssd(file.path(survhome), "Loyalty", sascmd=file.path(sashome, "sas.exe"))
Recid <- read.ssd(file.path(survhome), "recid", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(Loyalty)
names(Loyalty)


# Create a Survival Analysis Object #
Loyal.Surv <- Surv(TENURE, CENSORED==0)


# Building an AFT Model #
Loyal.AFT <- survreg(Loyal.Surv ~ AGE + LOYALTY + INCOME, dist='lognormal')
summary(Loyal.AFT)


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
detach(Loyalty)
attach(Recid)
names(Recid)

# Create a Survival Analysis Object #
Recid.Surv <- Surv(WEEK, ARREST==1)

# AFT Model - Exponential Distribution #
Recid.AFT <- survreg(Recid.Surv ~ FIN + AGE + RACE + WEXP + MAR + PARO + PRIO, dist='exponential')
summary(Recid.AFT)

# AFT Model - Weibull Distribution #
Recid.AFT <- survreg(Recid.Surv ~ FIN + AGE + RACE + WEXP + MAR + PARO + PRIO, dist='weibull')
summary(Recid.AFT)

Recid.AFT <- survreg(Recid.Surv ~ FIN + AGE + PRIO, dist='weibull')
summary(Recid.AFT)

Surv.Prob <- exp(-(WEEK*exp(-(coef(Recid.AFT)[1] + coef(Recid.AFT)[2]*FIN + coef(Recid.AFT)[3]*AGE + coef(Recid.AFT)[4]*PRIO)))^(1/Recid.AFT$scale))

Old.t <- (-log(Surv.Prob))^Recid.AFT$scale*exp(coef(Recid.AFT)[1] + coef(Recid.AFT)[2]*FIN + coef(Recid.AFT)[3]*AGE + coef(Recid.AFT)[4]*PRIO)

New.t <- (-log(Surv.Prob))^Recid.AFT$scale*exp(coef(Recid.AFT)[1] + coef(Recid.AFT)[2] + coef(Recid.AFT)[3]*AGE + coef(Recid.AFT)[4]*PRIO)

Difference <- New.t - Old.t
summary(Difference[which(FIN==0)])

# Goodness-of-Fit Tests #
Recid.KP <- survfit(Recid.Surv ~ 1)

Log.Surv <- -log(Recid.KP$surv)
LogLog.Surv <- log(-log(Recid.KP$surv))
Logit.Surv <- log((1-Recid.KP$surv)/Recid.KP$surv)
LNorm.Surv <- qnorm(1-Recid.KP$surv)
Log.Week <- log(Recid.KP$time)

plot(Recid.KP$time, Log.Surv)
plot(Log.Week, LogLog.Surv)
plot(Log.Week, Logit.Surv)
plot(Log.Week, LNorm.Surv)
