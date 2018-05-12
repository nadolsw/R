#-------------------------------------#
#      Survival Curves, Censoring,    #
#        & Hazard Probabilities       #
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

attach(Recid)
names(Recid)

# Create a Survival Analysis Object #
Loyal.Surv <- Surv(TENURE, CENSORED==0)
Recid.Surv <- Surv(WEEK, ARREST==1)

# Create a Kaplan-Meier Survival Curve with Censoring #
Loyal.KP <- survfit(Loyal.Surv ~ 1)
plot(Loyal.KP)

Loyal.KP2 <- survfit(Loyal.Surv ~ LOYALTY)
plot(Loyal.KP2, conf.int=TRUE, col=c("red", "blue"))
legend('topright', c("Loyalty Program", "None Loyalty Program"), lty=1, col=c("blue", "red"))

Recid.KP <- survfit(Recid.Surv ~ 1)
plot(Recid.KP)

# Test for Differences in Survival Curves #
survdiff(Loyal.Surv ~ LOYALTY, rho=0) # Log-Rank Test #
survdiff(Loyal.Surv ~ LOYALTY, rho=1) # Wilcoxon Test #

# Calculating Hazard Probabilities #
h <- Loyal.KP$n.event/Loyal.KP$n.risk
plot(Loyal.KP$time, h, type="l", ylab="Estimated Hazard Probaility", xlab="Tenure")

