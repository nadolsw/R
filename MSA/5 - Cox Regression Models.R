#-------------------------------------#
#         Cox Regression Models       #
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
attach(Recid)
names(Recid)


# Create a Survival Analysis Object #
Recid.Surv <- Surv(WEEK, ARREST==1)


# Building a Proportional Hazards Model #
Recid.Cox <- coxph(Recid.Surv ~ FIN + AGE + RACE + WEXP + MAR + PARO + PRIO)
summary(Recid.Cox)


# Time-Varying Variables #
EMPLOYED <- rep(NA, length(WEEK))
for(i in 1:length(WEEK)){
  EMPLOYED[i] <- Recid[i,10+WEEK[i]]
}

Recid.Cox2 <- coxph(Recid.Surv ~ FIN + AGE + RACE + WEXP + MAR + PARO + PRIO + EMPLOYED)
summary(Recid.Cox2)


# Non-Proportional Hazards Model - cox.zph Function Tests ALL Variables Inidividually for Assumption as well as Provides a Global Test for Assumption #
cox.zph(Recid.Cox)


# Outliers and Influential Observations #
plot(cox.zph(Recid.Cox)) # Schoenfeld Residuals #

dfbeta <- residuals(Recid.Cox, type="dfbeta")
dfbeta

for(i in 1:7){
  plot(dfbeta[,i], ylab=names(coef(Recid.Cox))[i])
}
