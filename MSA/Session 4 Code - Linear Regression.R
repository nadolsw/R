#--------------------------#
#   R Workshop Session 4   #
#                          #
#         Dr LaBarr        #
#     MSA Class of 2015    #
#--------------------------#


Dpar <- par(no.readonly=TRUE)

install.packages("car")
install.packages("nortest")
library(car)
library(nortest)

# Hypothesis Testing - Simple t-tests #
file.dir <- "C:/Users/William/Desktop/NCSU MSA/Fall 2015/R Programming/Data/"
input.file <- "session_4_data_TNFA_regression.csv"

SB.Data <- read.table(file=paste(file.dir,input.file,sep=""),
                         header=TRUE,
                         sep="\t",
                         na.strings="?")

dim(SB.Data)
SB.Data
rownames(SB.Data) 
colnames(SB.Data)

apply(SB.Data,2,unique) # columns
apply(SB.Data[,1:2],2,unique) # first two columns

mean(SB.Data$SuperBowlWins[SB.Data$Conference=="NFC"])
mean(SB.Data$SuperBowlWins[SB.Data$Conference=="AFC"])

var.test(SB.Data$SuperBowlWins ~ SB.Data$Conference)

t.test(SB.Data$SuperBowlWins ~ SB.Data$Conference,
       alternative="two.sided",
       paired=FALSE,
       var.equal=TRUE)

# ANOVA #
SB.Result <- aov(SB.Data$SuperBowlWins ~ SB.Data$Conference)
anova(SB.Result) 

SB.Result <- aov(SB.Data$SuperBowlWins ~ SB.Data$Division)
anova(SB.Result)


# Introduction to Linear regression #
regression.file.dir <- "C:/Users/William/Desktop/NCSU MSA/Fall 2015/R Programming/Data/"
regression.file.csv <- "session_4_data_TNFA_regression.csv"

Data.Gene <- read.csv(file=paste(regression.file.dir,regression.file.csv,sep=""),
                                  header=TRUE,row.names=1)

head(Data.Gene)

x <- Data.Gene$TNFA
y <- Data.Gene$NFKB

Gene.Model <- lm(y ~ x)
Gene.Model

summary(Gene.Model)

plot(Gene.Model)

# Extra Regression Options #
coefficients(Gene.Model) # Coefficients of Model
confint(Gene.Model, level=0.97) # CIs for Coefficients # 
fitted(Gene.Model) # Predicted Values #
residuals(Gene.Model) # Residuals #
anova(Gene.Model) # ANOVA Table #

# Regression Diagnostics #
# vif(Gene.Model) if we had more than 1 variable #
ncvTest(Gene.Model)
durbinWatsonTest(Gene.Model)
ad.test(Gene.Model$residuals)


