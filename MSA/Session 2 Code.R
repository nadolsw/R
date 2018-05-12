#--------------------------#
#   R Workshop Session 2   #
#                          #
#         Dr LaBarr        #
#     MSA Class of 2015    #
#--------------------------#

# Writing Data to External Files #
Data <-data.frame(matrix(data=rnorm(n=500,mean=100,sd=15),nrow=50,ncol=10)) 

colnames(Data) <- paste("Var",1:ncol(Data),sep="-") # Adds Names to Columns #
rownames(Data) <- paste("Sample",1:nrow(Data),sep="-") # Adds Names to Rows #
Data[1:10,] # Just Looking at the Newly Named Data Set #

file.dir <- "C:/Users/William/Documents/R"
output.file <- "session-1-example.txt"

write.table(Data, file=paste(file.dir,output.file,sep=""),
            sep="\t", row.names=TRUE, col.names=TRUE) # Tried to Include Column Names, but Were Incorrect #

write.table(Data, file=paste(file.dir,output.file,sep=""),
            sep="\t", row.names=TRUE, col.names=FALSE) # Left off Column Names #

write.table(Data, file=paste(file.dir,output.file,sep=""),
            sep="\t", row.names=FALSE, col.names=TRUE) # Left off Row Names, but Put Column Names #

write.table(Data, file=paste(file.dir,output.file,sep=""),
            sep="\t", row.names=TRUE, col.names=NA) # Handles Column Names Correctly #

output.file.csv <- sub(pattern=".txt",replacement=".csv",x=output.file) # Replaces .txt with .csv in File Name Through Pattern Search! #
output.file.csv

write.table(Data, file=paste(file.dir,output.file.csv,sep=""),
            sep=",", row.names=TRUE, col.names=NA)

write.csv(Data, file=paste(file.dir,output.file.csv,sep=""),
          row.names=TRUE)

write.csv(Data, file=paste(file.dir,output.file.csv,sep=""))

# Reading From External Files #
#file.dir <- "C:/Users/Aric/Documents/Analytics/R Workshop/"
input.file.txt <- "session-1-example.txt"

dir(file.dir)  # Lists Contents of Path set in 'file.dir' #

Data.IN <- read.table(file=paste(file.dir,input.file.txt,sep=""),
                      sep="\t")
Data.IN[1:10,1:5] # What is Wrong with the Row & Column Names??? #

Data.IN <- read.table(file=paste(file.dir,input.file.txt,sep=""),
                      sep="\t", header=TRUE)
Data.IN[1:10,1:5] # Row Names Still NOT Fixed!!! #

Data.IN <- read.table(file=paste(file.dir,input.file.txt,sep=""),
                      sep="\t",header=TRUE,row.names=1)
Data.IN[1:10,1:5] # Row and Column Names are FIXED #

input.file.csv <- sub(pattern=".txt",replacement=".csv",x=input.file.txt)
input.file.csv

Data.IN <- read.csv(file=paste(file.dir,input.file.csv,sep=""),
                    header=TRUE,row.names=1)
Data.IN[1:10,1:5]

# Graphical Procedures #
file.dir <- "C:/Users/William/Desktop/NCSU MSA/Fall 2015/R Programming/Data/"
input.file.csv <- "session-2-data.csv"

Data1 <- read.csv(file=paste(file.dir,input.file.csv,sep=""), header=TRUE,row.names=1)

attach(Data1)
names(Data1)

help(par) # par refers to global options in R
Dpar <- par(no.readonly = TRUE) # Save the Default List of Settable Parameters #
Dpar$mfrow  # What is the Default Value of 'mfrow'? #

par(mfrow=c(2,2)) # Multiple Plots in One Output View (2x2 Layout) #
plot(x=TreatmentGroup,y=CYP3A4, main="CYP3A4")
plot(x=TreatmentGroup,y=IL4, main="IL4")
plot(x=TreatmentGroup,y=NFKB, main="NFKB")
plot(x=TreatmentGroup,y=TNFA, main="TNFA")
par(Dpar) # Resets the Graphical Parameters to Default #
# NOTE: My Axes are of Different Scales #

par(mfrow=c(2,2))
plot(x=TreatmentGroup,y=CYP3A4, main="CYP3A4", ylim=c(0,250))
plot(x=TreatmentGroup,y=IL4, main="IL4", ylim=c(0,250))
plot(x=TreatmentGroup,y=NFKB, main="NFKB", ylim=c(0,250))
plot(x=TreatmentGroup,y=TNFA, main="TNFA", ylim=c(0,250))
par(Dpar) # Resets the Graphical Parameters to Default #
# NOTE: Now Axes are on Same Scale #

par(mfrow=c(1,4))
plot(x=TreatmentGroup,y=CYP3A4, main="CYP3A4", ylim=c(0,250))
plot(x=TreatmentGroup,y=IL4, main="IL4", ylim=c(0,250))
plot(x=TreatmentGroup,y=NFKB, main="NFKB", ylim=c(0,250))
plot(x=TreatmentGroup,y=TNFA, main="TNFA", ylim=c(0,250))
par(Dpar) # Resets the Graphical Parameters to Default #

# More Graphical Fun! #
library(datasets)
attach(mtcars)
names(mtcars)

par(mfrow=c(2,2), bg="lightblue") # Change Background Color to Light Blue #
plot(qsec ~ hp, data=mtcars, pch="*") # Variable Symbol now H #
lines(mtcars$hp,predict(lm(mtcars$qsec ~ mtcars$hp)),lty="solid",col="orange",lwd=2) # Adds a SOlid Line for Regression #

plot(qsec ~ wt, data=mtcars, pch="*") 
lines(mtcars$wt,predict(lm(mtcars$qsec ~ mtcars$wt)),lty="solid",col="orange",lwd=4)
plot(qsec ~ disp, data=mtcars, pch="*")
lines(mtcars$disp,predict(lm(mtcars$qsec ~ mtcars$disp)),lty="solid",col="orange",lwd=4)
plot(qsec ~ drat, data=mtcars, pch="*")
lines(mtcars$drat,predict(lm(mtcars$qsec ~ mtcars$drat)),lty="solid",col="orange",lwd=4)
par(Dpar) # Resets the Graphical Parameters to Default #

# Introduction to Regression Graphics #
Mean.Error <- 30  # Mean for Normally Distributed Error Term #
SD.Error <- 20    # SD for Normally Distributed Error Term #

x <- rnorm(n=100,mean=20,sd=5)
plot(x) # Plot by Index #

y <- 4*x + 4 + rnorm(n=length(x),mean=Mean.Error,sd=SD.Error)
plot(x=x, y=y)

lines(x=x,y=predict(lm(y ~ x)),lty="solid",col="red",lwd=2) # Add a Solid Regression Line #

CI <- data.frame(predict(lm(y ~ x), interval="confidence"),x) # Create Confidence Intervals for Regression Line #
head(CI)

CI <- CI[order(CI$x,decreasing = FALSE),] # Sort by Values of x #
head(CI)

lines(CI$lwr ~ CI$x, lty="dashed",lwd=2,col="blue") # Draw Lower Limit CI #
lines(CI$upr ~ CI$x, lty="dashed",lwd=2,col="blue") # Draw Upper Limit CI #

PI <- data.frame(predict(lm(y ~ x), interval="prediction"),x) # Create Prediction Intervals for Regression Line #
head(PI)

PI <- PI[order(PI$x,decreasing = FALSE),] # Sort by Values of x #
head(PI)

lines(PI$lwr ~ PI$x, lty="dashed",lwd=2,col="orange") # Draw Lower Limit PI #
lines(PI$upr ~ PI$x, lty="dashed",lwd=2,col="orange") # Draw Lower Limit PI #

# IF Statements #
Good <- TRUE
Bad <- FALSE
debug <- 5

if(Good){print("Execute this code")}  # If True #
if(!Good){print("Execute this code")} # If NOT True #
if(Bad){print("Execute this code")}
if(Good & Bad){print("Execute this code")} # If True AND True #
if(Good | Bad){print("Execute this code")} # If True OR True #

if(debug > 1){print("Execute this code")}  # If True (greater than) #
if(!debug > 2){print("Execute this code")}  # If NOT True (greater than) #
if(debug == 0){print("Execute this code")}  # If True (equal to) #
if(debug != 0){print("Execute this code")}  # If True (NOT equal to) #

if(any(colnames(Data1)=="CYP3A4")){print("Execute this code")} # Uses any() Function to Scan Through Column Names #
if(any(colnames(Data1)=="Mystery Gene")){print("Execute this code")}

if(Good){
  print("Execute this code") # If True #
}else{
  print("Execute this OTHER code") # If NOT True #
}

if(Bad){
  print("Execute this code") # If True #
}else{
  print("Execute this OTHER code") # If NOT True #
}

### switch statement
### NOTE: this is like a shortcut for multiple if statements
Model <- "M1" # Example Selection for Multiple Models #
switch(EXPR=Model,
       M1 = print("Run CODE for Model 1"),
       M2 = print("Run CODE for Model 2"),
       M3 = print("Run CODE for Model 3"),
)

# For Loops #
for(i in 1:10){
  print(i) # Print the Value of i at Each Iteration #
}

for(i in 1:10){
  i 
}

for(gene in 1:10){
  print(mean(Data1[,gene])) # Print the Mean of Each Column for First 10 Columns #
}

for(gene in 1:10){
  print(paste("Mean expression of", colnames(Data1)[gene], "=", mean(Data1[,gene]))) # Print the Mean of Each Column for First 10 Columns #
}

Mean.Results <- data.frame(matrix(0,nrow=10,ncol=2,byrow=TRUE))
rownames(Mean.Results) <- colnames(Data1)[1:10]
colnames(Mean.Results) <- c("Mean","Variance")

for(gene in 1:10){
  Mean.Results$Mean[gene] <- mean(Data1[,gene])
  Mean.Results$Variance[gene] <- var(Data1[,gene])
}
Mean.Results

for(row in 1:3){
  for(column in 1:5){
    print(paste("row =",row,"; col =",column))
    print(Data1[row,column])
  }
}
Data1[1:3,1:5]

par(mfrow=c(2,3)) # Creating for Loop to Plot Multiple Graphs #
for(gene in 1:6){
  plot(x=Data1$TreatmentGroup,y=Data1[,gene],
       main=colnames(Data1)[gene], ylim=c(0,250),
       cex.axis=.8)
}
par(Dpar)

# While Loops #
counter <- 1
while(counter <= 10){ # Stop Processing After Counter Goes Above 10 #
  print(counter) # Print the Value of Each i at Interation #
  counter <- counter + 1 # Add to Counter at Every Step #
}

gene <- 1
while(gene <= 10){
  print(paste("Mean expression of", colnames(Data1)[gene], "=", mean(Data1[,gene]))) # print the mean at each iteration
  gene <- gene + 1
}

gene <- 1
while(gene < 1000){
  if(gene >= 25){break}   # Break Out of Loop if Gene Gets Above 25 #
  print(paste("Mean expression of", colnames(Data1)[gene], "=", mean(Data1[,gene]))) # print the mean at each iteration
  gene <- gene + 1 # Add to the Counter at Every Step #
}

gene <- 1
while(gene <= ncol(Data1)-1){
  print(paste("Mean expression of", colnames(Data1)[gene], "=", mean(Data1[,gene]))) # print the mean at each iteration
  gene <- gene + 1
}

# Apply Function #
head(Data1)

apply(Data1,2,mean) # Applies the Mean Function Across All Columns of Data1 #
apply(Data1[,1:25],2,mean) # Applies the Mean Function Across Only the Columns 1:25 #
lapply(Data1,is.numeric) # lapply() Does Functions Across Lists #

gene.columns <- which(lapply(Data1,is.numeric)=="TRUE")
apply(Data1[,gene.columns],2,mean)

