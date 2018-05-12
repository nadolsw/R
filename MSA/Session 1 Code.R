#--------------------------#
#   R Workshop Session 1   #
#                          #
#         Dr LaBarr        #
#     MSA Class of 2015    #
#--------------------------#

# Unary Operator Example #
log(3)

# Binary Operator Example #
2 + 2

x <- 2 + 2 # Saving our Value to a Variable x #
x # Printing the Value of x #

rm(x) # Removing the Value x from Workspace #

# Create Some Vectors and Manipulate #
x <- c(1,2,3,4,5) # Concatenates Values into a Vector #
x

y <- 1:10 # Creates Sequence of 1 to 10 #
y

x + y

x*y

v <- 2*x + y + 1
v

# Logical Vectors #
temp <- v > 13
temp

# Character Strings #
a <- "Hello"
b <- "My Friends"

c(a,b)
c<-paste(a,b)
c

# Functions #
x <- -5:5
y <- abs(x)

plot(x,y)

M <- matrix(data=rnorm(n=50,mean=100,sd=15),nrow=10,ncol=5) # Create a Matrix having 10 Rows and 5 Columns
M

Data <- data.frame(M) # Convert the Matrix to a Data Frame #
Data # Look at the Column Names! #

Data[1,1]	# Examine the Value in Position [1,1] #

Data[,1]	# Examine the First Column Using the '[]' Extractor Operator #

Data$X1	# Examine Column 'X1' (default data.frame name for the first column) #

Data[1:5,1]	# Examine the First Five Rows of Column 1 #

Data$X1[1:5]	# Examine the First Five Rows of Column 'X1' #

Data[1:5,]	# Examine the First Five Rows of all Columns #

head(Data,5)	# Examine the First Five Rows of all Columns #

V <- Data[,1]	# Create a Vector from the First Column of Your Data Frame #

# Using the library() Function #
library(datasets)  # load the 'datasets' package (contains example datasets)
attach(mtcars)	# attach this dataset to the R search path (for variable names, etc.)

head(mtcars)	# display the first few rows of the 'mtcars' dataset

stem(mtcars$mpg)	# stem-and-leaf plot of miles-per-gallon for all cars

hist(mtcars$mpg)	# histogram of miles-per-gallon for all cars

boxplot(mpg ~ cyl, data=mtcars, xlab="Cylinders", ylab="Miles per gallon") # boxplot of MPG by number of cylinders

boxplot(mpg ~ gear, data=mtcars, xlab="Number of Gears", ylab="Miles per gallon") # boxplot of MPG by number of gears

plot(mpg ~ cyl, data=mtcars) # (awkward scatterplot) plot of MPG by number of cylinders

plot(mpg ~ wt, data=mtcars) # plot of MPG by weight

plot(mpg ~ hp, data=mtcars) # plot of MPG by horsepower

# More Library Loading Practice #
library(foreign)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)

# Writing Data to External Files #
Data <-data.frame(matrix(data=rnorm(n=500,mean=100,sd=15),nrow=50,ncol=10)) 

colnames(Data) <- paste("Var",1:ncol(Data),sep="-") # Adds Names to Columns #
rownames(Data) <- paste("Sample",1:nrow(Data),sep="-") # Adds Names to Rows #
Data[1:10,] # Just Looking at the Newly Named Data Set #

file.dir <- "C:/Users/Aric/Documents/Analytics/R Workshop/"
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
file.dir <- "C:/Users/Aric/Documents/Analytics/R Workshop/"
input.file.txt <- "session-1-example.txt"

dir(file.dir)	# Lists Contents of Path set in 'file.dir' #

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

# Reading in SAS Data Sets #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4"
timehome <- "C:/Users/Aric/Documents/Analytics/Time Series/MSA 2014/Data"

AR2 <- read.ssd(file.path(timehome), "ar2", sascmd=file.path(sashome, "sas.exe"))
USAirlines <- read.ssd(file.path(timehome), "usairlines", sascmd=file.path(sashome, "sas.exe"))
