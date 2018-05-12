#--------------------------#
#   R Workshop Session 3   #
#                          #
#         Dr LaBarr        #
#     MSA Class of 2015    #
#--------------------------#

Dpar <- par(no.readonly=TRUE)

file.dir <- "C:/Users/William/Desktop/NCSU MSA/Fall 2015/R Programming/Data/"
input.file.csv <- "session-2-data.csv"

Data1 <- read.csv(file=paste(file.dir,input.file.csv,sep=""), header=TRUE,row.names=1)

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
       M3 = print("Run CODE for Model 3")
)

# For Loops #
for(i in 1:10){
  print(i) # Print the Value of i at Each Iteration #
}

for(i in 1:10){
  i 
}

print (ncol(Data1))

for(gene in 1:ncol(Data1)){
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

for(row in 1:10){
  for(column in 1:5){
    print(paste("row =",row,"; col =",column))
    print(Data1[row,column])
  }
}
Data1[1:10,1:5]

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


9# Structures of Functions #
a <- round(rnorm(n=100,mean=100,sd=15),0) # Creating Some Data #
b <- round(rnorm(n=100,mean=70,sd=15),0)

plot(a,b) # The Function is Called with Two Parameters #
plot(x=a,y=b) # Here We Have Two Named Parameters #
plot(x=a,y=b,main="Best. Plot. Ever.") # The Function is Called With an Optional Parameter #
plot(main="Really. Best. Plot. Ever.", x=a, y=b) # Order Doesn't Matter if Using Named Parameters #

# Writing Your Own Function - Distance Function #
my.distance <- function(x1,x2,metric,direction="onetwo"){
  if(metric=="difference"){
    if(direction=="onetwo"){
      d <- x1-x2
    }
    if(direction=="twoone"){
      d <- x2-x1
    }
  }
  if(metric=="squared"){
    d <- ((x1-x2)^2)
  }
  d # return value of d
}

# Calling the New Distance Function #
my.distance(x1=a,x2=b,metric="difference")  # What Will Direction Be Since it Wasn't Specified #
my.distance(x1=a,x2=b,metric="difference",direction="twoone") # Specify Direction Option #
my.distance(x1=a,x2=b,metric="squared")
d<-my.distance(x1=a,x2=b,metric="squared")

# Global Variables and Functions #
my.print.example <- function(x1){
  u <- mean(x1)
  print(u)
  print(d)
}

my.print.example(b)
# 'u' is a "Local Variable" Inside the Function, but 'd' is NOT Visible, Because it was a Local Variable Inside Another Function #

my.global.example <- function(x1){
  u <- mean(x1)
  print(u)
  global.u <<- u # "<<-" is a Global Assignment Operator That Saves to Global Workspace #
}

my.global.example(b)
