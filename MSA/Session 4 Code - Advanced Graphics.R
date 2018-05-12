#--------------------------#
#   R Workshop Session 4   #
#                          #
#         Dr LaBarr        #
#     MSA Class of 2015    #
#--------------------------#


Dpar <- par(no.readonly=TRUE)

# 3D Plotting #
install.packages("scatterplot3d")
install.packages("rgl")
library(scatterplot3d)
library(rgl)

temp <- seq(-pi, 0, length = 50)
x <- c(rep(1, 50) %*% t(cos(temp))) 
y <- c(cos(temp) %*% t(sin(temp))) 
z <- c(sin(temp) %*% t(sin(temp))) 
scatterplot3d(x, y, z, 
              highlight.3d = TRUE, angle = 120, 
              col.axis = "blue", col.grid = "lightblue", 
              main = "3D Hemisphere", pch = 20)


file.dir <- "C:/Users/William/Desktop/NCSU MSA/Fall 2015/R Programming/Data/"
input.file <- "session_4_data_graphics.csv"


Data2 <- read.csv(file=paste(file.dir,input.file,sep=""),
                       header=TRUE,row.names=1)  

# examine data
dim(Data2)
head(Data2)

# scatterplot3d with class data example
x <- Data2$Exposure.A 
y <- Data2$Exposure.B 
z <- Data2$Exposure.C 
scatterplot3d(x, y, z, 
              highlight.3d = FALSE, angle = 60,
              col.axis = "blue", grid = TRUE, box=FALSE,
              main = "Exposure Data", pch = 19,
              xlab=colnames(Data2)[1],ylab=colnames(Data2)[2],zlab=colnames(Data2)[3]) 


# Assigning Colors Based on Variables #
Data2.sorted <- Data2[order(Data2$Outcome,decreasing=TRUE),]

color.vec <- c(rep("red",sum(Data2.sorted$Outcome==1)),
               rep("blue",sum(Data2.sorted$Outcome==0)))

rbind(color.vec,Data2.sorted$Outcome) 

x <- Data2.sorted$Exposure.A
y <- Data2.sorted$Exposure.B
z <- Data2.sorted$Exposure.C
scatterplot3d(x, y, z,
              color=color.vec,
              highlight.3d = FALSE, angle = 60,
              col.axis = "blue", grid = TRUE, box=FALSE,
              main = "Exposure Data", pch = 16,
              xlab=colnames(Data2)[1],ylab=colnames(Data2)[2],zlab=colnames(Data2)[3])

legend("topright",legend=paste(unique(Data2.sorted$Outcome)," = ",c("Disease","Healthy"),sep=""),
       col=unique(color.vec),cex=.7,
       pch=16, pt.cex=1.5)


# Interacting with 3D Graphics #
x <- Data2.sorted$Exposure.A
y <- Data2.sorted$Exposure.B
z <- Data2.sorted$Exposure.C
open3d()
plot3d(x, y, z,
       size = 2,
       type="s", # pretty spheres
       col=color.vec,
       xlab=colnames(Data2)[1],ylab=colnames(Data2)[2],zlab=colnames(Data2)[3])


# Animations with Plots #
x <- Data2.sorted$Exposure.A
y <- Data2.sorted$Exposure.B
z <- Data2.sorted$Exposure.C
open3d()
plot3d(x, y, z,
       size = 2,
       type="s", # pretty spheres
       col=color.vec,
       xlab=colnames(Data2)[1],ylab=colnames(Data2)[2],zlab=colnames(Data2)[3])

play3d(spin3d(axis=c(1,0,0), rpm=10), duration=50)

play3d(spin3d(axis=c(-1,0,0), rpm=5), duration=10)

play3d(spin3d(axis=c(1,1,1), rpm=5), duration=10)

play3d(spin3d(axis=c(1,0,1), rpm=200), duration=50)

