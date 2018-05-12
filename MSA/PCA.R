#setwd('/Users/Shaina/Desktop/Data/ukfood')
#food=read.csv("ukfood.csv",row.names=1)

food=read.csv("http://www4.ncsu.edu/~slrace/LinearAlgebra2016/Code/ukfood.csv", header=TRUE,row.names=1)


food=as.data.frame(t(food))
head(food)
food=data.frame(food)

# princomp function can only be used with more observations than variables. Use prcomp 
# for opposite situation

pca=prcomp(food)

# first plot just looks at magnitudes of eigenvalues
plot(pca)

# next plot views our four datapoints (locations) projected onto the 2-dimensional subspace
# that captures as much information (i.e. variance) as possible
plot(pca$x)
text(pca$x[,1], pca$x[,2],row.names(food))

# now we can also view our original variable axes projected down onto that same space!
# a visual you can relate this to: Take a plane (piece of poster board) running at an angle
# through the origin in 3 space. Think of the unit axis vectors being projected orthogonally
# onto this poster board... The closer the plane comes to that axis, the longer that projection will be.
# Long projections means that those principal components run close to the original variable -
# they are highly correlated. Shorter projections indicate less correlation with PCs. Less correlation
# with major PCs may simply mean there isn't much variance along those variables - the variables with the 
# most variance are likely to dominate the first components.
biplot(pca)

