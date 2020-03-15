## TASK 1 ##

# set random seed so that our results are reproducible
RNGkind(sample.kind = "Rounding")
set.seed(2)

# generate a matrix, called x, with 100 numbers drawn from standard normal distribution,
# placed in two columns
x=matrix(rnorm(50*2),ncol=2)

# add 3 to the first column in the first 25 rows
x[1:25,1]=x[1:25,1]+3

# subtract 4 from the second column in the first 25 rows 
x[1:25,2]=x[1:25,2]-4

#Creating clsuter with centroid (3,-4) and another with centroid (0,0) to test the algorithm


## TASK 2 ##

# run K-means using x as our data, to create 2 clusters, and with 20 different initializations
# store the results in a variable called km.out
km.out=kmeans(x,2,nstart=20)

# display the results
km.out

# display the cluster each observation in x was assigned to
km.out$cluster


## TASK 3 ##

# consider first the following example, which plots the points (1,1), (2,2), ..., (10, 10)
# col specifies a color for each point, pch specifies a shape for each point
plot(1:10,1:10,col=1:10,pch=1:10)

# plot, when applied with col = km.out$cluster provides a special type of plot
# notice that each point will be given one of two colors, depending on the cluster the point belongs to
plot(x,col=km.out$cluster)

# plot, when applied with pch = km.out$cluster, gives each point 
# one of two shapes, depending on the cluster the point belongs to
# lwd=2 is the thickness of bold shapes, the higher the number, the thicker
plot(x,col=km.out$cluster,pch=km.out$cluster,lwd=2)

# to display the within-cluster variation for each cluster
km.out$withinss

# to display the total within-cluster variation
km.out$tot.withinss

## TASK 4 ##

# set random seed so that our results are reproducible
RNGkind(sample.kind = "Rounding")
set.seed(4)

# run K-means using x as our data, to create 3 clusters, and with 20 different initializations
# store the results in a variable called km.out2
km.out2=kmeans(x,3,nstart=50)

# display the results
km.out2

# plot the results to see clusters
plot(x,col=km.out2$cluster,pch=km.out2$cluster)

# to display the within-cluster variation for each cluster
km.out2$withinss

# to display the total within-cluster variation
km.out2$tot.withinss
