## TASK 1 ##
RNGkind(sample.kind = "Rounding")
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

## TASK 2 ##

# run hierarchical clustering, using complete as your measure of similarity
# dist() is an R function that computes Euclidean distances between observations
# store the results in a variable called hc.complete
hc.complete=hclust(dist(x),method="complete")

# plot the dendrogram
plot(hc.complete)


## TASK 3 ## 
# run hierarchical clustering again, but using different measures of similarity:
# average, single, and centroid
# store the results in variable called hc.average, hc.single, hc.centroid
hc.average=hclust(dist(x),method="average")
hc.single=hclust(dist(x),method="single")
hc.centroid=hclust(dist(x),method="centroid")


# divide the plotting space into four columns
par(mfrow=c(1,4))

# plot the dendrograms from different measures of similarity
plot(hc.complete)
plot(hc.average)
plot(hc.single)
plot(hc.centroid)

## TASK 4 ##

# cut the tree to obtain two clusters
# notice the output, which tells you the the cluster for each observation
cutree(hc.complete,2)

# cut the tree to obtain two clustrs, and assign the clustering results to a variabel called hc.out.2
hc.out.2=cutree(hc.complete,2) 

# re-set the plotting space
par(mfrow=c(1,1))

# plot the data, giving different colors depending on clusters
# this is similar to what we did with K-means
plot(x,col=hc.out.2)


## TASK 5 ##

# cut the tree to obtain three clusters
# notice the output, which tells you the the cluster for each observation
cutree(hc.complete,3)

# cut the tree to obtain three clusters, and assign the clustering results to a variabel called hc.out.3
hc.out.3=cutree(hc.complete,3) 

# plot the data, giving different colors depending on clusters
# this is similar to what we did with K-means
plot(x,col=hc.out.3)
