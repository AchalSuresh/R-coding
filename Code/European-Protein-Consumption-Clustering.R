## TASK 1 ##

# After setting your working directory, read the data from protein.csv
# into a data.frame called food
food <- read.csv( "C:/Users/achal/Desktop/Carey/Data Analytics/Data/protein.csv" )

# check the data 
View(food)

## TASK 2 ## 

# set random seed so that our results are reproducible
RNGkind(sample.kind = "Rounding")
set.seed(1)

# store the white and red meat consumptions in a separate data.frame called food2
food2<-food[,c(2,3)]

# cluster the contries based on white and red meat consumption
# form three clusters and use 10 initializations
# store the results in a variable called Meats
Meats <- kmeans(food2,3,nstart=10)
Meats

## TASK 3 ## 

# display the results
Meats

# plot all countries, with Red Meat on x-axis and White Meat on y-axis
plot(food$RedMeat, food$WhiteMeat, xlab="Red Meat", ylab="White Meat")

# use different colors for clusters
plot(food$RedMeat, food$WhiteMeat,col=Meats$cluster, xlab="Red Meat", ylab="White Meat")

# use different shapes for clusters
plot(food$RedMeat, food$WhiteMeat, pch=Meats$cluster, xlab="Red Meat", ylab="White Meat")

# use different colors and shapes for clusters
plot(food$RedMeat, food$WhiteMeat,col=Meats$cluster, pch=Meats$cluster, xlab="Red Meat", ylab="White Meat",lwd=3)

# display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=Meats$cluster)

## TASK 4 ##
# cluster the contries based on all types of protein
# form seven clusters and use 10 initializations
# store the results in a variable called Protein
Protein <- kmeans(food[,-1],7,nstart=10)

# display the results
Protein

# plot all countries, with Red Meat on x-axis and White Meat on y-axis,
# using different colors and shapes for different clusters
plot(food$RedMeat, food$WhiteMeat, col=Protein$cluster,pch=Protein$cluster,xlab="Red Meat", ylab="White Meat") 

#display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=Protein$cluster)


## TASK 5 ##

# run hierarchical clustering, using complete as your measure of similarity
# dist() is an R function that computes Euclidean distances between observations
# store the results in a variable called hc.food.complete
hc.food.complete=hclust(dist(food[,-1]),method="complete")

# plot the dendrogram
plot(hc.food.complete)

# cut the tree to obtain three clusters
hc.out.1=cutree(hc.food.complete,3) 

## TASK 6 ##

# plot all countries, with Red Meat on x-axis and White Meat on y-axis,
# use different colors and shapes for clusters
plot(food$RedMeat,food$WhiteMeat,col=hc.out.1,pch=hc.out.1)

#display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=hc.out.1)

## TASK 7 ##

# run hierarchical clustering, using average as your measure of similarity
# dist() is an R function that computes Euclidean distances between observations
# store the results in a variable called hc.food.average
hc.food.average=hclust(dist(food[,-1]),method="average")

# plot the dendrogram
plot(hc.food.average)

# cut the tree to obtain three clusters
hc.out.2=cutree(hc.food.average,3) 

# plot all countries, with Red Meat on x-axis and White Meat on y-axis,
# use different colors and shapes for clusters
plot(food$RedMeat,food$WhiteMeat,col=hc.out.2,pch=hc.out.2)

#display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=hc.out.2)



food2 = food2<-food[,c(6,10)]
hc.food.average=hclust(dist(food2),method="complete")

plot(hc.food.average)


hc.out.3=cutree(hc.food.average,3) 

plot(food$Fish,food2$Fr.Veg,col=hc.out.3,pch=hc.out.3)
text(x=food$Fish,y=food$Fr.Veg-0.25,labels=food$Country,col=hc.out.3)




food3 = food[,c(2,7)]
hc.food.complete=hclust(dist(food3),method="complete")

plot(hc.food.complete)


hc.out.4=cutree(hc.food.complete,3) 

plot(food3$RedMeat,food3$Cereals,col=hc.out.4,pch=hc.out.4)
text(x=food3$RedMeat,y=food3$Cereals-0.25,labels=food$Country,col=hc.out.4)
