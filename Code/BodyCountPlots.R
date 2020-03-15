# BU510.650 Data Analytics Week 1
# Plotting data using R
# Example: Film death counts
# Set your working directory. In my case, it is "C:/R-Work".
setwd("C:/Users/achal/Desktop/Carey/Data Analytics/Data")
# Read the data from the file "filmdeathcounts.csv" into a data frame called "BodyCountData". 
BodyCountData = read.csv("filmdeathcounts.csv")
# Let's change the names of the columns in the data frame to shorter and more descriptive names. We use "colnames" function and assign a vector of new names. The function c() indicates the list of names is a vector.
colnames(BodyCountData) <-c("Film","Year","Bodies","MPAA","Genre","Director","Minutes","IMDB")
# Let's add another column, titled "FilmCount", to our data frame and fill it with "1" for each film.
BodyCountData["FilmCount"] <- 1
# Let's add one more column, "BodyPerMin", which will show the number of bodies per minute for each film.
BodyCountData["BodyPerMin"] <- BodyCountData[,3] / BodyCountData[,7]
# Next, we create a table that will show the total number of bodies for each year. We use the "tapply" function. In the following line, tapply will check what values appear in the "Year" column, and for each year, it will "sum" together the numbers in the "Bodies" column of the films made in that year.    
t1=tapply(BodyCountData$Bodies,BodyCountData$Year,FUN="sum")
# We can now create a barplot that shows the total number of bodies in each year. 
barplot(t1,xlab="year",ylab="Total # Bodies")
# Load the library "lattice", which gives us further capabilities for plotting.
library(lattice)
# "barchart" comes with the library "lattice", for example.
barchart(t1)
# Create a new table t2, which will have our data in decreasing order of the third column, which is the number of bodies in each movie
t2 <- BodyCountData[order(BodyCountData[,3],decreasing=TRUE),]
# Pick only the first 10 rows of t2 - these are the top-10 movies in terms of body count 
t2 <- t2[1:10,]
# Pick only the first columns of t2
t2<-t2[c(1,3)]
# Create a barchart that shows the number of bodies for each movie in t2
barchart(Film ~ Bodies, data=t2)
# Use the "table" function to find out how many films fell into each MPAA category
t3 <- table(BodyCountData$MPAA)
t3
# Create an xy-plot of all movies, plotting MPAA rating on the x-axis and the body count on the y-axis
xyplot(Bodies~MPAA,data=BodyCountData,col="black")
# We could also create a boxplot to capture the same information
boxplot(Bodies~MPAA,data=BodyCountData)
# "bwplot" is the same as boxplot - it comes with the "lattice" library, which we loaded earlier
bwplot(Bodies~MPAA,data=BodyCountData)
# Create an xy-plot of all movies, plotting IMDB ratings on the x-axis and the body count on the y-axis
xyplot(Bodies~IMDB,data=BodyCountData)
# Create an xy-plot of all movies, plotting IMDB ratings on the x-axis and the "bodies per minute" on the y-axis 
xyplot(BodyPerMin~IMDB,data=BodyCountData)
