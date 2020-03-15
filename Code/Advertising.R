# read the data into an R data.frame called my.ad
my.ad<-read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Data/Advertising.csv")

## Before we do regression, here are few commands to review the data a bit

# let us see the first six rows of the data
head(my.ad)

# let us see the last six rows of the data
tail(my.ad)

# here is another way to see a preview of the data
View(my.ad)

# the following commands will show us the names of variables, etc. in the data
names(my.ad)
summary(my.ad)

# what if we wanted to remove some of the information from the data

# the following will remove rows 2 through 5 and will assign the result to a new data.frame called my.ad2
my.ad2 <- my.ad[-(2:5),]

# the following will remove rows 2, 5, and 10 and will assign the result to a new data.frame called my.ad3
my.ad3 <- my.ad[-c(2,5,10),]


# the following will remove column 1 nd will assign the result to a new data.frame called my.ad4
my.ad4 <- my.ad[,-1] 

# check the first six rows of my.ad4
head(my.ad4)

# the command par(mfrow=c(x,y)) divides plot space into x rows and y columns. Used to see multiple graphs together
par(mfrow=c(1,1)) 

# plot Sales versus TV budget
plot(my.ad$TV,my.ad$sales,xlab = "TV",ylab = "Sales")

par(mfrow=c(1,2)) 
plot(my.ad$TV,my.ad$sales,xlab = "TV",ylab = "Sales")
plot(my.ad$TV,my.ad$sales,xlab = "TV",ylab = "Sales")
plot(my.ad$TV,my.ad$sales,xlab = "TV",ylab = "Sales")
plot(my.ad$TV,my.ad$sales,xlab = "TV",ylab = "Sales")
plot(my.ad$radio,my.ad$sales,xlab = "TV",ylab = "Sales")




# run a regression of Sales wrt TV budget. lm-linear model, sales-y, tv-x
my.lm=lm(sales~TV,data=my.ad)
summary(my.lm)

##In-class exercise##


# plot Sales versus Newspaper budget
par(mfrow=c(1,1)) 
plot(my.ad$newspaper,my.ad$sales,xlab = "TV",ylab = "Sales")
# run a regression of Sales wrt Newspaper budget
my.lm1=lm(sales~newspaper,data=my.ad)
summary(my.lm1)

# Next, we run five regressions reported on Slide 28 of Session 2 notes

# Regression with TV only 
my.lm.1=lm(sales~TV, data=my.ad)
summary(my.lm.1)

# Regression with Radio only 
my.lm.2=lm(sales~radio, data=my.ad)
summary(my.lm.2)

# Regression with Newspaper only 
my.lm.3=lm(sales~newspaper, data=my.ad)
summary(my.lm.3)

# Regression with TV and Radio 
my.lm.4=lm(sales~TV+radio, data=my.ad)
summary(my.lm.4)

# Regression with TV, Radio, and Newspaper 
my.lm.5=lm(sales~TV+radio+newspaper, data=my.ad)
summary(my.lm.5)

# Suppose that we decided to use the model with TV and Radio for prediction

#Use a normal probability plot to check whether residuals follow a normal distribution
qqnorm(my.lm.4$res)
qqline(my.lm.4$res)

#Plot residuals versus fitted values to check whether all the variances of the residuals are equal
plot(my.lm.4$res ~ my.lm.4$fitted)

#Plot residuals in the order of the data to check if there is any pattern associated with the order of data collection (assuming that the order of the data is the order of data collection)
plot(my.lm.4$res)
plot(my.lm.4$res ~ my.ad$TV)
plot(my.lm.4$res ~ my.ad$radio)

# Let us now predict the sales in a city where TV budget is $100K and Radio budget is $50K

# The following yields a prediction along with a 95% confidence interval around it
predict(lm(sales~TV+radio, data=my.ad), data.frame(TV=100, radio=50), interval="confidence")

# The following yields a prediction along with a 95% prediction interval around it
predict(lm(sales~TV+radio, data=my.ad), data.frame(TV=100, radio=50), interval="prediction")



my.lm.6=lm(sales~TV+radio+TV*radio, data=my.ad)
summary(my.lm.6)


qqnorm(my.lm.6$res)
qqline(my.lm.6$res)


plot(my.lm.6$res ~ my.lm.6$fitted)

#Plot residuals in the order of the data to check if there is any pattern associated with the order of data collection (assuming that the order of the data is the order of data collection)
plot(my.lm.6$res)
plot(my.lm.6$res ~ my.ad$TV)
plot(my.lm.6$res ~ my.ad$radio)

ci <- predict(lm(sales~TV+radio+TV*radio, data=my.ad), interval="confidence")

predict(lm(sales~TV+radio+TV*radio, data=my.ad), interval="prediction")

