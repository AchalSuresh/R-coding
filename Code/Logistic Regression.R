#1
r1= exp(-6+(0.05*40)+3.5)

p= r1/(1+r1)

log(1)
###############################################################
#2a
#Load library ISLR to load dataset College
library(ISLR)
View(College)
#Remove all values having NA
College2=na.omit(College)

#Loading Predictors and Response into Vectors x and y resp.
x=model.matrix(Apps~.,data=College2)[,-1] 

y=College2$Apps

#Loading library glmnet
library(glmnet)

#2b) 5-fold cross-validation on the training data, to determine the best lambda value
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1, nfolds=5)

plot(cv.out)
#2b Best value of lambda
bestlam=cv.out$lambda.min
bestlam

#Running lasso with the best lambda value
lasso.final=glmnet(x, y, alpha=1, lambda=bestlam)
coef(lasso.final)

#2c Predicitng number of applications for all colleges
lasso.pred = predict(lasso.final, s=bestlam, newx=x)
write.table(lasso.pred, "C:/Users/achal/Desktop/Carey/Data Analytics/mydata.txt")

#Mean absolute Error
mean((lasso.pred - y)^2)

##################################################################################
#3a

Auto <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Auto.csv" ,na.strings = "?")

Auto2=na.omit(Auto)
median(Auto2$mpg)#22.75

#create a column mpg_above to denote 
Auto2$mpg_above<-1
Auto2$mpg_above[Auto2$mpg<22.75]<-0

#3b
#randomly splitting the data into
set.seed(1)

#Load rownumber to train
train=sample(1:nrow(Auto2),nrow(Auto2)/2)

test=(-train)

Auto2.train=Auto2[train,]


Auto2.test=Auto2[test,]

Auto2.test.output = Auto2$mpg_above[test]

#3c Logisitic Regression
logreg.fit1 <- glm(mpg_above~weight+year+origin,data=Auto2.train,family=binomial)

summary(logreg.fit1)

#3d
logreg.fit2 <- glm(mpg_above~weight+year,data=Auto2.train,family=binomial)
summary(logreg.fit2)


#3e
#Predicting the value for test data
logreg.fit2.prob=predict(logreg.fit2,Auto2.test,type="response")
head(logreg.fit2.prob)

#Creating a vector to store values which consists of 0
logreg.fit2.predict=rep(0,nrow(Auto2.test))
logreg.fit2.predict

#Updating rows which have probability greater than 0.5
logreg.fit2.predict[logreg.fit2.prob>0.5] = 1

#3f compute the percentage of time the prediction was correct
mean(logreg.fit2.predict==Auto2.test.output)

