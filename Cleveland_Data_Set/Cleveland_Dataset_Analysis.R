#Hypothesis 1- Regression on num

# setting directory
setwd("C:/Users/posto/Desktop/Study/Spring semester/Data analytics/Project")
library(class)
library(glmnet)
heart.data <- read.csv("processed.cleveland.data",header=FALSE, sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data <- na.omit(heart.data)
heart.data

#chclass <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
#heart.data <- convert.magic(heart.data,chclass)

#convert heart disease to 0s and 1s
heart.data$num[heart.data$num > 0] <- 1

#converting to factors
heart.data$sex<-as.factor(heart.data$sex)
heart.data$cp<-as.factor(heart.data$cp)
heart.data$fbs<-as.factor(heart.data$fbs)
heart.data$restecg<-as.factor(heart.data$restecg)
heart.data$exang<-as.factor(heart.data$exang)
heart.data$slope<-as.factor(heart.data$slope)
heart.data$ca<-as.factor(heart.data$ca)
heart.data$thal<-as.factor(heart.data$thal)
heart.data$num<-as.factor(heart.data$num)


#creating results vector to run LAsso and identify lambda



#creating factors matrix
#factors=model.matrix(num~.,data=heart.data)[,-1]

set.seed(1)

#Splitting the data into training and testing
testing = sample(1:nrow(heart.data), 99)
training = -testing
testing_data = heart.data[testing,]
training_data = heart.data[training,]

# creating matrixes and vector of response variables for training data
factors_training = model.matrix(num~.,data=training_data)[,-1]
response_training = training_data$num


#running lasso on training data
lasso = cv.glmnet(factors_training, response_training, alpha = 1, family = "binomial")

# identifying best lambda
minlamda=lasso$lambda.min


# use best lambda for lasso and identifying coefficients
model1 = glmnet(factors_training, response_training, alpha = 1, lamda = minlamda, family = "binomial")
coef(model1, s=minlamda)

#predicting the reponse variable for testing data

#creating factor matrix of testing variables and vector of response variables for testing data
factors_testing = model.matrix(num~.,data=testing_data)[,-1]
response_testing = testing_data$num

#predicting and finding error
prediction = predict(model1, s=minlamda, newx = factors_testing, type = "response")

# creating vector for interpretation of probabilities
interpretations <- rep(0, 99)

#interpreting predicted probabilities
#60% probability allows to achieve better accuracy
interpretations[prediction>0.5]=1


mean(interpretations==response_testing)
table(interpretations, response_testing)


# splitting num into four variables: num1 - the least serious diagnosis, num4 - the most serious diagnosis
heart.data$num1 <-0
heart.data$num2 <-0
heart.data$num3 <-0
heart.data$num4 <-0
heart.data$num1[heart.data$num == 1] <-1
heart.data$num2[heart.data$num == 2] <-1
heart.data$num3[heart.data$num == 3] <-1
heart.data$num4[heart.data$num == 4] <-1


#converting to factors
heart.data$sex<-as.factor(heart.data$sex)
heart.data$cp<-as.factor(heart.data$cp)
heart.data$fbs<-as.factor(heart.data$fbs)
heart.data$restecg<-as.factor(heart.data$restecg)
heart.data$exang<-as.factor(heart.data$exang)
heart.data$slope<-as.factor(heart.data$slope)
heart.data$ca<-as.factor(heart.data$ca)
heart.data$thal<-as.factor(heart.data$thal)
heart.data$num<-as.factor(heart.data$num)



###########################################
#Running lasso and logistic regression using num1 as response variable - the least signifficant diagnosis###################
set.seed(1)
heart.data1 = heart.data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)]
heart.data1


#Splitting the data into training and testing, where testing - 30% of all raws
testing1 = sample(1:nrow(heart.data1), 99)
training1 = -testing1
testing_data1 = heart.data1[testing1,]
training_data1 = heart.data1[training1,]

# creating matrixes and vector of response variables for training data
factors_training1 = model.matrix(num1~.,data=training_data1)[,-1]
response_training1 = training_data1$num1

#running lasso on training data
lasso1 = cv.glmnet(factors_training1, response_training1, alpha = 1, family = "binomial")

# identifying best lambda
minlamda1=lasso1$lambda.min


# use best lambda for lasso and identifying coefficients
model1 = glmnet(factors_training1, response_training1, alpha = 1, lamda = minlamda1, family = "binomial")
coef(model1, s=minlamda1)

#predicting the reponse variable for testing data

#creating factor matrix of testing variables and vector of response variables for testing data
factors_testing1 = model.matrix(num1~.,data=testing_data1)[,-1]
response_testing1 = testing_data1$num1

#predicting and finding error
prediction1 = predict(model1, s=minlamda1, newx = factors_testing1, type = "response")

# creating vector for interpretation of probabilities
interpretations1 <- rep(0, 99)

#interpreting predicted probabilities
#60% probability allows to achieve better accuracy
interpretations1[prediction1>0.5]=1

#total accuracy of predictions
mean(interpretations1==response_testing1)

############################################Running lasso and logistic regression using num2 as response variable ###################################################
set.seed(1)
heart.data2 = heart.data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,16)]
heart.data2


#Splitting the data into training and testing, where testing - 30% of all raws
testing2 = sample(1:nrow(heart.data2), 99)
training2 = -testing2
testing_data2 = heart.data2[testing2,]
training_data2 = heart.data2[training2,]

# creating matrixes and vector of response variables for training data
factors_training2 = model.matrix(num2~.,data=training_data2)[,-1]
response_training2 = training_data2$num2

#running lasso on training data
lasso2 = cv.glmnet(factors_training2, response_training2, alpha = 1, family = "binomial")

# identifying best lambda
minlamda2=lasso2$lambda.min


# use best lambda for lasso and identifying coefficients
model2 = glmnet(factors_training2, response_training2, alpha = 1, lamda = minlamda2, family = "binomial")
coef(model2, s=minlamda2)

#predicting the reponse variable for testing data

#creating factor matrix of testing variables and vector of response variables for testing data
factors_testing2 = model.matrix(num2~.,data=testing_data2)[,-1]
response_testing2 = testing_data2$num2

#predicting and finding error
prediction2 = predict(model2, s=minlamda2, newx = factors_testing2, type = "response")

# creating vector for interpretation of probabilities
interpretations2 <- rep(0, 99)

#interpreting predicted probabilities
#60% probability allows to achieve better accuracy
interpretations2[prediction2>0.5]=1

#total accuracy of predictions
mean(interpretations2==response_testing2)

############################################Running lasso and logistic regression using num3 as response variable ###################################################
set.seed(1)
heart.data3 = heart.data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,17)]


#Splitting the data into training and testing, where testing - 30% of all raws
testing3 = sample(1:nrow(heart.data3), 99)
training3 = -testing3
testing_data3 = heart.data3[testing3,]
training_data3 = heart.data3[training3,]

# creating matrixes and vector of response variables for training data
factors_training3 = model.matrix(num3~.,data=training_data3)[,-1]
response_training3 = training_data3$num3

#running lasso on training data
lasso3 = cv.glmnet(factors_training3, response_training3, alpha = 1, family = "binomial")

# identifying best lambda
minlamda3=lasso3$lambda.min


# use best lambda for lasso and identifying coefficients
model3 = glmnet(factors_training3, response_training3, alpha = 1, lamda = minlamda3, family = "binomial")
coef(model3, s=minlamda3)

#predicting the reponse variable for testing data

#creating factor matrix of testing variables and vector of response variables for testing data
factors_testing3 = model.matrix(num3~.,data=testing_data3)[,-1]
response_testing3 = testing_data3$num3

#predicting and finding error
prediction3 = predict(model3, s=minlamda3, newx = factors_testing3, type = "response")

# creating vector for interpretation of probabilities
interpretations3 <- rep(0, 99)

#interpreting predicted probabilities
#60% probability allows to achieve better accuracy
interpretations3[prediction3>0.5]=1

#total accuracy of predictions
mean(interpretations3==response_testing3)


############################################Running lasso and logistic regression using num4 as response variable ###################################################
set.seed(1)
heart.data4 = heart.data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,18)]


#Splitting the data into training and testing, where testing - 30% of all raws
testing4 = sample(1:nrow(heart.data4), 99)
training4 = -testing4
testing_data4 = heart.data4[testing4,]
training_data4 = heart.data4[training4,]

# creating matrixes and vector of response variables for training data
factors_training4 = model.matrix(num4~.,data=training_data4)[,-1]
response_training4 = training_data4$num4

#running lasso on training data
lasso4 = cv.glmnet(factors_training4, response_training4, alpha = 1, family = "binomial")

# identifying best lambda
minlamda4=lasso4$lambda.min


# use best lambda for lasso and identifying coefficients
model4 = glmnet(factors_training4, response_training4, alpha = 1, lamda = minlamda4, family = "binomial")
coef(model4, s=minlamda4)

#predicting the reponse variable for testing data

#creating factor matrix of testing variables and vector of response variables for testing data
factors_testing4 = model.matrix(num4~.,data=testing_data4)[,-1]
response_testing4 = testing_data4$num4

#predicting and finding error
prediction4 = predict(model4, s=minlamda4, newx = factors_testing4, type = "response")

# creating vector for interpretation of probabilities
interpretations4 <- rep(0, 99)

#interpreting predicted probabilities
#60% probability allows to achieve better accuracy
interpretations4[prediction4>0.5]=1

#total accuracy of predictions
mean(interpretations4==response_testing4)








###################################################################################################33

#Hypothesis 2- Cluserting on the dataset
#Creating the dataset
heart.data <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Project/processed.cleveland.data",sep=",",na.strings = '?')


names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data <- na.omit(heart.data)

# set random seed so that our results are reproducible
RNGkind(sample.kind = "Rounding")
set.seed(1)

#convert heart disease to 0s and 1s
heart.data$num[heart.data$num > 0] <- 1

#converting to factors
heart.data$sex<-as.factor(heart.data$sex)
heart.data$cp<-as.factor(heart.data$cp)
heart.data$fbs<-as.factor(heart.data$fbs)
heart.data$restecg<-as.factor(heart.data$restecg)
heart.data$exang<-as.factor(heart.data$exang)
heart.data$slope<-as.factor(heart.data$slope)
heart.data$ca<-as.factor(heart.data$ca)
heart.data$thal<-as.factor(heart.data$thal)
heart.data$num<-as.factor(heart.data$num)



#Clustering for age,sex,cp,bp,ecg,thalach,exang,peak,slope,ca,thal
heart_1 = scale(heart.data[,c(1,2,3,4,7,8,9,10,12,13,14)])


#K means clustering
km.out=kmeans(heart_1,2,nstart=20)
km.out$betweenss#20.8

heart.clust=hclust(dist(heart_1),method="complete")
km.out=cutree(heart.clust,2) 
km.out

#Attempting with Hierachial clustering since the k mean clustering has accuracy of 20.8%



#Performing Heirachial clustering
heart.clust=hclust(dist(heart_1),method="complete")
km.out=cutree(heart.clust,2) 
km.out

#Cluster 2- 131 and cluster 1-165
h1=ifelse(heart.data$num[km.out==1]>=1,1,0)
length(which(h1==1))#31
length(which(h1==0))#143

h2=ifelse(heart.data$num[km.out==2]>=1,1,0)
length(which(h2==1))#106
length(which(h2==0))#16


#Find the mean age of customers for both the clusters
mean(heart.data$age[km.out==1])

mean(heart.data$age[km.out==2])

#Determining the ratio of sexes
h3=ifelse(heart.data$sex[km.out==2]==1,'Male','Female')
table(h3)

h4=ifelse(heart.data$sex[km.out==1]==1,'Male','Female')
table(h4)

#Find the mean cholestrol levels of customers for both the clusters
mean(heart.data$chol[km.out==1])

mean(heart.data$chol[km.out==2])

#Find the mean BP levels of customers for both the clusters
mean(heart.data$trestbps[km.out==1])

mean(heart.data$trestbps[km.out==2])

#Check for presence of Sugar
h5=ifelse(heart.data$sex[km.out==1]==1,'Yes','No')
table(h5)

h6=ifelse(heart.data$sex[km.out==2]==1,'Yes','No')
table(h6)

#Determining the ratio of ECG
table(heart.data$restecg[km.out==1])

table(heart.data$restecg[km.out==2])

#Thalach levels
mean(heart.data$thalach[km.out==1])

mean(heart.data$thalach[km.out==2])

#SUmmary of Exang data
h7=ifelse(heart.data$exang[km.out==1]==1,'1','0')
table(h7)

h7=ifelse(heart.data$exang[km.out==2]==1,'1','0')
table(h7)

#Summary of Peak
mean(heart.data$oldpeak[km.out==1])

mean(heart.data$oldpeak[km.out==2])

#Summary of CA values
table(heart.data$ca[km.out==1])

table(heart.data$ca[km.out==2])

#Summary of thal values
table(heart.data$thal[km.out==1])

table(heart.data$thal[km.out==2])




# Plotting data,
heart.data$num[heart.data$num > 0] <- 1

#Cholestrol VS age
plot(heart.data$age, heart.data$chol, col=km.out,pch=km.out,xlab="Age", ylab="chol") 


#display the presence of heart disease for each point
text(x=heart.data$age+0.5, y=heart.data$chol-1.25,labels=heart.data$num,col=km.out,pch=km.out)

#Age vs thal
plot(heart.data$age, heart.data$thal, col=km.out$cluster,pch=km.out$cluster,xlab="num", ylab="thal") 

#display the presence of heart disease for each point
text(x=heart.data$num, y=heart.data$thal,labels=heart.data$num,col=km.out$cluster,pch=km.out$cluster)

#AGe vs sex
plot(heart.data$sex, heart.data$age,col=km.out$cluster,pch=km.out$cluster,xlab="sex",ylab="age") 

#display the presence of heart disease for each point
text(x=heart.data$age, y=heart.data$sex,labels=heart.data$thal,col=km.out$cluster,pch=km.out$cluster)

#Age vs presence of heart disease
plot(heart.data$num, heart.data$age,col=km.out$cluster,pch=km.out$cluster,xlab="Disease", ylab="Age") 

#display the presence of heart disease for each point
text(x=heart.data$num, y=heart.data$age,labels=heart.data$num,col=km.out$cluster,pch=km.out$cluster)

heart.data$num = ifelse(heart.data$num>=1,1,0)
#Age vs blood pressure
plot(heart.data$trestbps, heart.data$age,col=km.out,pch=km.out,xlab="BP", ylab="Age") 

#display the presence of heart disease for each point
text(x=heart.data$trestbps, y=heart.data$age-0.5,labels=heart.data$num,col=km.out,pch=km.out)


##################################################################################################

#Hypothesis 3- Logistic Regression on angina

#Assigning a new column called as blood pressure and splitting based on the median value.
heart.data$trestbps01 = 1
heart.data$trestbps01[heart.data$trestbps < median(heart.data$trestbps)] = 0

#Assigning a new column called thalach(Maximum heart rate achieved) and splitting based on the median value
heart.data$thalach01 = 1
heart.data$thalach01[heart.data$thalach < median(heart.data$thalach)] = 0

#Splitting the data into training and testing datasets
train=sample(1:nrow(heart.data), nrow(heart.data)/2)
test = (-train)

#creting datasets for logistic regression
heart.train = heart.data[train,c(4,8,9,15,16)]
heart.test = heart.data[-train,c(4,8,9,15,16)]

heart1.train =heart.data[train,]
heart1.test = heart.data[test,]

exang.train = heart.data$exang[train]
exang.test = heart.data$exang[test]



#Running logistic regression
library(glmnet)

logreg.fit = glm(heart.train$exang~thalach+trestbps, data = heart.train, family = binomial)
summary(logreg.fit)

#Dropping blood pressure and running logistic regression again
logreg.fit2 = glm(heart.train$exang~thalach, data = heart.train, family = binomial)
summary(logreg.fit2)

#Checking accuracy of the model
logreg.fit2.prob = predict(logreg.fit2, data = heart.test, type = "response")
logreg.fit2.pred = rep(0,148)
logreg.fit2.pred[logreg.fit2.prob>0.5] = 1

heart.test.output = heart.data[-train,c(9)]

mean(logreg.fit2.pred==heart.test.output)

