# Loading the dataset Auto and ommiting Na values
Auto <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Code/Auto(2).csv" ,na.strings = "?")

Auto2=na.omit(Auto)

#1a) Creating a column mpg01 to the data frame
mpg01 = ifelse(Auto2$mpg > median(Auto2$mpg), "1", "0")
Auto2 = data.frame(Auto2,mpg01)

#b) Splitting the data into training and testing
RNGkind(sample.kind = "Rounding")
set.seed(1)

train=sample(1:nrow(Auto2),nrow(Auto2)/2)

test=(-train)

#c) matrix containing Training data
Auto2.train = Auto2[train,c(5,7,8)]
Auto2.test

#matrix containing Testing data
Auto2.test = Auto2[test,c(5,7,8)]

#vector containing the responses for training data
mpg01.train=mpg01[train]
mpg01.train

#vector containing the responses for testing data
mpg01.test=mpg01[test]
#mpg01.test


library(class)

# run knn with K=1 and store prediction results in a variable called knn.pred_01
knn.pred_01 = knn(Auto2.train,Auto2.test,mpg01.train,k=1)

#Checking the accuracy of the model
table(knn.pred_01,mpg01.test)
mean(knn.pred_01==mpg01.test) #83.67%

#MOdel with k=3
knn.pred_03 = knn(Auto2.train,Auto2.test,mpg01.train,k=3)

table(knn.pred_03,mpg01.test)
mean(knn.pred_03==mpg01.test) #87.24%

#Model with k=5
knn.pred_05 = knn(Auto2.train,Auto2.test,mpg01.train,k=5)

table(knn.pred_05,mpg01.test)
mean(knn.pred_05==mpg01.test) #88.26%

################################################################################
#2a)

# load library ISLR and view the data frame Carseats
library(ISLR)
View(OJ)

help(OJ)

#a)
RNGkind(sample.kind = "Rounding")
set.seed(18) 

#Load 800 rows to training set
train = sample(1:nrow(OJ), 800)
sample()

#The rest of the rows are assigned to test
test = (-train)
OJ.test = OJ[test,]
OJ.test

# store the actual Purachase details for test data in a vector called Purachase.test 
Purchase.test = OJ$Purchase[test]


#b) Fitting a decision Tree with Purchase as response for training data
library(tree)
tree.OJ = tree(Purchase ~ ., OJ, subset=train)

summary(tree.OJ)

#c)Plotting the decision Tree
plot(tree.OJ)
text(tree.OJ,pretty=0)


#e) Using Prediction on the test data
# the predictions are stored in a vector called tree.pred
  tree.pred = predict(tree.OJ,OJ.test,type="class")

# display a table that shows predictions for test data versus actuals for test data
table(tree.pred, Purchase.test)

# calculating prediction accuracy
mean(tree.pred==Purchase.test)


#f)

cv.OJ = cv.tree(tree.OJ,FUN=prune.misclass)
summary(cv.OJ)
cv.OJ

#g) 
# Creating the decision tree with the best prediction accuracy
prune.OJ = prune.misclass(tree.OJ,best=4)
plot(prune.OJ)
text(prune.OJ, pretty = 0)


#h) the predictions are stored in a vector called tree.pred_1
tree.pred_1 = predict(prune.OJ,OJ.test,type="class")

# display a table that shows predictions for test data versus actuals for test data
table(tree.pred_1, Purchase.test)

# calculate prediction accuracy
mean(tree.pred_1==Purchase.test)#84.44%

