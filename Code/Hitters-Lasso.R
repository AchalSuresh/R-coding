######################################################
############# Example: Hitters -- LASSO
#######################################################

## TASK 1 ##

# as in the previous example, we load the library ISLR, which has
# the Hitters dataframe and we remove the records with missing values
library(ISLR)
Hitters2=na.omit(Hitters)
View(Hitters2)

# for the library we will use for ridge regression and LASSO, we need
# to convert the categorical variables to binary variables
# The command model.matrix() automates this task. It prepares the dataset to be used is an regression.
# Salary~ tells the function that this is the response and hence it is not included in the set of predictor
# For categorical variables, model.matrix creates binary variables (numerical)
# x will hold the data for predictors but excluding the first column "intercept" created by default
x=model.matrix(Salary~.,data=Hitters2)[,-1] 

# the vector y will hold the data for the response, salary in this case
y=Hitters2$Salary

# display the first six rows of x and y
head(x)
head(y)

## TASK 2 ##

library(glmnet)

# run LASSO with lambda = 0 and store results in a variable called lasso.1
lasso.1=glmnet(x,y,alpha=1,lambda=0)#alpha=1 indicates it is lasso operation

# display the coefficient estimates
# Because lambda = 0, these should be the same as what we would get if we ran multiple regression
coef(lasso.1)

# run LASSO with lambda = 100000 and store results in a variable called lasso.2
lasso.2=glmnet(x,y,alpha=1,lambda=100000)

# display the coefficient estimates
# Because lambda is very large, these should be 0 or close to 0
coef(lasso.2)

# run LASSO with lambda = 100 and display the coefficient estimates
lasso.3=glmnet(x,y,alpha=1,lambda=100)
coef(lasso.3)

## TASK 3 ##

# create a vector with 100 elements, which we will call grid, starting from 10^2 and going to 10^-2 
grid=10^seq(2,-2,length=100)

# display the vector called grid -- these will be the range of lambda values
grid

# run LASSO for all values of lambda in the vector grid 
# and store the results in a variable called lasso.mod
lasso.mod=glmnet(x,y,alpha=1,lambda=grid) 

# check the dimension of coef(lasso.mod) -- it will be 20 rows and 100 columns: 
# one column for each possible value of lambda and one row for each coefficient
dim(coef(lasso.mod))

# check the 50th lambda value and display the coefficient estimates for that lambda
lasso.mod$lambda[50]
coef(lasso.mod)[,50]

# check the 60th lambda value and display the coefficient estimates for that lambda
lasso.mod$lambda[60]
coef(lasso.mod)[,60]

## TASK 4 ##

# the following command will yield the coefficient estimates for lambda = 50
predict(lasso.mod, s=50, type="coefficients")[1:20,]

## TASK 5 ##

# set a random number seed so that our results are "reproducible"
set.seed(1)

# nrow(x) will give us the number of rows in x. we are randomly picking 
# half of those rows to be our training data. The variable train will store the row numbers 
# in our training data.
train=sample(1:nrow(x), nrow(x)/2) # split data into two subsets
train
# the variable test will store the numbers of remaining rows, which will be our test data(It will store negative of all other values)
test=(-train)
test

# next, we create the part of x and y that will be our training data
# we will call these x.train and y.train
x.train=x[train,] # the comma helps to select the number of columns
y.train=y[train]

# and, we create the part of x and y that will be our test data
# we will call these x.test and y.test
x.test=x[test,]
y.test=y[test]

## TASK 6 ##

# run LASSO using only the training data
lasso.mod=glmnet(x.train, y.train, alpha=1, lambda=grid,thresh=1e-12)#thresh- termination criteria

## TASK 7 ##
# predict the salaries for the test data using lambda=4
lasso.pred = predict(lasso.mod, s=4, newx=x.test)

lasso.pred
# find MSE, the mean of squared prediction errors on the test data
mean((lasso.pred - y.test)^2)

## TASK 8 ##

# Use 10-fold cross-validation on the training data, to determine the best lambda value
# nfold = 10 is default. Otherwise, you can change the default by using cv.glmnet(...,nfold=k), where k is a number.
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)

# plot the MSE observed in the cross-validation as a function of log(lambda)
plot(cv.out)

# determine which lambda minimized the MSE, call it bestlam, and display it
bestlam=cv.out$lambda.min
bestlam

## TASK 9 ##

#final model with best lambda

lasso.final=glmnet(x, y, alpha=1, lambda=bestlam)
coef(lasso.final)

## TASK 10 ##

# predict the salaries for the test data using the best lambda. 
lasso.pred = predict(lasso.final, s=bestlam, newx=x[test,])

# find MSE, the mean of squared prediction errors on the test data
mean((lasso.pred - y.test)^2)


