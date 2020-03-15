#Task 1. Import the dataset
credit <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Data/Credit.csv")

#Task 2.

#Fit a linear model using balance as the response and 4 predictors
lm.credit4 <- lm(Balance ~ Income + Limit + Cards + Student, data=credit)
summary(lm.credit4)
View(lm.credit4)
#Find the RSS
lm.credit4.RSS <- sum(lm.credit4$residuals^2)
lm.credit4.RSS
SIG <- 99.56^2
SIG1 <- 98.61^2
#Find the AIC
AIC(lm.credit4)
#Find the BIC
AIC(lm.credit4,k=log(400))

#Task 3.

#Fit a linear model using balance as the response and 6 predictors
lm.credit6 <- lm(Balance ~ Income + Limit + Cards + Student + Rating + Age, data=credit)
summary(lm.credit6)
#Find the RSS
lm.credit6.RSS <- sum(lm.credit6$residuals^2)
lm.credit6.RSS
#Find the AIC
AIC(lm.credit6)
#Find the BIC
AIC(lm.credit6,k=log(400))
