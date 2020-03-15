heart.data <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Project/processed.cleveland.data",sep=",",na.strings = '?')

names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data = na.omit(heart.data)



#Creating a copy of the data Frame
heart.data_1 = heart.data

#Select all values of num greater than 0 as 1
heart.data_1$num[heart.data$num > 0] <- 1

#Proportion of people having heart diseases
prop.table(table(heart.data_1$num))

#column to check for Cholesterol and high BP
heart.data_1$chol = ifelse(heart.data$chol>240,1,0)
heart.data_1$trestbps = ifelse(heart.data$trestbps>130,1,0)


#Proportion of people having heart disease based on sex of the person
table(heart.data_1$num, heart.data_1$sex)
prop.table(table(heart.data_1$num, heart.data_1$sex))

#Proportion of people having heart disease based on blood pressure of the person
prop.table(table(heart.data_1$num, heart.data_1$trestbps))

#Proportion of people having heart disease based on Cholesterol of the person
prop.table(table(heart.data_1$num, heart.data_1$chol))

#Proportion between sex and Cholesterol
prop.table(table(heart.data_1$sex, heart.data_1$chol))



par(mfrow=c(1,1)) 


#Plots for different combinations
#Presence of heart disease and Cholesterol levels
boxplot(heart.data$chol~heart.data_1$num,xlab = "Disease",ylab = "Cholesterol",main="Cholesterol and Occurance of Disease")

#Presence of heart disease and chest pain levels
boxplot(heart.data$cp~heart.data_1$num,xlab = "Disease",ylab = "cp", main="Chest pain and Presence of disease")


#Presence of heart disease and blood pressure levels
boxplot(heart.data$trestbps~heart.data_1$num,xlab = "Disease",ylab = "bp",main="Blood Pressure and Occurance of Disease")

#Presence of heart disease and Thalassemia
boxplot(heart.data_1$num~heart.data_1$thal,xlab = "Thalassemia",ylab = "number of diseases",main="Thalassemia and Occurance of Disease")

#Presence of heart disease and Age
boxplot(heart.data_1$age~heart.data_1$num,xlab = "Disease",ylab = "bp",main="Age and Occurance of Disease")

#Presence of Thalassemia and Cholesterol
boxplot(heart.data_1$chol~heart.data_1$thal,xlab = "Thalassemia",ylab = "Cholesterol",main="Thalassemia and Cholesterol Levels")

#Presence of Thalassemia and blood Pressure
boxplot(heart.data_1$trestbps~heart.data_1$thal,xlab = "Thalassemia",ylab = "BP",main="Thalassemia and Blood Pressure Levels")




#Finding the age group with the highest number of heart diseases
aggregate(num ~ age + sex, data=heart.data_1, FUN=sum)

aggregate(num ~ age + sex, data=heart.data_1, FUN=length)

aggregate(num ~ age + sex, data=heart.data_1, FUN=function(x) {sum(x)/length(x)})


