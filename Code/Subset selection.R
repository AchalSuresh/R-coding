#Load the data into Bikeshare
BikeShare <- read.csv("./R-coding/Data/Bikeshare.csv")

#Adding Weekend Column to the data set
BikeShare$Weekend <- 0

#Assigning Weekend 1 when weekday is 0 or 6 to dentote weekend
BikeShare$Weekend[BikeShare$Weekday==0 | BikeShare$Weekday==6 ] <- 1

#Check for number of columns with NA as data
sum(is.na(BikeShare)) 

#Linear model for Rentals
m1<-lm(Rentals~Temperature+Humidity+Windspeed+Weekend,data=BikeShare)
summary(m1)
# The temperature,humidity and windspeed are statistically significant to the number of rentals

#Linear model for Registered users
m2<-lm(Registered~Temperature+Humidity+Windspeed+Weekend,data=BikeShare)
summary(m2)
# The temperature,humidity,windspeed and weekend are statistically significant to the number of rentals by registered users


#Linear model for Casual users
m3<-lm(Casual~Temperature+Humidity+Windspeed+Weekend,data=BikeShare)
summary(m3)
# The temperature,humidity,windspeed and weekend are statistically significant to the number of casual users




###################################################################################
#3)

#Loading the data set
AutoLoss <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Data/AutoLoss.csv")

#Checking for NA values
sum(is.na(AutoLoss$Losses)) 

#Updating values with ? with NA
AutoLoss <- read.csv("C:/Users/achal/Desktop/Carey/Data Analytics/Data/AutoLoss.csv", na.strings = "?")
sum(is.na(AutoLoss$Losses)) 

#Removing all data with NA
AutoLoss <- na.omit(AutoLoss)
sum(is.na(AutoLoss$Losses)) 

#3a)

library(leaps)

# best subsets with up to 15 predictors
subfull=regsubsets(Losses~.,data=AutoLoss, nvmax=15)
summary(subfull)

# for the best model with 10 predictors are NumbDoorsTwo,BodyStylehatchback, BodystyleSedan,BodyStyleWagon,
# DriveWeelsrwd,Height,EngineSize,Horsepower,PeakRPM,CityMPG

# Find the model with least Cp.
sub.summary=summary(subfull)

which.min(sub.summary$cp) #5
coef(subfull,5) # Determine the coefficients of the model with 5 predictors

#Forward subset selection method
subfull.fwd=regsubsets(Losses~.,data=AutoLoss,nvmax=5, method="forward")
summary(subfull.fwd)
#NumDoorsTwo,BodyStyleSedan,DriveWheelsrwd,Height and CityMPG
#Dispalying the coefficients
coef(subfull.fwd,5) 

#Backward Subset selection method
subfull.bwd=regsubsets(Losses~.,data=AutoLoss,nvmax=5, method="backward")
coef(subfull.bwd,5) 

