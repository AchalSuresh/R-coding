# set your working directory


# read the data in Hollywood_rules into data.frame called Hollywood_rules
Hollywood_rules <- read_excel("./R-coding/Data/Hollywood_rules.xls", sheet = "Exhibit 1")

#Create a dummy variable to differentiate comedy films from noncomedy films
Hollywood_rules$comedyYes <- 0
Hollywood_rules$comedyYes[Hollywood_rules$Genre == "Comedy"] <- 1

#As a first step, we run a standard linear regression and check the validity of the basic assumptions that have to be met for the linear regression to be sound
m1<-lm(`Total U.S. Gross`~Budget+comedyYes+MPAA_D+`Known Story`+Sequel,data=Hollywood_rules)
summary(m1)

#Check normality of the residuals
plot(m1,2)#from this plot, we can conclude that normality of residuals is questionable as several points don't fall along the regression line

#Check equality of variance in the residuals
plot(m1,1)# Notice the expanding variability as the fitted values increase. Equal variance assumption is not met

#We cannot check independence as we don't know how the data was collected

#Let's try transforming the response. Let's apply the log transformation as it is the most usual
m1t<-lm(log(`Total U.S. Gross`)~Budget+comedyYes+MPAA_D+`Known Story`+Sequel,data=Hollywood_rules)
summary(m1t)

#Check normality of the residuals
plot(m1t,2)#normality assumption is more clear for this model than for the previous one

#Check equality of variance in the residuals
plot(m1t,1)# homogeneity of variance is better too.

#We cannot check independence as we don't know how the data was collected

#The resulting regression is 

#LN(Total U.S. Gross) = 16.88 + (1.408*10^-08)Budget + (0.337)ComedyYes + (0.068)MPAA_D - (0.127)`Known Story` + 0.533Sequel

#5b
#Dropping non-significant variables

m2t<-lm(log(`Total U.S. Gross`)~Budget+comedyYes+Sequel,data=Hollywood_rules)
summary(m2t)

#The resulting regression is 

#LN(Total U.S. Gross) = 16.84 + (1.408*10^-08)Budget + (0.33)ComedyYes + 0.57Sequel

#5c
#Total U.S Gross for sequels = e^(16.84 + (1.408*10^-08)Budget + (0.33)ComedyYes))E^(0.57*1)
#Total U.S Gross for non-sequels = e^(16.84 + (1.408*10^-08)Budget + (0.33)ComedyYes))E^(0.57*0)

#Holding Budget and ComedyYes constant we get the constant k 
#k = e^(16.84 + (1.408*10^-08)Budget + (0.33)ComedyYes))

#Hence,
#Total U.S Gross for sequels = 1.768k
#Total U.S Gross for non-sequels = k
#(1.768-1)*100 = 77%

#Conclusion: Holding all variables constant, sequels produce a U.S. gross (1.768-1=0.768)*100 higher than non-sequels

