library(readxl)
# Load the excel sheet into R
hollywood <- read_excel("./R-coding/Data/KEL702-XLS-ENG-1740.XLS", sheet="Exhibit 1")

View(hollywood)

#dummy Variable to differentiate between Conedy and Non-comedy
hollywood$comedyYes <- 0
hollywood$comedyYes[hollywood$Genre == "Comedy"] <- 1


#6a linear model having preroduction factors and opening day factors
m1<-lm(`Opening Gross`~Budget+comedyYes+MPAA_D+`Known Story`+Sequel+`Opening Theatres` + Summer + Holiday + Christmas,data=hollywood)
summary(m1)

#normality plot to check for normal distribution
qqnorm(m1$res)
plot(m1,2)

#Variance check for the model
plot(m1,1)

#Since the variance of the residual is not constant, transforming the model
mlog<-lm(log(`Opening Gross`)~Budget+comedyYes+MPAA_D+`Known Story`+Sequel+`Opening Theatres` + Summer + Holiday + Christmas,data=hollywood)
summary(mlog)

#normality plot to check for normal distribution
qqnorm(m1$res)
plot(mlog,2)

#Variance check for the model
plot(mlog,1)

#6b-Dropping the non-sgnificant parameters
mlog1<-lm(log(`Opening Gross`)~Budget+Sequel+`Opening Theatres` + Summer,data=hollywood)
summary(mlog1)

#6d
t1 = qt(c(.025, .975), df=70)
#Confidence Interval for the coefficients estimate
c1= 4.904*10^-04
cfu =  c1 + 1.99* (7.083*10^-05) #Upper estimate
cfl =  c1 - 1.99* 7.083*10^-05 #Lower Estimate
  

#Opening Gross for a movie
#Opening Gross = e^(14.494+5.063*(10^-09)*Budget + 3.620*(10^-01)*Sequel + 4.904*(10^-04)*Opening Theatre + -0.2337*Summer)

#Holding Budget,Sequel and Summer Constant which can be represented as k
#Opening Gross = k* e^(4.904*(10^-04)*Opening Theatre)

#Point Estimate for 100 Movie theatres
OG =exp(4.904*(10^-04)*100) # OG for 100 theatres
#OG = 1.0502
#Opening Gross Increases by 1.05*the old value 

#Confidence Interval
OGU = exp(cfu*100)#ogl=1.0355
OGL = exp(cfl*100)#ogu=1.065

# The opening Gross COnfidence Interval will be from 1.03  and 1.06 times the old value
