install.packages("readxl")

# Install package to read excel file
library(readxl)
# Load the excel sheet into R
hollywood <- read_excel("C:/Users/achal/Desktop/Carey/Data Analytics/Data/KEL702-XLS-ENG-1740.XLS", sheet="Exhibit 1")

#Summarize the information in the file

summary(hollywood[c(2,3,6,4,10,15)])

#Group the movies by Genre ad MPAA
table(hollywood$Genre)
table(hollywood$MPAA)

#Determine the proportion of movies by Genre and MPAA
prop.table(table(hollywood$Genre))
prop.table(table(hollywood$MPAA))

# create a column ROI and 
hollywood["ROI"] <- ((hollywood$`Total U.S. Gross`- hollywood$Budget)/hollywood$Budget)

# To determine the average of ROI fo reach Genre
aggregate(ROI ~ Genre, data=hollywood, FUN=mean)

# To determine the average of Opening Gross fo reach Genre
aggregate(`Opening Gross` ~ Genre, data=hollywood, FUN=mean)

#To create a subset of data having only comedy movies
comedymov <- hollywood[hollywood$Genre =='Comedy',]

#To create a Boxplot
boxplot(ROI~MPAA,data=comedymov)
