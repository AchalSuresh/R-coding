# BU510.650 Data Analytics Week 1
# Exploring data using R
# Example: Titanic

# As we enter commands in the command line, you can copy-paste those into an "R Script," which you could reload and use later if you want to. In R Studio, click the new document button in the top-left corner and select "R Script".

# Next we want R to read the data set we want to work with. In R Studio, on the top-right pane, click "Import Dataset." Browse to titanic.csv and select it. Click "Import". 
titanic <- read.csv("./R-coding/Data/titanic.csv")
# When R reads a .csv file, it stores the data into an object type called a "data frame." Let's check what types of variables we have in titanic. We use "str" command.
str(titanic)
# Let's summarize the information in the Survived column. We can use "titanic$Survived" to refer to the "Survived" column of "titanic" data frame. The "table" function will then go through the column and count the occurrence of each value (in this case how many times 0s and 1s occurred in the "Survived" column, with 0 corresponding to a passenger who died and 1 corresponding to a passenger who survived).
table(titanic$Survived)
# The function prop.table will convert the numbers in the table to proportions.
prop.table(table(titanic$Survived))
# The function "summary" summarizes the information contained in this argument. Let us summarize the information in the Sex column, which shows each passenger's gender.
summary(titanic$Sex)
# We can also create a multi dimensional table, say a table that distributes the passengers according to their gender and whether they survived or not.
titanic_summ <- table(titanic$Sex, titanic$Survived)
titanic_summ

# Let's convert the numbers into proportions.
prop.table(table(titanic$Sex, titanic$Survived))
# Let's use prop.table so that the proportions add up to 100% in each row. Use prop.table(...,2) to make proportions add up to 100% in each column
prop.table(table(titanic$Sex, titanic$Survived),1)
# Now, let us summarize the information in the Age column.

help("summary.connection")
summary(titanic$Name)
# We will add another column to our data frame. This column will be titled "Child" and it will be equal to 1 if the passenger is less than 18 years old and 1 otherwise.
titanic$Child=0
titanic$Child[titanic$Age<18]=1
# We will use "aggregate" function to count how many passengers survived, grouping the passengers according to their gender and whether or not they were children.  The "aggregate" function divides the data into groups according to the variable to the right of "~" and applies the function specified in "FUN" to the variable to the left of "~". In this case, it will add the numbers (0s and 1s in the Survived column) for four different groups of passengers: Male children, female children, male adults, female adults.
aggregate(Survived ~ Child + Sex, data=titanic, FUN=sum)
# We will use "aggregate" function again, this time to count how many passengers belong to each of the four different groups (male children, female children, male adults, female adults). We can achieve that by setting "FUN = length".
aggregate(Survived ~ Child + Sex, data=titanic, FUN=length)

aggregate(Survived ~ Child + Sex, data=titanic, FUN=mean)

# Finally, we will use "aggregate" function again, this time to find the proportion that survived in each group. Here, "FUN" is set to a function, which we define.
aggregate(Survived ~ Child + Sex, data=titanic, FUN=function(x) {sum(x)/length(x)})
# Next we add a new column to our data frame, called "Fare2", to indicate how expensive the fare was for each passenger. We will group them into four: those who paid $30+, between 20 and 30, between 10 and 20, and less than 10.
titanic$Fare2<-"30+"
titanic$Fare2[titanic$Fare < 30 & titanic$Fare >= 20] <- "20-30"
titanic$Fare2[titanic$Fare < 20 & titanic$Fare >= 10] <- "10-20"
titanic$Fare2[titanic$Fare < 10] <- "<10"

#Finally, we check how passengers did when we group them according to their fare category, travel class, and gender. We use the "aggregate" function again.
aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic, FUN=function(x) {sum(x)/length(x)})
