# Manank Valand - 10429101
# Reference from lecture notes on R and Assignments
# reference from class lecture tutorials and stackoverflow.com
rm(list=ls())

# reference https://www.statmethods.net/management/subset.html
# reference https://stackoverflow.com/questions/15030910/randomly-sample-a-percentage-of-rows-within-a-data-frame?noredirect=1&lq=1

dsn <- read.csv("C://Users/manan/Desktop/R/midterm/IBM_Attrition_v3.csv",header=TRUE,na.strings="?")
#install.packages("knn")
library(class)

# replacing NA with mean of column
myMean <- mean(dsn$MonthlyIncome, na.rm=TRUE)
dsn[is.na(dsn$MonthlyIncome),"MonthlyIncome"]<- as.integer(myMean)

#changing the values in marital status to numbers
dsn$MaritalStatus<-as.integer(dsn$MaritalStatus)
View(dsn)

# selecting random 30% sample of dataset
# we need to use round here because we will get the values in float
id<- sample(nrow(dsn), round(0.3*nrow(dsn)))
sorted <- sort(id)
# sorting and making test and training dataset
test<-dsn[sorted,]
training<-dsn[-sorted,]

# finding knn with k=3 attrition rate column no. is 6
predict <- knn(training[,-6], test[,-6], training[,6], k=3)
table(Prediction=predict, Actual=test[,6])
# finding prediction attrition rate
predicted_attrition_rate <-sum(predict==test[,6])/length(sorted)
predicted_attrition_rate

