# Manank Valand - 10429101
# Reference from lecture notes on R and Assignment 2 submission
rm(list=ls())

#reference from class lecture tutorials and stackoverflow.com

# 1. Load CSV and Summarizing each column
dsn <- read.csv("C://Users/manan/Desktop/R/midterm/IBM_Attrition_v3.csv",header=TRUE,na.strings="?")
#dsn1 <- dsn[,-c(1)]
View(dsn)
summary(dsn)

#2. Identifying the missing values
sapply(dsn, function(x) sum(is.na(x)))

#3.	Displaying the frequency table of "Attrition" vs. "MaritalStatus"
table(dsn$Attrition,dsn$MaritalStatus)

#4. Displaying the scatter plot of "Age", "MaritalStatus" and "YearsAtCompany", one pair at a time
pairs(dsn[c("Age", "MaritalStatus", "YearsAtCompany")])

#5. Show histogram box plot for columns:  "Age", "MaritalStatus" and "YearsAtCompany"
par(mfrow=c(2,3)) #making frames to display properly in 2 rows and 3 columns
hist(dsn$Age)
#changing values in numbers 1=Married, 2=Divorced, 3=Single
hist(as.numeric(dsn$MaritalStatus))
hist(dsn$YearsAtCompany)
boxplot(dsn[c("Age", "MaritalStatus", "YearsAtCompany")])

#6 Replacing the missing values of "MonthlyIncome" with the "mean" of "MonthlyIncome".
myMean <- mean(dsn$MonthlyIncome, na.rm=TRUE)
#round(myMean)
dsn[is.na(dsn$MonthlyIncome),"MonthlyIncome"]<- as.integer(myMean)
View(dsn)
