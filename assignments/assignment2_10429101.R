# Manank Valand - 10429101
# Reference from lecture notes on R

rm(list=ls())
#reference from class lecture tutorials and stackoverflow.com
#reading csv file with header and skipping 1st column and summary of all of them
dsn <- read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv",header=TRUE,na.strings="?")
dsn1 <- dsn[,-c(1)]
View(dsn) 
summary(dsn)

#identifying the missing values
sapply(dsn1, function(x) sum(is.na(x)))
#naData <- sapply(dsn1, function(x) sum(is.na(x))); naData[naData>0]

#Replacing the missing values with mode of the column with mfv method
library(modeest)

PL_mfv<-mlv(dsn1$F6, method = "mfv",na.rm = TRUE) 
#str(PL_mfv)
PL_mfv$M
#is.na(dsn1$F6)
dsn1[is.na(dsn1$F6),"F6"]<-PL_mfv$M

#displaying frequeny table of class vs F6
table(class=dsn1[,10],F6=dsn1[,6])
#table(dsn1$class,dsn1$F6)


#scater plot of f1 to f6
pairs(dsn1[,1:6])

#histogram boxplot for f7 to f9
par(mfrow=c(2,3))
hist(dsn1$F7)
hist(dsn1$F8)
hist(dsn1$F9)
boxplot(dsn1[7:9])

#Q2.delete all objects
rm(list=ls())
dsn <- read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv",header=TRUE,na.strings="?")
dsn2<-na.omit(dsn)
View(dsn2)