rm(list=ls())
dsn <- read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv",header=TRUE,na.strings="?")

#remove the rows with missing values
dsn1<-na.omit(dsn)

library(class)


#store every fifth record in test
idx<-seq(from=1,to=nrow(dsn1),by=5)
test<-dsn1[idx,]

#store the rest in training
training<-dsn1[-idx,]

#use knn with k=1 and classify the test dataset
p1<-knn(training[,-5],test[,-5],training[,5],k=1)

correct1<-test[,5]==p1
correct_rate1<-sum(correct1)/length(correct1)
correct_rate1

#measure the performance
table(Prediction=p1,Actual=test[,5] )

#f.	Repeat the above steps with k=2, k=5, k=10.

