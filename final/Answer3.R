## final q3. 
## Manank Valand
## 10429109

rm(list=ls())
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages('C50')
dsn1<-read.csv('C://Users/manan/Desktop/513 Final Manank/IBM_Employee_Attrition_V2.csv')
dsn<-dsn1[,-c(2,6)]
dsn<-cbind(dsn, Attrition=dat[,2])


#make test and train
index<-seq(1, nrow(dsn), 5)
train<-dsn[-index,]
test<-dsn[index,]

#implementation
library(rpart)
library(rpart.plot)

crt_class<-rpart(Attrition~., data=train)
rpart.plot(crt_class)
# By this output of graph, we can say that, 
# 1. OverTime, 
# 2. MonthlyIncome, and 
# 3. MaritalStatus are top 3 important attributes


crt_pred<-predict(crt_class, test, type="class")
crt_err<-sum(test[,12]!=crt_pred)/nrow(test)
crt_err


library('C50')
C50_attr<-C5.0(Attrition~., data=train)
summary(C50_attr)
plot(C50_attr)

# from the above results, we can say that the most important three attributes are:
# 1. 100.00%	TotalWorkingYears
# 2. 98.72%	OverTime
# 3. 32.06%	MaritalStatus

C50_pred<-predict(C50_attr, test)
table(actual=test[,12], C50=C50_pred)
c50_err<-sum(test[,12]!=C50_pred)/nrow(test)
c50_err
