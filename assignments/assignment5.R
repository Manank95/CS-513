## Manank Valand
## 10429101
## Reference from Lecture code in R for C50.

rm(list=ls())
#install.packages("C50", repos="http://R-Forge.R-project.org")
#install.packages("C50")
#install.packages("libcoin")
library('C50')


??C5.0
dsn<-read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv")

?factor
# for the last column - diagnosis Class
cats<-factor(dsn[,11])
dsn<-cbind(dsn[,2:10],cats)
attach(dsn)

c5tree<-C5.0(cats~.,data=dsn)

summary(c5tree)

c5tree
plot(c5tree)

# just for reference