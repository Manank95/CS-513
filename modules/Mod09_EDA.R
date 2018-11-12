#################################################
#  Company    : Stevens 
#  Project    : R Bootcamp 
#  Purpose    : EDA
#  First Name  : Khasha
#  Last Name  : Dehnad
#  Id			    : 12345
#  Date       : replacing and removing missing values
#  Comments   : only for reference

rm(list=ls())


data(iris)
View(iris)

length(iris)
nrow(iris)
iris_missing<-iris
iris_missing[c(3,30,40),3]

iris_missing[c(3,30,40),3]<-NA

summary( iris_missing)



?boxplot()
boxplot(iris_missing[1:3])
?hist()
hist(iris_missing$Sepal.Length)
?pairs()
pairs(iris[1:2] )
 
pairs(iris[1:4])
pairs(iris[1:4], main = "Iris Data ",      pch = 10 )

 pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[factor(iris$Species)])
?plot 
 plot(iris[,1:2])
  

iris_missing<-iris
iris_missing[c(3,30,40),3]<-NA

?na.omit
iris_missing2<-na.omit(iris_missing)

data(iris)
View(iris)
iris_missing<-iris
iris_missing[c(3,30,40),3]<-NA
installed.packages()
?install.packages 
#install.packages("modeest")
library(modeest)
?mlv()

PL_mfv<-mlv(iris_missing$Petal.Length , method = "mfv",na.rm = TRUE) 
str(PL_mfv)
PL_mfv$M
is.na(iris_missing$Petal.Length)
iris_missing[is.na(iris_missing$Petal.Length),"Petal.Length"]<-PL_mfv$M


iris_missing<-iris
iris_missing[c(3,30,40),3]<-NA
#install.packages('VIM')
?VIM
library('VIM')
?kNN

iris_missing2<-kNN(iris_missing,variable ='Petal.Length',k=3,
         dist_var=c('Sepal.Length','Sepal.Width'))