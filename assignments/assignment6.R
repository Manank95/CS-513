## Manank Valand
## 10429101
# reference from Lecture codes provided in R.


rm(list=ls())
#install.packages('e1071', dependencies = TRUE)
install.packages("randomForest")
library(randomForest)
library(class) 
library(e1071)



?tabulate()
#tabulate(c(2,3,3,5), nbins = 10);
?naiveBayes()
?table()
?ftable()
?as.character()


set.seed(123)

dsn<-read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv", header=TRUE)

class(dsn)
index<-sort(sample(nrow(dsn),round(.25*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

#class(dsn)
nbclass <- naiveBayes(Class~(F1+F2+F3+F4+F5+F6+F7+F8+F9), data = training)
nbclass
?predict

cat_class<-predict(nbclass,test)
cat_class

table(actual=test[,11], NB=cat_class)
#comparing the prediction to actual.
#dat_class<-cbind(dsn, cat_class)

Error<-(test[,11]!=cat_class)
Error_rate<-sum(Error)/length(Error)
Error_rate


##################################





## Question 2 

rm(list=ls())
dsn1<-read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv", header=TRUE)

set.seed(123)
#index<-sort(sample(nrow(dsn1), round(.25*nrow(dsn1))))
index<-seq(1, nrow(dsn1),by=4)
dsn1$Class <- as.factor(dsn1$Class)
training<-dsn1[-index,]
test<-dsn1[index,]

fit<-randomForest(Class~.,data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction<-predict(fit, test)
table(actual=test[,11],Prediction)
wrong<-(test[,11]!=Prediction)
error<-sum(wrong)/length(wrong)
error

# Answer: Top 3 features according to MeanDecreaseAccuracy graph are: F6, F1 and F2.
# just for your reference
