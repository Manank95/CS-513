## Manank Valand
## 10429101
## Assignment 7

## reference from lecture code provided for Artificial Neural Network on canvas.

rm(list=ls())

#install.packages("neuralnet")
library("neuralnet")

dsn<-read.csv('C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv', na.strings = '?')

# removing all NA values
benign<-ifelse(dsn$Class==2,1,0)
malignant<-ifelse(dsn$Class==4,1,0)
dsn1<-na.omit(data.frame(dsn,benign,malignant))

# dividing into test and training datasets
index<-seq (1,nrow(dsn1),by=5)
training<-dsn1[-index,]
test<-dsn1[index,]


# neural network model, with 10 hidden nodes
net_dsn1<-neuralnet(benign+malignant~F1+F2+F3+F4+F5+F6+F7+F8+F9, training, hidden=10, threshold=0.01)

# Plotting
plot(net_dsn1)

net_dsn1_results<-compute(net_dsn1, test[,c(-1,-11,-12,-13)]) 
resutls<-data.frame(Actual_Benign=test$benign,
                    Actual_Malignant=test$malignant,
                    ANN_Benign=round(net_dsn1_results$net.result[,1]),
                    ANN_Malignant=round(net_dsn1_results$net.result[,2]),
                    Prediction=ifelse(round(net_dsn1_results$net.result[,1])==1,'B','M'))

table(Actual=resutls$Actual_Malignant,Prediction=resutls$Prediction)

wrong<-(round(net_dsn1_results$net.result[,1])!=test$benign )
error<-sum(wrong)/length(wrong)
error

# Only for your reference, not for direct copy