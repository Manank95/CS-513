# clear environment
rm(list=ls())

# read data and EDA
dat<-read.csv('diamonds.csv')
summary(dat)
summary(dat[,3])
summary(dat[,4])
summary(dat[,5])

# change factor level
dat[,3]<-factor(dat[,3], levels=c('Fair', 'Good', 'Very Good', 'Premium', 'Ideal'))
dat[,4]<-factor(dat[,4], levels=c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
dat[,5]<-factor(dat[,5], levels=c('I1', 'SI1', 'SI2', 'VS1', 'VS2', 'VVS1', 'VVS2', 'IF'))

# represent price with price levels; adjust column order
dat[,12]<-round(dat[,8]/2000)+1
colnames(dat)[12]<-'pricelv'
dat<-dat[, c(2,3,4,5,6,7,9,10,11,8,12)]
pairs(dat[,c(1,10)])

# linear regression to test the relevance of each coulumn and price
summary(lm(dat[,10]~dat[,1])) # Multiple R-squared:  0.8493
summary(lm(dat[,10]~dat[,2])) # 0.01286
summary(lm(dat[,10]~dat[,3])) # 0.03128
summary(lm(dat[,10]~dat[,4])) # 0.02715
summary(lm(dat[,10]~dat[,5])) # 0.0001134
summary(lm(dat[,10]~dat[,6])) # 0.01616
summary(lm(dat[,10]~dat[,7])) # 0.7822
summary(lm(dat[,10]~dat[,8])) # 0.749
summary(lm(dat[,10]~dat[,9])) # 0.7418

# knn without normalization
dsn<-dat[,-10]
dsn[,2]<-as.numeric(dat[,2])
dsn[,3]<-as.numeric(dat[,3])
dsn[,4]<-as.numeric(dat[,4])

index<-seq(1, nrow(dsn), 5)
training<-dsn[-index,]
test<-dsn[index,]

library(class)

i<-1
err_rate<-NULL
while(i<=9){
  predict<-knn(training[,-10],test[,-10],training[,10],k=i)
  err_rate[i]<-sum(predict!=test[,10])/nrow(test)
  i<-i+1
}
View(err_rate) # 0.239 0.265 0.25 0.252 0.246 0.249 0.241 0.238 0.24
# values for 10~90: 0.243 0.247 0.254 0.256 0.256 0.262 0.266 0.268 0.275

# with normalized data
dsn_nor<-dsn
i<-1
while(i<=9){
  mx=max(dsn[,i])
  mn=min(dsn[,i])
  dsn_nor[,i]<-(dsn[,i]-mn)/(mx-mn)
  i<-i+1
}

training<-dsn_nor[-index,]
test<-dsn_nor[index,]
i<-1
err_rate<-NULL
while(i<=9){
  predict<-knn(training[,-10],test[,-10],training[,10],k=i)
  err_rate[i]<-sum(predict!=test[,10])/nrow(test)
  i<-i+1
}
View(err_rate) #0.198 0.209 0.191 0.192 0.19 0.193 0.19 0.194 0.195

# add weighted
dsn_wei<-dsn_nor
dsn_wei[,1]<-dsn_nor[,1]*5
dsn_wei[,7:9]<-dsn_nor[,7:9]*3
training<-dsn_wei[-index,]
test<-dsn_wei[index,]
i<-1
err_rate<-NULL
while(i<=9){
  predict<-knn(training[,-10],test[,-10],training[,10],k=i)
  err_rate[i]<-sum(predict!=test[,10])/nrow(test)
  i<-i+1
}
View(err_rate) #0.178 0.185 0.167 0.17 0.161 0.164 0.163 0.162 0.162


# C50 method
library('C50')
# C50's arguments must be factors
dsn_factor<-dsn[,1:9]
dsn_factor[,1]<-as.factor(dat[,1])
dsn_factor[,2:4]<-dat[,2:4]
dsn_factor[,5]<-as.factor(dat[,5])
dsn_factor[,6]<-as.factor(dat[,6])
dsn_factor[,7]<-as.factor(dat[,7])
dsn_factor[,8]<-as.factor(dat[,8])
dsn_factor[,9]<-as.factor(dat[,9])
# the variable must also be a factor
price_lv<-factor(dsn[,10])
dsn_factor<-cbind(dsn_factor, price_lv)

training<-dsn_factor[-index,]
test<-dsn_factor[index,]

C50_class<-C5.0(price_lv~., data=training)
summary(C50_class)
str(C50_class)

C50_predict<-predict(C50_class, test)
table(actual=test[,10], C50=C50_predict)
c50_rate<-sum(test[,10]!=C50_predict)/length(test[,10])
c50_rate

# Naive Bayes
library(class) 
library(e1071)

# according to my test, dataset must use factor
# using all attributes
nBayes_all<-naiveBayes(price_lv ~ . , data=dsn_factor)
category_all<-predict(nBayes_all, dsn_factor)
table(NBayes_all=category_all, real_pricelevel=dsn_factor$price_lv)
NB_error_rate<-sum(category_all!=dsn_factor$price_lv)/length(category_all)
NB_error_rate #0.310

# using carat only
nBayes_carat<-naiveBayes(price_lv ~ carat, data=dsn_factor)
category_carat<-predict(nBayes_carat, dsn_factor)
table(NBayes_carat=category_carat, real_pricelevel=dsn_factor$price_lv)
NB_error_rate<-sum(category_carat!=dsn_factor$price_lv)/length(category_carat)
NB_error_rate #0.331

# using carat+x+y+z
nBayes_mix<-naiveBayes(price_lv ~ carat+x+y+z, data=dsn_factor)
category_mix<-predict(nBayes_mix, dsn_factor)
table(NBayes_mix=category_mix, real_pricelevel=dsn_factor$price_lv)
NB_error_rate<-sum(category_mix!=dsn_factor$price_lv)/length(category_mix)
NB_error_rate #0.321


# svm 
library('e1071')
# using all attributes using price
svm_model_all <- svm(price_lv ~ ., data=dsn_factor, type="C-classification", kernel="linear")
svm_model_all

svm_predict <- predict( svm_model_all ,dsn_factor , type="class" )
svm_table <- table(actual=svm_predict, real_pricelevel=dsn_factor$price_lv)

svm_predict
svm_table


#error rate
wrong <- (svm_predict!=dsn_factor$price_lv)
svm_rate<-sum(wrong)/length(svm_predict)
svm_rate #0.13


#svm using carat
svm_model_carat <- svm(price_lv ~ carat, data=dsn_factor, type="C-classification", kernel="linear")
svm_model_carat

svm_predict_carat <- predict( svm_model_carat ,dsn_factor , type="class" )
svm_table_carat <- table(actual=svm_predict_carat, real_pricelevel=dsn_factor$price_lv)

#error rate
wrong_carat <- (svm_predict_carat!=dsn_factor$price_lv)
svm_rate_carat<-sum(wrong_carat)/length(svm_predict_carat)
svm_rate_carat #0.13


#svm using carat+x+y+z
svm_model_mix <- svm(price_lv ~ carat+x+y+z, data=dsn_factor, type="C-classification", kernel="linear")
svm_model_mix

summary(svm_model_mix)

svm_predict_mix <- predict( svm_model_mix ,dsn_factor , type="class" )
svm_table_mix <- table(actual=svm_predict_mix, real_pricelevel=dsn_factor$price_lv)


#error rate
wrong_mix <- (svm_predict_mix!=dsn_factor$price_lv)
svm_rate_mix <- sum(wrong_mix)/length(svm_predict_mix)
svm_rate_mix


# random forest
library(randomForest)
# crushes if we use all the input
dsn_short<-dsn[seq(1,nrow(dsn),5),]
index_short<-seq(1,nrow(dsn_short),5)
training<-dsn_short[-index_short,]
test<-dsn_short[index_short,]

rf_all<-randomForest(pricelv~., data=training, importance=TRUE, ntree=1000)
rf_all
importance(rf_all)
predict<-predict(rf_all, test)
err_rate<-sum(test[,10]!=round(predict))/nrow(test)
err_rate # 0.182

# ANN
library("neuralnet")
# prepare destination varible
dsn_short<-dsn[seq(1,nrow(dsn),20),]

lv1<-ifelse(dsn_short$pricelv==1,1,0)
lv2<-ifelse(dsn_short$pricelv==2,1,0)
lv3<-ifelse(dsn_short$pricelv==3,1,0)
lv4<-ifelse(dsn_short$pricelv==4,1,0)
lv5<-ifelse(dsn_short$pricelv==5,1,0)
lv6<-ifelse(dsn_short$pricelv==6,1,0)
lv7<-ifelse(dsn_short$pricelv==7,1,0)
lv8<-ifelse(dsn_short$pricelv==8,1,0)
lv9<-ifelse(dsn_short$pricelv==9,1,0)
lv10<-ifelse(dsn_short$pricelv==10,1,0)
dsn_lvs<-data.frame(dsn_short,lv1,lv2,lv3,lv4,lv5,lv6,lv7,lv8,lv9,lv10)

index_short<-seq(1,nrow(dsn_short),5)
training<-dsn_lvs[-index_short,]
test<-dsn_lvs[index_short,]

# 10 hidden nodes
ann_res<-neuralnet(lv1+lv2+lv3+lv4+lv5+lv6+lv7+lv8+lv9+lv10~carat,
                   training, hidden=10, threshold=0.1)
# algorithm did not converge in 1 of 1 repetition(s) within the stepmax:
# ann_res<-neuralnet(lv1+lv2+lv3+lv4+lv5+lv6+lv7+lv8+lv9+lv10~carat+cut+color+clarity+depth+table+x+y+z,
#                   training, hidden=10, threshold=0.01)
# ann_res<-neuralnet(lv1+lv2+lv3+lv4+lv5+lv6+lv7+lv8+lv9+lv10~carat+x+y+z,
#                   training, hidden=10, threshold=0.005)
# ann_res<-neuralnet(lv1+lv2+lv3+lv4+lv5+lv6+lv7+lv8+lv9+lv10~carat+x+y+z,
#                   training, hidden=10, threshold=0.1)

# the result above did not converge
plot(ann_res)

# show prediction result
ann_compute<-compute(ann_res, test[,1])

i<-1
err_rate<-NULL
while(i<=9){
  err_rate[i]<-sum(round(ann_compute$net.result[,i])!=test[,10+i])/nrow(test)
  i<-i+1
}
summary(err_rate) # mean error_rate: 0.065

# average linkage method. cannot work. more than 10GB
# hres<-hclust(dist(dsn), method = 'average')
# plot(hres)


# K-means clustering method
km<-kmeans(dsn, centers = 10);
str(km)

