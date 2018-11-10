#################################################
#  Company    : Stevens 
#  Project    : R Bootcamp 
#  Purpose    : Function
#  First Name  : Khasha
#  Last Name  : Dehnad
#  Id			    : 12345
#  Date       :
#  Comments   : only for your reference

rm(list=ls())
install.packages("knn")
?knn()
?table
?sample()
library(class)
range_1_100<-1:100
sample(range_1_100,80)
smpl<-sort(sample(range_1_100,80))
idx<-sort(sample(nrow(iris),as.integer(.65*nrow(iris))))
training<-iris[idx,]
test<-iris[-idx,]
test
predict<-knn(training[,-5],test[-5],training[,5],k=3)
table(prediction=predict,Actual=test[,5])
#################################################
##   Step:
## 
######################
## Define a funcition!!!


mmnorm <-function(x,minx,maxx)
{z<-((x-minx)/(maxx-minx))
 return(z)                              
}

mmnorm2 <-function(x)
{z<-((x-min(x))/(max(x)-min(x)))
return(z)                              
}



myvector<-1:20
mmnorm2(myvector)

myvector<-1:20
mmnorm(myvector,min(myvector),max(myvector))


maxx<-20
minx<-1
mmnorm4 <-function(x)
{
  minx<-10
  z<-((x-minx)/(maxx-minx))
  y<-list(z,minx,maxx)
 return(y)                              
}
results<-mmnorm4(myvector)


length(results)
results[1]
results[2]
results[3]
minx
maxx

mmnorm3 <-function(x)
{z<-((x-min(x))/(max(x)-min(x))) 
return(z)                              
}
mmnorm3(myvector)



f_print <-function(x1,x2,x3,x4,x5)
{ 
 print(c('value of x1',x1))
 print(c('value of x2',x2))
  print(c('value of x3',x3)) 
  print(c('value of x4',x4)) 
  print(c('value of x5',x5)) 
}
f_print(10,5,3,2,1)

f_print(x2=10,x5=5,3,2,1)
 