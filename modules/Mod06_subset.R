#################################################
#  Company    : Stevens 
#  Project    : R Bootcamp 
#  Purpose    : subset
#  First Name  : Khasha
#  Last Name  : Dehnad
#  Id			    : 12345
#  Date       :
#  Comments   :
rm(list=ls())

#################################################
##   Step:
##    [[]]
##     []
##
######################

avector<-c("A","B","C","D")
second<-avector[2]
names(avector)<-c("first","second","third","fourth" )
second<-avector["second"]
avector
avector[2]<-"E"
avector


my.lst <- list(34453, c("Khasha", "Dehand"), c(14.3,12,15,19))
my.lst[2:3]
is.list(my.lst[2])

element2<-my.lst[[2]] 
is.list(element2)
is.vector(element2)

typeof(element2)
is.list(element2)
is.vector(element2)
last_name<-my.lst[[2]][2] 

data()
data(iris)
View(iris)
iris[2,3]

iris[,3]
iris[3,]

trid_column<-iris[,-3] #all columns but 3

subset1<-iris[-c(5,1,20,23),-5] # no rows 5,1,20,23 and no colum 5, remove negative if you want that

subset2<-iris[1:20,] #to see the first 20 rows

subset3<-iris[c(T,F),c(2,3,4)]

indx<-2:20

subset2<-iris[-indx,-5]

subset1<-iris[c(1,2,5,8), ]
subset3<-iris[-c(1,2,5,8), ]
subset4<-iris[c(T,F),]

subset2<-iris[c(T,F),c(2,3,4)]


dsn <- read.csv("C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv")
View(dsn)

table(Class=dsn$Class,Survival=dsn$Survived)
ftable(Class=dsn$Class,Sex=dsn$Sex,Age=dsn$Age, Survived=dsn$Survived)
?table
Sex
dsn$Sex
attach(dsn)
Sex
Sex<-c("Yes","No")
rm("Sex")
detach(dsn)
## again error appears 
Sex

colcls=c("STATEFIPS"="integer"
         ,"STATE"="character"
         ,"zipcode"="integer"
         ,"agi_stub"="integer"
         , "N1"="double"
         ,"NUMDEP"="NULL"
)
data("iris")
View(iris)
?sample
nrow(iris)
idx<-sample(nrow(iris),as.integer(.65*nrow(iris)))

training<-iris[idx,]
test<-iris[-idx,]

idx<-seq(1,nrow(iris),5)

training<-iris[-idx,]
test<-iris[idx,]




income_zip<-
  read.csv("C://AIMS/Stevens_/SAS_Bootcamp_v2/Raw_data/income_by_zip_2013_subset.csv"
           ,header = TRUE
           ,colClasses=colcls,na.strings="?"
  )
is.factor(income_zip$STATE)
New_zip.frame<-
  data.frame(cbind(Zipchar=as.character(income_zip$zipcode),State_factor=as.factor(income_zip$STATE)))
is.data.frame(mydata.frame) 

new_agi_stub<-income_zip[is.na(income_zip$agi_stub)==FALSE,"agi_stub" ]

#<-cbind(,)
#<-rbind(pcs1,mcs1)
#typeof()
#is.data.frame()
#is.matrix()
#<-as.data.frame(cbind( ,)) 


indx<-seq(from=1, to=nrow(iris), by=5)
test<-iris[indx,]
training<-iris[-indx,]

data("iris")
View(iris)
?subset()
hist(iris$Sepal.Length)
summary(iris$Sepal.Length)
iris_subset<-subset(iris,Sepal.Length<6.4,select = -Species)
