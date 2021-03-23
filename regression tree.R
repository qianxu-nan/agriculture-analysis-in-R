tee<-c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)
at1<-c(1,1,1,2,2,3,4,5,5)
at2<-c(6,6,7,7,7,7)
bt1<-c(1,1,1,2,2,3,4)
bt2<-c(5,5,6,6,7,7,7,7)
sdr_a<-sd(tee)-(length(at1)/length(tee))*sd(at1)+(length(at2)/length(tee))*sd(at2)
sdr_b<-sd(tee)-(length(bt1)/length(tee))*sd(bt1)+(length(bt2)/length(tee))*sd(bt2)
train<-read.csv("C:/Users/Qianxu/Desktop/train.csv",header=TRUE,stringsAsFactors = TRUE)
summary(train$Sex)
summary(as.factor(train$Survived))
prop.table(table(train$Sex,train$Survived))
prop.table(table(train$Sex,train$Survived),1)
prop.table(table(train$Sex,train$Survived),2)
Agemean<-round(mean(train$Age,na.rm = TRUE))
train$Age[is.na(train$Age)]=Agemean
train$Age<-ifelse(is.na(train$Age),30,train$Age)
train$Child<-0
train$Child[train$Age<18]<-1
train$Child<-as.factor(train$Child)
summary(train$Child)
aggregate(Survived~Child+Sex,data=train,FUN=sum)
class(train$Sex)
class(train$Child)
aggregate(Survived~Child+Sex,data=train,FUN=length)
aggregate(Survived~Child+Sex,data=train,FUN=function(x){sum(x)/length(x)})
train$Fare2<-'30+'
train$Fare2[train$Fare<30&train$Fare>=20]<-'20-30'
train$Fare2[train$Fare<20&train$Fare>=10]<-'10-20'
train$Fare2[train$Fare<10]<- '10'
aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN=function(x){sum(x)/length(x)})
library(rpart)
test<-read.csv("C:/Users/Qianxu/Desktop/test.csv",header=TRUE,stringsAsFactors = TRUE)
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method='class')
plot(fit)
text(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction<-predict(fit,test,type='class')
test$Prediction<-predict(fit,test,type='class')
?rpart
?rpart.control
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method='class',control=rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit)
#test$Survived<-0
#test$Sur