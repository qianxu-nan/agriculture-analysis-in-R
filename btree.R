creit<-read.csv("C:/Users/Qianxu/Desktop/credit.csv",header=TRUE,stringsAsFactors = TRUE)
set.seed(12345)
creid_rand<-creit[order(runif(1000)), ]
creit_train=creid_rand[1:900, ]
creit_test=creid_rand[901:1000, ]
head(creit_train)
head(creit_test)
prop.table(table(creit_train$default))
prop.table(table(creit_test$default))
library(C50)
train<-creit_train[-17]
class<-as.factor(creit_train$default)
creit_model<-C5.0(train,class)
test<-creit_test[-17]
M<-C5.0(train, class, trials=1, costs=NULL)
creit_boost10<-C5.0(train,class,trials=10)
summary(creit_boost10)
p <-predict(creit_model, test)
p1 <-predict(creit_boost10, test)
summary(p1)
summary(p)
library(gmodels)
CrossTable(creit_test$default,p,
           prop.chisq= FALSE, prop.c= FALSE, prop.r= FALSE,
           dnn=c('actual default', 'predicted default'))
CrossTable(creit_test$default,p1,
           prop.chisq= FALSE, prop.c= FALSE, prop.r= FALSE,
           dnn=c('actual default', 'predicted default'))
error_cost<-matrix(c(0,1,4,0),nrow=2)
creit_cost <-C5.0 (train,class, costs= error_cost)
p2 <-predict(creit_cost, test)
CrossTable(creit_test$default,p2,
           prop.chisq= FALSE, prop.c= FALSE, prop.r= FALSE,
           dnn=c('actual default', 'predicted default'))
