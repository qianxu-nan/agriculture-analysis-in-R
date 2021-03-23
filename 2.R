word_agri<-read.csv("C:/Users/Qianxu/Desktop/world_dataset3 .csv",header=TRUE,stringsAsFactors =FALSE )
word_agri=word_agri[ ,1:10]
str(word_agri)
summary(word_agri)
y=word_agri$PRODHA
x=word_agri[c(-1,-8)]
shapiro.test(word_agri$PRODHA)
install.packages('RNOmni');
library(RNOmni);
y_var=rankNorm(word_agri$PRODHA);
shapiro.test(y_var)
x_var=as.data.frame(lapply(x,std))
summary(x_var)
word_agriculture<-cbind(y_var,x_var)
View(word_agriculture)
m<-lm(y_var ~.,data=word_agriculture)
step=step(m,direction="both") 

drop1(step)
summary(step)
n<-lm(y_var~ECONSZSC+LANDQSC+WWSC+WQSC+GASC,data=word_agriculture)
n
summary(n)

t<-lm(y_var~LANDQSC+WQSC,data=word_agriculture)
summary(t)
t


prodha_norm<-dnorm(y_var)
hist(y_var,prob=T,col="light blue")
xfit<-seq(min(y_var),max(y_var),length=40)
yfit<-dnorm(xfit,mean(y_var),sd(y_var))
lines(xfit,yfit,col="red",lwd=3)
plot(lm(y_var~LANDQSC+WQSC,data=word_agriculture),col='red')

