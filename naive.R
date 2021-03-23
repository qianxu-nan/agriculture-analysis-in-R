sms_raw<-read.csv("C:/Users/Qianxu/Desktop/sms_spam.csv",header=TRUE, stringsAsFactors= FALSE)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
library("tm")
sms_corpus <-Corpus(VectorSource(sms_raw$text)) 
class(sms_corpus)
inspect(sms_corpus[1:5]) 
corpus_clean<-tm_map(sms_corpus,tolower)
inspect(corpus_clean[1:5])
corpus_clean<-tm_map(corpus_clean,removeNumbers) 
inspect(corpus_clean[1:5])
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords()) 
inspect(corpus_clean[1:5])
corpus_clean<-tm_map(corpus_clean,removePunctuation)
inspect(corpus_clean[1:5])
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
inspect(corpus_clean[1:5])
sms_dtm <- DocumentTermMatrix(corpus_clean) 
sms_raw_train<-sms_raw[1:4180, ] 
sms_raw_test<-sms_raw[4181:5574, ]
sms_corpus_train<-corpus_clean[1:4180] 
sms_corpus_test<-corpus_clean[4181:5574]
sms_dtm_train<-sms_dtm[1:4180, ]
sms_dtm_test<-sms_dtm[4181:5574, ] 
prop.table(table(sms_raw$type)) 
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
install.packages("wordcloud") 
library(wordcloud) 
wordcloud(sms_corpus_train, min.freq=40, random.order= FALSE) 
spam<-subset(sms_raw_train, type=="spam") 
ham<-subset(sms_raw_train, type=="ham") 
spam[1:5]
wordcloud(ham$text, max.words=40, scale=c(3,0.5)) 
wordcloud(spam$text, max.words=40, scale=c(3,0.5)) 
Dict<-findFreqTerms(sms_dtm_train, 5) 
sms_train<-DocumentTermMatrix(sms_corpus_train, list(dictionary=Dict)) 
sms_train
sms_test <-DocumentTermMatrix(sms_corpus_test, list(dictionary=Dict)) 
convert_counts <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x, levels=c(0,1),labels=c("No","Yes"))
  return(x)
} 
convert_counts(6) 

sms_train1<-apply(sms_train, MARGIN=2,convert_counts)
sms_test1<-apply(sms_test, MARGIN=2,convert_counts) 
library(e1071) 
sms_classifier <-naiveBayes(sms_train1, sms_raw_train$type) 
sms_test_predict<-predict(sms_classifier, sms_test1) 
library(gmodels)
CrossTable(sms_test_predict,sms_raw_test$type,prop.chisq = FALSE, prop.t = FALSE,prop.c = FALSE,prop.r = TRUE, dnn=c('predicted','actual')) 
