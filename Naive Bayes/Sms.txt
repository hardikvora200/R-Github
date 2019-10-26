####Exploring and preparing the data
#####Read the sms raw data into the sms data frame
sms_raw<-read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Naive Bayes\\sms_raw_NB.csv")
View(sms_raw)
###Examine the structure of sms data
str(sms_raw)
###Convert spam/ham to factor
sms_raw$type<-factor(sms_raw$type)
###Examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)  #### ham-4812, spam-747
###Build a corpus using the text mining(tm) package
library(tm)
library(tmap)
sms_corpus<-Corpus(VectorSource(sms_raw$text))
####Examine the sms_corpus
print(sms_corpus)
inspect(sms_corpus[1:3])
###Clean up the corpus using tm_map()
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords(kind="en"))
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
####Examine the clean corpus
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])
####Create a document term sparse matrix
sms_dtm<-DocumentTermMatrix(corpus_clean)
sms_dtm
View(sms_dtm)
#######Creating training and test datasets
sms_raw_train<-sms_raw[1:4169,]
sms_raw_test<-sms_raw[4170:5559,]
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]
####Check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
####Word cloud visualisation
library(wordcloud)
wordcloud(sms_corpus_train,min.freq=30,random.order=FALSE)
####Subset the training data into spam and ham groups
spam<-subset(sms_raw_train,type="spam")
ham<-subset(sms_raw_train,type="ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale=c(3,0.5))
####Indicator feature for frequent words
sms_dict<-findFreqTerms(sms_dtm_train,3)
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
####Convert Counts to a factor
Convert_counts<-function(x)
{
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
}
####apply() convert_counts() to columns of train network
sms_train<-apply(sms_train,MARGIN = 2, Convert_counts)
sms_test<-apply(sms_test,MARGIN = 2, Convert_counts)
#####Train a model on the data
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)
sms_classifier
#####Evaluating model performance
sms_test_pred<-predict(sms_classifier,sms_test)
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','Actual'))
?CrossTable()
###########Improving model performance
sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type, laplace=1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','Actual'))

##########Using Bagging
library(naivebayes)
library(adabag)
library(mlbench)
if (bag == TRUE) { 
  k <- 1
  while (k == 1){
    boostrap <- sms_train(1:n, replace = TRUE, prob = pesos)
    fit <- naiveBayes(type ~ ., data = sms_train[boostrap, -1],
                      control = control)
    k <- length(fit$frame$var)
  }
  flearn <- predict(fit, newdata = sms_test, type = "class")
  ind <- as.numeric(vardep != flearn)
  err <- sum(pesos * ind)
}
sms_train<-as.data.frame(sms_train)
sms_test<-as.data.frame(sms_test)
sms.bagging <- bagging(type ~.,data=sms_train,bag = TRUE,mfinal=5)
#Using the pruning option
sms.bagging.pred <- predict.bagging(sms.bagging,newdata=sms_test, newmfinal=3)
sms.bagging.pred$confusion
sms.bagging.pred$error ##0.00647482
#######################Using Boosting
library(e1071)
sms.naivebayes <- naiveBayes(type~.,data=sms_train,maxdepth=5)
sms.naivebayes.pred <- predict(sms.naivebayes,newdata=sms_test,type="class")
tb <- table(sms.naivebayes.pred,sms_test[,1])
error.naivebayes <- 1-(sum(diag(tb))/sum(tb))
tb
error.naivebayes  ###  0.009352518
class(boosting)
sms.adaboost <- boosting(type ~.,data=sms_train[1:4169,],mfinal=3, coeflearn="Zhu")
?boosting()
sms.adaboost.pred <- predict.boosting(sms.adaboost,newdata=sms_test[1:1390,])
sms.adaboost.pred$confusion
sms.adaboost.pred$error   ##0.005035971
#comparing error evolution in training and test set
errorevol(sms.adaboost,newdata=sms_train[1:4169, ])->evol.train
errorevol(sms.adaboost,newdata=sms_test[1:1390, ])->evol.test
plot.errorevol(evol.test,evol.train)

