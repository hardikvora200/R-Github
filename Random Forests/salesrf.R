Company <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Random Forests\\Company_Data.csv")
View(Company)
attach(Company)
sum(is.na(Company))
Company <- na.omit(Company) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Company)### 600 6
colnames(Company)
-------------------------------------------------
#Measures of Central Tendency                Sales
mean(Company$Sales)  ### -7.496325
median(Company$Sales)##### 7.49
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Company$Sales)  ###### 7.8
#Measures of Dispersion
var(Company$Sales)  ###### 7.975626
sd(Company$Sales)  #### 2.824115
range(Company$Sales)##### 0.00  16.27
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Sales) ##### 16.27
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Company$Sales)  #### 0.1848638
#Measures of Kurtosis 
kurtosis(Company$Sales)######  2.905167
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Company$Sales,horizontal = TRUE)
hist(Company$Sales)
barplot(Company$Sales)
str(Company)
#qqplot
qqnorm(Company$Sales)
qqline(Company$Sales)####Normalisation achieved
install.packages(psych)
library(psych)
describe(Company)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(Company$Sales,-7.496325,2.824115)
-----------------------------------------------------------------------
#Measures of Central Tendency                CompPrice
mean(Company$CompPrice)  ### -124.975
median(Company$CompPrice)##### 125
#mode
getmode(Company$CompPrice)  #####  121
#Measures of Dispersion
var(Company$CompPrice)  ###### 235.1472
sd(Company$CompPrice)  ####  15.33451
range(Company$CompPrice) ##### 77  175
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$CompPrice) ##### 98
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$CompPrice)  #### -0.04259408
#Measures of Kurtosis 
kurtosis(Company$CompPrice)###### 3.026185
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$CompPrice,horizontal = TRUE)
hist(Company$CompPrice)
barplot(Company$CompPrice)
#qqplot
qqnorm(Company$CompPrice)
qqline(Company$CompPrice)###Normalisation achieved
pnorm(Company$CompPrice,-124.975,15.33451)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Income
mean(Company$Income)  ### -68.6575
median(Company$Income)##### 69
#mode
getmode(Company$Income)  ##### 69
#Measures of Dispersion
var(Company$Income)  ###### 783.2182
sd(Company$Income)  #### 27.98604
range(Company$Income) ##### 21 120
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Income) ##### 99
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Income)  #### 0.04925888
#Measures of Kurtosis 
kurtosis(Company$Income)###### 1.913267
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Income,horizontal = TRUE)
hist(Company$Income)
barplot(Company$Income)
#qqplot
qqnorm(Company$Income)
qqline(Company$Income)###Normalisation achieved
login<-log(Company$Income)
qqnorm(login)
qqline(login)
expin<-exp(login)
qqnorm(expin)
qqline(expin)
sqrtin<-sqrt(login)
qqnorm(sqrtin)
qqline(sqrtin)
recin<-(1/(Company$Income))
qqnorm(recin)
qqline(recin)
rec2in<-(1/(Company$Income)) * (1/(Company$Income))
qqnorm(rec2in)
qqline(rec2in)
rec4in<-(1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income))
qqnorm(rec4in)
qqline(rec4in)
rec8in<-(1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income)) * (1/(Company$Income))
qqnorm(rec8in)
qqline(rec8in)  ####Normalisation achieved
pnorm(Company$Income,-68.6575,27.98604)
------------------------------------------------------------------
#Measures of Central Tendency                Advertising
mean(Company$Advertising)  ### 6.635
median(Company$Advertising)##### 5
#mode
getmode(Company$Advertising)  ##### 0
#Measures of Dispersion
var(Company$Advertising)  ###### 44.22734
sd(Company$Advertising)  #### 6.650364
range(Company$Advertising) ##### 0  29
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Advertising) ##### 29
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Advertising)  #### 0.6371848
#Measures of Kurtosis 
kurtosis(Company$Advertising)###### 2.44671
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Advertising,horizontal = TRUE)
hist(Company$Advertising)
barplot(Company$Advertising)
#qqplot
qqnorm(Company$Advertising)
qqline(Company$Advertising)###Normalisation achieved
expad<-exp(Company$Advertising)
qqnorm(expad)
qqline(expad)   ######Normalisation achieved
pnorm(Company$Advertising,6.635,6.650364)
-------------------------------------------------------------------
#Measures of Central Tendency                Population
mean(Company$Population)  ### -264.84
median(Company$Population)##### 272
#mode
getmode(Company$Population)  ##### 276
#Measures of Dispersion
var(Company$Population)  ###### 21719.81
sd(Company$Population)  #### 147.3764
range(Company$Population) ##### 10  509
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Population) ##### 499
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Population)  #### -0.05103434
#Measures of Kurtosis 
kurtosis(Company$Population)###### 1.797696
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Population,horizontal = TRUE)
hist(Company$Population)
barplot(Company$Population)
#qqplot
qqnorm(Company$Population)
qqline(Company$Population)
expop<-exp(Company$Population)
qqnorm(expop)
qqline(expop)   ######Normalisation achieved
pnorm(Company$Population,-264.84,147.3764)
-------------------------------------------------------------
#Measures of Central Tendency                Price
mean(Company$Price)  ### -115.795
median(Company$Price)##### 117
#mode
getmode(Company$Price)  ##### 120
#Measures of Dispersion
var(Company$Price)  ###### 560.5844
sd(Company$Price)  #### 23.67666
range(Company$Price) ##### 24 191
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Price) ##### 167
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Price)  #### -0.1248159
#Measures of Kurtosis 
kurtosis(Company$Price)###### 3.431294
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Price,horizontal = TRUE)
hist(Company$Price)
barplot(Company$Price)
#qqplot
qqnorm(Company$Price)
qqline(Company$Price)###Normalisation achieved
pnorm(Company$Price,-115.795,23.67666)
--------------------------------------------------------------
#Measures of Central Tendency                Age
mean(Company$Age)  ### 53.3225
median(Company$Age)##### 54.5
#mode
getmode(Company$Age)  ##### 62
#Measures of Dispersion
var(Company$Age)  ###### 262.4496
sd(Company$Age)  #### 16.2003
range(Company$Age) ##### 25 80
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Age) ##### 55
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Age)  #### -0.076892
#Measures of Kurtosis 
kurtosis(Company$Age)###### 1.864776
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Age,horizontal = TRUE)
hist(Company$Age)
barplot(Company$Age)
#qqplot
qqnorm(Company$Age)
qqline(Company$Age)
logage<-log(Company$Age)
qqnorm(logage)
expage<-exp(Company$Age)
qqnorm(expage)
qqline(expage)  ###Normalisation achieved
pnorm(Company$Age,53.3225,16.2003)
-------------------------------------------------------------------
#Measures of Central Tendency                Education
mean(Company$Education)  ### -13.9
median(Company$Education)##### 14
#mode
getmode(Company$Education)  ##### 17
#Measures of Dispersion
var(Company$Education)  ###### 6.867168
sd(Company$Education)  #### 2.620528
range(Company$Education) ##### 10 18
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Company$Education) ##### 8
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Company$Education)  #### 0.04384163
#Measures of Kurtosis 
kurtosis(Company$Education)###### 1.702878
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Company$Education,horizontal = TRUE)
hist(Company$Education)
barplot(Company$Education)
#qqplot
qqnorm(Company$Education)
qqline(Company$Education)
logedu<-log(Company$Education)
qqnorm(logedu)
expedu<-exp(Company$Education)
qqnorm(expedu)
qqline(expedu) 
sqrtedu<-sqrt(Company$Education)
qqnorm(sqrtedu) 
qqline(sqrtedu)
recedu<-(1/(Company$Education))                  
qqnorm(recedu)
rec2edu<-(1/(Company$Education)) * (1/(Company$Education)) 
qqnorm(rec2edu)  
rec4edu<-(1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) 
qqnorm(rec4edu)
rec8edu<-(1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) * (1/(Company$Education)) 
qqnorm(rec8edu)
qqline(rec8edu)
rec16edu<-rec8edu*rec8edu
qqnorm(rec16edu)
qqline(rec16edu)
rec32edu<-rec16edu*rec16edu
qqnorm(rec32edu)
qqline(rec32edu)  ## Normalisation achieved
pnorm(Company$Education,-13.9,2.620528)
-----------------------------------------------------------------
####Normalisation of data
normalized_data<-scale(Company[,c(1:6,8:9)]) 
View(normalized_data)
------------------------------------------------------------------
# Using Random Forest
install.packages("randomForest")
library(randomForest)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
Company_less<-Company[Company$Sales<=5,]   ###77
View(Company_less)
Company_more<-Company[Company$Sales>5,]   ### 323
View(Company_more)
Company_train <- rbind(Company_less[1:38,],Company_more[1:162,])
View(Company_train)
Company_test <- rbind(Company_less[39:77,],Company_more[163:323,])
View(Company_test)
# Building a random forest model on training data 
Company_train$Sales<-as.factor(Company_train$Sales)
str(Company_train$Sales)
fit.forest <- randomForest(Sales ~ . ,data=Company_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean((Company_train$Sales) == predict(fit.forest,Company_train)) # 100% accuracy 
# Prediction of train data
pred_train <- predict(fit.forest,Company_train)
pred_train
library(caret)
# Confusion Matrix
confusionMatrix(Company_train$Sales, pred_train)
# Predicting test data and test accuracy
Company_test$Sales<-as.factor(Company_test$Sales)
str(Company_test$Sales)
fit.forest <- randomForest(Sales ~ . ,data=Company_test, na.action=na.roughfix,importance=TRUE)
pred_test <- predict(fit.forest,data=Company_test)
mean((Company_test$Sales) == predict(fit.forest,Company_test)) # Accuracy = 100% 
# Confusion Matrix 
confusionMatrix(Company_test$Sales, pred_test)
# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
fit.forest <- randomForest(Sales ~ . ,data=Company, na.action=na.roughfix,importance=TRUE)
Company$Sales<-as.factor(Company$Sales)
str(Company$Sales)
acc_wbcd <- mean(Company$Sales==predict(fit.forest,Company))
acc_wbcd   ### 100% accuracy
varImpPlot(fit.forest)
##########Using Bagging
library(rpart)
library(adabag)
library(mlbench)
###l <- length(Fraud[,1])
##sub <- sample(1:l,2*l/3)
if (bag == TRUE) { 
  k <- 1
  while (k == 1){
    boostrap <- Company_train(1:n, replace = TRUE, prob = pesos)
    fit <- rpart(Sales ~ ., data = Company_train[boostrap, -1],
                 control = control)
    k <- length(fit$frame$var)
  }
  flearn <- predict(fit, newdata = Company_test, type = "class")
  ind <- as.numeric(vardep != flearn)
  err <- sum(pesos * ind)
}
Company.bagging <- bagging(Sales ~ .,data=Company_train,bag = TRUE,mfinal=5,
                           control=rpart.control(maxdepth=5, minsplit=15,CP=0))
#Using the pruning option
Company.bagging.pred <- predict.bagging(Company.bagging,newdata=Company_test, newmfinal=3)
Company.bagging.pred$confusion
Company.bagging.pred$error
#######################Using Boosting
Company.rpart <- rpart(Sales~.,data=Company_train,maxdepth=5)
Company.rpart.pred <- predict(Company.rpart,newdata=Company_test,type="class")
tb <- table(Company.rpart.pred,Company_test[,11])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart
Company.adaboost <- boosting(Sales ~.,data=Company_train[1:197,],mfinal=3, coeflearn="Zhu",
                             control=rpart.control(maxdepth=5))
Company.adaboost.pred <- predict.boosting(Company.adaboost,newdata=Company_test[1:203,])
Company.adaboost.pred$confusion
Company.adaboost.pred$error
#comparing error evolution in training and test set
errorevol(Company.adaboost,newdata=Company_train[1:197, ])->evol.train
View(evol.test)
errorevol(Company.adaboost,newdata=Company_test[1:203, ])->evol.test
plot.errorevol(evol.test,evol.train)

