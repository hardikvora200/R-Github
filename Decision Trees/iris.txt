install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
data()
data("iris")
View(iris)
sum(is.na(iris))
iris <- na.omit(iris) 
dim(iris)
colnames(iris)
-------------------------------------------------
#Measures of Central Tendency                Sepal.Length
mean(iris$Sepal.Length)  ### -5.843333
median(iris$Sepal.Length)##### 5.8
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(iris$Sepal.Length)  ###### 5
#Measures of Dispersion
var(iris$Sepal.Length)  ###### 0.6856935
sd(iris$Sepal.Length)  #### 0.8280661
range(iris$Sepal.Length)##### 4.3 7.9
rangevalue <- function(x){max(x)-min(x)}
rangevalue(iris$Sepal.Length) ##### 3.6
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(iris$Sepal.Length)  #### 0.3117531
#Measures of Kurtosis 
kurtosis(iris$Sepal.Length)######  2.426432
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(iris$Sepal.Length,horizontal = TRUE)
hist(iris$Sepal.Length)
barplot(iris$Sepal.Length)
str(iris)
#qqplot
qqnorm(iris$Sepal.Length)
qqline(iris$Sepal.Length)####Normalisation achieved
install.packages(psych)
library(psych)
describe(iris)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(iris$Sepal.Length,-5.843333,0.8280661)
-----------------------------------------------------------------------
#Measures of Central Tendency                Sepal.Width
mean(iris$Sepal.Width)  ### -3.057333
median(iris$Sepal.Width)##### 3
#mode
getmode(iris$Sepal.Width)  #####  3
#Measures of Dispersion
var(iris$Sepal.Width)  ###### 0.1899794
sd(iris$Sepal.Width)  ####  0.4358663
range(iris$Sepal.Width) ##### 2.0  4.4
rangevalue <- function(x){max(x)-min(x)}
rangevalue(iris$Sepal.Width) ##### 2.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(iris$Sepal.Width)  #### 0.3157671
#Measures of Kurtosis 
kurtosis(iris$Sepal.Width)  ###### 3.180976
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(iris$Sepal.Width,horizontal = TRUE)
hist(iris$Sepal.Width)
barplot(iris$Sepal.Width)
#qqplot
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width)
logsw<-log(iris$Sepal.Width)
qqnorm(logsw)
qqline(logsw)
expsw<-exp(iris$Sepal.Width)
qqnorm(expsw)
qqline(expsw)
sqrtsw<-sqrt(iris$Sepal.Width)
qqnorm(sqrtsw)
qqline(sqrtsw)
algsw<-(iris$Sepal.Width * iris$Sepal.Width) + iris$Sepal.Width
qqnorm(algsw)
qqline(algsw)###Normalisation achieved
pnorm(iris$Sepal.Width,-3.057333,0.4358663)
--------------------------------------------------------
#Measures of Central Tendency                Petal.Length
mean(iris$Petal.Length)  ### 3.758
median(iris$Petal.Length)  ##### 4.35
#mode
getmode(iris$Petal.Length)  #####  1.4
#Measures of Dispersion
var(iris$Petal.Length)  ###### 3.116278
sd(iris$Petal.Length)  ####  1.765298
range(iris$Petal.Length) ##### 1.0  6.9
rangevalue <- function(x){max(x)-min(x)}
rangevalue(iris$Petal.Length) ##### 5.9
#Measures of skewness
library(moments)
#Measures of skewness
skewness(iris$Petal.Length)  #### -0.2721277
#Measures of Kurtosis 
kurtosis(iris$Petal.Length)  ###### 1.604464
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(iris$Petal.Length,horizontal = TRUE)
hist(iris$Petal.Length)
barplot(iris$Petal.Length)
#qqplot
qqnorm(iris$Petal.Length)
qqline(iris$Petal.Length)
logpl<-log(iris$Petal.Length)
qqnorm(logpl)
qqline(logpl)
exppl<-exp(iris$Petal.Length)
qqnorm(exppl)
qqline(exppl)
recexpl<-(1/exp(iris$Petal.Length))
qqnorm(recexpl)
qqline(recexpl)
sqrtpl<-sqrt(iris$Petal.Length)
qqnorm(sqrtpl)
qqline(sqrtsw)
algpl<-(iris$Petal.Length * iris$Petal.Length) + iris$Petal.Length
qqnorm(algpl)
qqline(algpl)
recpl<-(1/iris$Petal.Length)###Normalisation achieved
qqnorm(recpl)
rec2pl<-(1/iris$Petal.Length) * (1/iris$Petal.Length)
qqnorm(rec2pl)
qqline(rec2pl)
rec4pl<-(1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length)
qqnorm(rec4pl)
qqline(rec4pl)
rec8pl<-(1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length) * (1/iris$Petal.Length)
qqnorm(rec8pl)
qqline(rec8pl)####Normalisation achieved
pnorm(iris$Petal.Length,3.758,1.765298)
----------------------------------------------------------
#Measures of Central Tendency                Petal.Width
mean(iris$Petal.Width)  ### 1.199333
median(iris$Petal.Width)##### 1.3
#mode
getmode(iris$Petal.Width)  #####  0.2
#Measures of Dispersion
var(iris$Petal.Width)  ###### 0.5810063
sd(iris$Petal.Width)  ####  0.7622377
range(iris$Petal.Width) ##### 0.1  2.5
rangevalue <- function(x){max(x)-min(x)}
rangevalue(iris$Petal.Width) ##### 2.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(iris$Petal.Width)  #### -0.1019342
#Measures of Kurtosis 
kurtosis(iris$Petal.Width)  ###### 1.663933
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(iris$Petal.Width,horizontal = TRUE)
hist(iris$Petal.Width)
barplot(iris$Petal.Width)
#qqplot
qqnorm(iris$Petal.Width)
qqline(iris$Petal.Width)
logpw<-log(iris$Petal.Width)
qqnorm(logpw)
qqline(logpw)
exppw<-exp(iris$Petal.Width)
qqnorm(exppw)
qqline(exppw)
sqrtpw<-sqrt(iris$Petal.Width)
qqnorm(sqrtpw)
qqline(sqrtpw)
algpw<-(iris$Petal.Width * iris$Petal.Width) + iris$Petal.Width
qqnorm(algpw)
qqline(algpw)
recpw<-(1/(iris$Petal.Width))           
qqnorm(recpw)
qqline(recpw)
rec2pw<- (1/(iris$Petal.Width * iris$Petal.Width))  
qqnorm(rec2pw)
qqline(rec2pw)
rec4pw<-rec2pw * rec2pw
qqnorm(rec4pw)
qqline(rec4pw)
rec8pw<-rec4pw * rec4pw
qqnorm(rec8pw)
qqline(rec8pw)   ###Normalisation achieved
pnorm(iris$Petal.Width,1.199333,0.7622377)
------------------------------------------------------------------------
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
# Building model on training data 
irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(irisc5.0_train,iris_train)
mean(iris_train$Species==pred_train) # 97.33% Accuracy
library(caret)
confusionMatrix(pred_train,iris_train$Species)
predc5.0_test <- predict(irisc5.0_train,newdata=iris_test) # predicting on test data
mean(predc5.0_test==iris_test$Species) # 94.66% accuracy 
confusionMatrix(predc5.0_test,iris_test$Species)
library(gmodels)
# Cross tablez
CrossTable(iris_test$Species,predc5.0_test)
##### Using tree function 
library(tree)
# Building a model on training data 
iris_tree <- tree(Species~.,data=iris_train)
plot(iris_tree)
text(iris_tree,pretty = 0)
# Predicting the test data using the model
pred_tree <- as.data.frame(predict(iris_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(iris_tree,newdata=iris_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
mean(pred_tree$final==iris_test$Species) # Accuracy = 94.66%
CrossTable(iris_test$Species,pred_tree$final)
##########Using Bagging
library(rpart)
library(adabag)
library(mlbench)
###l <- length(Fraud[,1])
##sub <- sample(1:l,2*l/3)
if (bag == TRUE) { 
  k <- 1
  while (k == 1){
    boostrap <- iris_train(1:n, replace = TRUE, prob = pesos)
    fit <- rpart(Species ~ ., data = iris_train[boostrap, -1],
                 control = control)
    k <- length(fit$frame$var)
  }
  flearn <- predict(fit, newdata = iris_test, type = "class")
  ind <- as.numeric(vardep != flearn)
  err <- sum(pesos * ind)
}
iris.bagging <- bagging(Species ~.,data=iris_train,bag = TRUE,mfinal=5,control=rpart.control(maxdepth=5, minsplit=15,CP=0))
#Using the pruning option
iris.bagging.pred <- predict.bagging(iris.bagging,newdata=iris_test, newmfinal=3)
iris.bagging.pred$confusion
iris.bagging.pred$error
#######################Using Boosting
iris.rpart <- rpart(Species~.,data=iris_train,maxdepth=5)
iris.rpart.pred <- predict(iris.rpart,newdata=iris_test,type="class")
tb <- table(iris.rpart.pred,iris_test[,5])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart
iris.adaboost <- boosting(Species ~.,data=iris_train[1:75,],mfinal=3, coeflearn="Zhu",
                           control=rpart.control(maxdepth=5))
##Fraud.adaboost1 <- boosting(Taxable.Income ~.,data=Fraud_test[1:300,],mfinal=3, coeflearn="Zhu",
##                         control=rpart.control(maxdepth=5))
iris.adaboost.pred <- predict.boosting(iris.adaboost,newdata=iris_test[1:75,])
iris.adaboost.pred$confusion
iris.adaboost.pred$error
#comparing error evolution in training and test set
errorevol(iris.adaboost,newdata=iris_train[1:75, ])->evol.train
errorevol(iris.adaboost,newdata=iris_test[1:75, ])->evol.test
plot.errorevol(evol.test,evol.train)
