Fraud <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Decision Trees\\Fraud_check.csv")
View(Fraud)
attach(Fraud)
sum(is.na(Fraud))
Fraud <- na.omit(Fraud) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Fraud)### 600 6
colnames(Fraud)
-------------------------------------------------
#Measures of Central Tendency                Taxable.Income
mean(Fraud$Taxable.Income)  ### -55208.38
median(Fraud$Taxable.Income)##### 55074.5
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Fraud$Taxable.Income)  ######   59689
#Measures of Dispersion
var(Fraud$Taxable.Income)  ###### 686692989
sd(Fraud$Taxable.Income)  #### 26204.83
range(Fraud$Taxable.Income)##### 10003 99619
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Fraud$Taxable.Income) #####  89616
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Fraud$Taxable.Income)  #### 0.0299397
#Measures of Kurtosis 
kurtosis(Fraud$Taxable.Income)######   1.800209
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Fraud$Taxable.Income,horizontal = TRUE)
hist(Fraud$Taxable.Income)
barplot(Fraud$Taxable.Income)
str(Fraud)
#qqplot
qqnorm(Fraud$Taxable.Income)
qqline(Fraud$Taxable.Income)
logti<-log(Fraud$Taxable.Income)
qqnorm(logti)
qqline(logti)
sqrtti<-sqrt(Fraud$Taxable.Income)
qqnorm(sqrtti)
qqline(sqrtti)###Normalisation achieved
install.packages(psych)
library(psych)
describe(Fraud)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(Fraud$Taxable.Income,-55208.38,26204.83)
-----------------------------------------------------------------------
#Measures of Central Tendency                City Population
mean(Fraud$City.Population)  ### -108747.4
median(Fraud$City.Population)##### 106493.5
#mode
getmode(Fraud$City.Population)  #####57194
#Measures of Dispersion
var(Fraud$City.Population)  ###### 2485029991
sd(Fraud$City.Population)  ####  49850.08
range(Fraud$City.Population) ##### 25779  199778
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Fraud$City.Population) ##### 173999
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Fraud$City.Population)  ####0.1246962
#Measures of Kurtosis 
kurtosis(Fraud$City.Population)###### 1.879176
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Fraud$City.Population,horizontal = TRUE)
hist(Fraud$City.Population)
barplot(Fraud$City.Population)
#qqplot
qqnorm(Fraud$City.Population)
qqline(Fraud$City.Population)
logcp<-log(Fraud$City.Population)
qqnorm(logcp)
qqline(logcp) 
reclogcp<-(1/log(Fraud$City.Population))
qqnorm(reclogcp)
qqline(reclogcp)
sqrtcp<-sqrt(Fraud$City.Population)
qqnorm(sqrtcp)
qqline(sqrtcp)####Normalisation achieved
pnorm(Fraud$City.Population,-108747.4,49850.08)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Work.Experience
mean(Fraud$Work.Experience)  ### -15.55833
median(Fraud$Work.Experience)##### 15
#mode
getmode(Fraud$Work.Experience)  ###### 10
#Measures of Dispersion
var(Fraud$Work.Experience)  ###### 78.18357
sd(Fraud$Work.Experience)  #### 8.842147
range(Fraud$Work.Experience)##### 0  30
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Fraud$Work.Experience) #### 30
#Measures of skewness
skewness(Fraud$Work.Experience)  ### 0.01848227
#Measures of Kurtosis 
kurtosis(Fraud$Work.Experience)###### 1.8322
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Fraud$Work.Experience,horizontal = TRUE)
hist(Fraud$Work.Experience)
barplot(Fraud$Work.Experience)
#qqplot
qqnorm(Fraud$Work.Experience)
qqline(Fraud$Work.Experience)
expwe<-exp(Fraud$Work.Experience)
qqnorm(expwe)
qqline(expwe)   ########Normalisation achieved
pnorm(Fraud$Work.Experience,-15.55833,8.842147)
------------------------------------------------------------------
####Normalisation of data
normalized_data<-scale(Fraud[,3:5]) #excluding the ID# columnbefore normalizing
View(normalized_data)
### Either of these 2 functions(scale/Preprocess) to normalize the data
library(caret)
preObj <- preProcess(Fraud[,c(-1,-2,-6)], method=c("center", "scale"))
newData <- predict(preObj, Fraud[, c(-1,-2,-6)])
View(newData)
-----------------------------------------------------------------------
install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
##data()
##data("Fraud")
# Splitting data into training and testing. As the Taxable.Income are in order 
# splitting the data based on Taxable.Income 
Fraud_Taxable.Income<-Fraud[Fraud$Taxable.Income<="30000",]
View(Fraud_Taxable.Income)  ##124
Fraud_Taxable.Income1<-Fraud[Fraud$Taxable.Income>"30000", ]
View(Fraud_Taxable.Income1) ##476
Fraud_train <- rbind(Fraud_Taxable.Income[1:62,],Fraud_Taxable.Income1[1:238,])
View(Fraud_train)
Fraud_test <- rbind(Fraud_Taxable.Income[63:124,],Fraud_Taxable.Income1[239:476,])
View(Fraud_test)
# Building model on training data 
####Conversion of categorical variables into factors
Fraud_train$Undergrad<-as.factor(Fraud_train$Undergrad)
str(Fraud_train$Undergrad)
Fraud_train$Marital.Status<-as.factor(Fraud_train$Marital.Status)
str(Fraud_train$Marital.Status)
Fraud_train$Urban<-as.factor(Fraud_train$Urban)
str(Fraud_train$Urban)
Fraud_train$Taxable.Income<-as.factor(Fraud_train$Taxable.Income)
str(Fraud_train$Taxable.Income)
Fraud_train$Work.Experience<-as.factor(Fraud_train$Work.Experience)
str(Fraud_train$Work.Experience)
Fraud_train$City.Population<-as.factor(Fraud_train$City.Population)
str(Fraud_train$City.Population)
-----------------------------------------------------------------------
Fraud_test$Undergrad<-as.factor(Fraud_test$Undergrad)
str(Fraud_test$Undergrad)
Fraud_test$Marital.Status<-as.factor(Fraud_test$Marital.Status)
str(Fraud_test$Marital.Status)
Fraud_test$Urban<-as.factor(Fraud_test$Urban)
str(Fraud_test$Urban)
Fraud_test$Taxable.Income<-as.factor(Fraud_test$Taxable.Income)
str(Fraud_test$Taxable.Income)
Fraud_test$Work.Experience<-as.factor(Fraud_test$Work.Experience)
str(Fraud_test$Work.Experience)
Fraud_test$City.Population<-as.factor(Fraud_test$City.Population)
str(Fraud_test$City.Population)
library(C50)
Fraudc5.0_train <- C5.0(Fraud_train[,-c(1,2,6)],Fraud_train$Taxable.Income)
?C5.0
plot(Fraudc5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(Fraudc5.0_train,Fraud_train)
mean(Fraud_train$Taxable.Income==pred_train) ### 99.66667% accuracy
library(caret)
confusionMatrix(pred_train,Fraud_train$Taxable.Income)
-------------------------------------------------------------
Fraudc5.0_test <- C5.0(Fraud_test[,-c(1,2,6)],Fraud_test$Taxable.Income)  
predc5.0_test <- predict(Fraudc5.0_test,newdata=Fraud_test) # predicting on test data
mean(predc5.0_test==Fraud_test$Taxable.Income) # 100% accuracy 
confusionMatrix(predc5.0_test,Fraud_test$Taxable.Income)
library(gmodels)
# Cross tables
CrossTable(Fraud_test$Taxable.Income,predc5.0_test)
##### Using tree function 
library(tree)
# Building a model on training data 
Fraud_tree <- tree(Taxable.Income ~ .,data=Fraud_train)
plot(Fraud_tree)
text(Fraud_tree,pretty = 0)
# Predicting the test data using the model
Fraud_train$City.Population <- as.numeric(scale(Fraud_train$City.Population))
View(Fraud_train$City.Population)
Fraud_train$Work.Experience <- as.numeric(scale(Fraud_train$Work.Experience))
View(Fraud_train$Work.Experience)
Fraud_train$Taxable.Income<-as.numeric(scale(Fraud_train$Taxable.Income))
View(Fraud_train$Taxable.Income)
-----------------------------------------------------------------
library(caret)
preObj <- preProcess(Fraud_test[,c(-1,-2,-6)], method=c("center", "scale"))
newData <- predict(preObj, Fraud_test[, c(-1,-2,-6)])
View(newData)
###Predicting the test data using the model
pred_tree <- as.data.frame(predict(Fraud_tree,newdata=Fraud_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(Fraud_tree,newdata=Fraud_test)
pred_tree$final <- colnames(pred_test_df)[lapply(pred_test_df,which.max)]
pred_tree$final
mean(pred_tree$final == Fraud_test$Taxable.Income) 
library(gmodels)
CrossTable(Fraud_test$Taxable.Income,pred_tree$final)
##########Using Bagging
library(rpart)
library(adabag)
library(mlbench)
l <- length(Fraud[,1])
##sub <- sample(1:l,2*l/3)
if (bag == TRUE) { 
     k <- 1
     while (k == 1){
    boostrap <- Fraud_train(1:n, replace = TRUE, prob = pesos)
    fit <- rpart(Taxable.Income ~ ., data = Fraud_train[boostrap, -1],
                 control = control)
       k <- length(fit$frame$var)
       }
  flearn <- predict(fit, newdata = Fraud_test, type = "class")
  ind <- as.numeric(vardep != flearn)
  err <- sum(pesos * ind)
}
Fraud.bagging <- bagging(Taxable.Income ~.,data=Fraud_train,bag = TRUE,mfinal=5,control=rpart.control(maxdepth=5, minsplit=15,CP=0))
#Using the pruning option
Fraud.bagging.pred <- predict.bagging(Fraud.bagging,newdata=Fraud_test, newmfinal=3)
Fraud.bagging.pred$confusion
Fraud.bagging.pred$error
#######################Using Boosting
Fraud.rpart <- rpart(Taxable.Income~.,data=Fraud_train,maxdepth=5)
Fraud.rpart.pred <- predict(Fraud.rpart,newdata=Fraud_test,type="class")
tb <- table(Fraud.rpart.pred,Fraud_test[,6])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart
Fraud.adaboost <- boosting(Taxable.Income ~.,data=Fraud_train[1:300,],mfinal=3, coeflearn="Zhu",
                             control=rpart.control(maxdepth=5))
##Fraud.adaboost1 <- boosting(Taxable.Income ~.,data=Fraud_test[1:300,],mfinal=3, coeflearn="Zhu",
  ##                         control=rpart.control(maxdepth=5))
Fraud.adaboost.pred <- predict.boosting(Fraud.adaboost,newdata=Fraud_test[1:300,])
Fraud.adaboost.pred$confusion
Fraud.adaboost.pred$error
#comparing error evolution in training and test set
errorevol(Fraud.adaboost,newdata=Fraud_train[1:300, ])->evol.train
errorevol(Fraud.adaboost,newdata=Fraud_test[1:300, ])->evol.test
plot.errorevol(evol.test,evol.train)
