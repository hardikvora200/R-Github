library(naivebayes)
library(ggplot2)
library(caret)
library(caret)
library(e1071)
#### Data(Train)
train_sal <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Naive Bayes\\SalaryData_Train.csv")
View(train_sal)
attach(train_sal)
sum(is.na(train_sal))  ##0
train_sal <- na.omit(train_sal) # Omitting NA values from the Data if it is there
dim(train_sal)### 30161  14
colnames(train_sal)
set.seed(30161)
library(class)
-------------------------------------------------------------------------------
#Measures of Central Tendency                age from training data
mean(train_sal$age)  ### -38.43812
median(train_sal$age)##### 37
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(train_sal$age)  ###### 36
#Measures of Dispersion
var(train_sal$age)  ###### 172.5238
sd(train_sal$age)  #### 13.13483
range(train_sal$age)##### 17  90
rangevalue <- function(x){max(x)-min(x)}
rangevalue(train_sal$age) ##### 73
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(train_sal$age)  #### 0.5301541
#Measures of Kurtosis 
kurtosis(train_sal$age)######  2.855065
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(train_sal$age,horizontal = TRUE)
hist(train_sal$age)
barplot(Zoo$hair)
str(train_sal)
#qqplot
qqnorm(train_sal$age)
qqline(train_sal$age)
library(psych)
describe(train_sal)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(train_sal$age,-38.43812,13.13483)
-----------------------------------------------------------------------
#Measures of Central Tendency                educationno from training data
mean(train_sal$educationno)  ### -10.12132
median(train_sal$educationno)##### 10
#mode
getmode(train_sal$educationno)  #####  9
#Measures of Dispersion
var(train_sal$educationno)  ###### 6.502689
sd(train_sal$educationno)  ####  2.550037
range(train_sal$educationno) ##### 1  16
rangevalue <- function(x){max(x)-min(x)}
rangevalue(train_sal$educationno) ##### 15
#Measures of skewness
library(moments)
#Measures of skewness
skewness(train_sal$educationno)  #### -0.3053632
#Measures of Kurtosis 
kurtosis(train_sal$educationno)###### 3.643299
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(train_sal$educationno,horizontal = TRUE)
hist(train_sal$educationno)
barplot(train_sal$educationno)
#qqplot
qqnorm(train_sal$educationno)
qqline(train_sal$educationno)
expeducationno<-exp(train_sal$educationno)
qqnorm(expeducationno)
qqline(expeducationno)
logeducationno<-log(train_sal$educationno)
qqnorm(logeducationno)
qqline(logeducationno)
sqrteducationno<-sqrt(train_sal$educationno)
qqnorm(sqrteducationno)
qqline(sqrteducationno)
receducationno<-(1/(train_sal$educationno))
qqnorm(receducationno)
qqline(receducationno)
rec2educationno<-(1/(train_sal$educationno)) * (1/(train_sal$educationno))
qqnorm(rec2educationno)
qqline(rec2educationno)
pnorm(train_sal$educationno,-10.12132,2.550037)
-----------------------------------------------------------------------    
#Measures of Central Tendency                capitalgain from training data
mean(train_sal$capitalgain)  ### -1092.044
median(train_sal$capitalgain)##### 0
#mode
getmode(train_sal$capitalgain)  ##### 0
#Measures of Dispersion
var(train_sal$capitalgain)  ###### 54855748
sd(train_sal$capitalgain)  #### 7406.467
range(train_sal$capitalgain) #####  0 99999
rangevalue <- function(x){max(x)-min(x)}
rangevalue(train_sal$capitalgain) ##### 99999
#Measures of skewness
library(moments)
#Measures of skewness
skewness(train_sal$capitalgain)  #### 11.90189
#Measures of Kurtosis 
kurtosis(train_sal$capitalgain)  ###### 156.6355
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(train_sal$capitalgain,horizontal = TRUE)
hist(train_sal$capitalgain)
barplot(train_sal$capitalgain)
#qqplot
qqnorm(train_sal$capitalgain)
qqline(train_sal$capitalgain)
pnorm(train_sal$capitalgain,-1092.044,7406.467)
------------------------------------------------------------------
#Measures of Central Tendency                capitalloss from training data
mean(train_sal$capitalloss)  ### 88.30231
median(train_sal$capitalloss)##### 0
#mode
getmode(train_sal$capitalloss)  ##### 0
#Measures of Dispersion
var(train_sal$capitalloss)  ###### 163314
sd(train_sal$capitalloss)  #### 0.4935224
range(train_sal$capitalloss) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(train_sal$capitalloss) ##### 4356
#Measures of skewness
library(moments)
#Measures of skewness
skewness(train_sal$capitalloss)  #### 4.528013
#Measures of Kurtosis 
kurtosis(train_sal$capitalloss)###### 22.52585
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(train_sal$capitalloss,horizontal = TRUE)
hist(train_sal$capitalloss)
barplot(train_sal$capitalloss)
#qqplot
qqnorm(train_sal$capitalloss)
qqline(train_sal$capitalloss)
pnorm(train_sal$capitalloss,88.30231,0.4935224)
-------------------------------------------------------------------
#Measures of Central Tendency                hoursperweek from training data
mean(train_sal$hoursperweek)  ### -40.93127
median(train_sal$hoursperweek)##### 40
#mode
getmode(train_sal$hoursperweek)  ##### 40
#Measures of Dispersion
var(train_sal$hoursperweek)  ###### 143.5248
sd(train_sal$hoursperweek)  #### 11.98018
range(train_sal$hoursperweek) ##### 1  99
rangevalue <- function(x){max(x)-min(x)}
rangevalue(train_sal$hoursperweek) ##### 98
#Measures of skewness
library(moments)
#Measures of skewness
skewness(train_sal$hoursperweek)  #### 0.3308397
#Measures of Kurtosis 
kurtosis(train_sal$hoursperweek)###### 6.166959
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(train_sal$hoursperweek,horizontal = TRUE)
hist(train_sal$hoursperweek)
barplot(train_sal$hoursperweek)
#qqplot
qqnorm(train_sal$hoursperweek)
qqline(train_sal$hoursperweek)
exphoursperweek<-exp(train_sal$hoursperweek)
qqnorm(exphoursperweek)
qqline(exphoursperweek)
pnorm(train_sal$hoursperweek,-40.93127,11.98018)
--------------------------------------------------------------------------
#####Data(Test)
test_sal <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Naive Bayes\\SalaryData_Test.csv")
str(test_sal)
View(test_sal)
attach(test_sal)
sum(is.na(test_sal))  ##0
test_sal <- na.omit(test_sal) # Omitting NA values from the Data if it is there
dim(test_sal)### 15060  14
colnames(test_sal)
set.seed(15060)
--------------------------------------------------------------------------
#Measures of Central Tendency                age from test data
mean(test_sal$age)  ### 38.76833
median(test_sal$age)##### 37
getmode(test_sal$age)  ###### 35
#Measures of Dispersion
var(test_sal$age)  ###### 179.0425
sd(test_sal$age)  #### 13.38068
range(test_sal$age)##### 17  90
rangevalue <- function(x){max(x)-min(x)}
rangevalue(test_sal$age) ##### 73
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(test_sal$age)  #### 0.5360584
#Measures of Kurtosis 
kurtosis(test_sal$age)######  2.81835
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(test_sal$age,horizontal = TRUE)
hist(test_sal$age)
barplot(test_sal$age)
str(test_sal)
#qqplot
qqnorm(test_sal$age)
qqline(test_sal$age)
library(psych)
describe(test_sal)
pnorm(test_sal$age,38.76833,13.38068)
-----------------------------------------------------------------------
#Measures of Central Tendency                educationno from test data
mean(test_sal$educationno)  ### -10.11275
median(test_sal$educationno)##### 10
#mode
getmode(test_sal$educationno)  #####  9
#Measures of Dispersion
var(test_sal$educationno)  ###### 6.547085
sd(test_sal$educationno)  ####  2.558727
range(test_sal$educationno) ##### 1  16
rangevalue <- function(x){max(x)-min(x)}
rangevalue(test_sal$educationno) ##### 15
#Measures of skewness
library(moments)
#Measures of skewness
skewness(test_sal$educationno)  #### -0.3209674
#Measures of Kurtosis 
kurtosis(test_sal$educationno)###### 3.617864
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(test_sal$educationno,horizontal = TRUE)
hist(test_sal$educationno)
barplot(test_sal$educationno)
#qqplot
qqnorm(test_sal$educationno)
qqline(test_sal$educationno)
expeducationno<-exp(test_sal$educationno)
qqnorm(expeducationno)
qqline(expeducationno)
logeducationno<-log(test_sal$educationno)
qqnorm(logeducationno)
qqline(logeducationno)
sqrteducationno<-sqrt(test_sal$educationno)
qqnorm(sqrteducationno)
qqline(sqrteducationno)
receducationno<-(1/(test_sal$educationno))
qqnorm(receducationno)
qqline(receducationno)
rec2educationno<-(1/(test_sal$educationno)) * (1/(test_sal$educationno))
qqnorm(rec2educationno)
qqline(rec2educationno)
pnorm(test_sal$educationno,-10.11275,2.558727)
-----------------------------------------------------------------------    
#Measures of Central Tendency                capitalgain from test data
mean(test_sal$capitalgain)  ### -1120.302
median(test_sal$capitalgain)##### 0
#mode
getmode(test_sal$capitalgain)  ##### 0
#Measures of Dispersion
var(test_sal$capitalgain)  ###### 59339010
sd(test_sal$capitalgain)  #### 7703.182
range(test_sal$capitalgain) #####  0 99999
rangevalue <- function(x){max(x)-min(x)}
rangevalue(test_sal$capitalgain) ##### 99999
#Measures of skewness
library(moments)
#Measures of skewness
skewness(test_sal$capitalgain)  #### 11.569
#Measures of Kurtosis 
kurtosis(test_sal$capitalgain)  ###### 146.5433
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(test_sal$capitalgain,horizontal = TRUE)
hist(test_sal$capitalgain)
barplot(test_sal$capitalgain)
#qqplot
qqnorm(test_sal$capitalgain)
qqline(test_sal$capitalgain)
pnorm(test_sal$capitalgain,-1120.302,7703.182)
------------------------------------------------------------------
#Measures of Central Tendency                capitalloss from training data
mean(test_sal$capitalloss)  ### 89.0419
median(test_sal$capitalloss)##### 0
#mode
getmode(test_sal$capitalloss)  ##### 0
#Measures of Dispersion
var(test_sal$capitalloss)  ###### 165066.1
sd(test_sal$capitalloss)  #### 406.2832
range(test_sal$capitalloss) ##### 0  3770
rangevalue <- function(x){max(x)-min(x)}
rangevalue(test_sal$capitalloss) ##### 3770
#Measures of skewness
library(moments)
#Measures of skewness
skewness(test_sal$capitalloss)  #### 4.496291
#Measures of Kurtosis 
kurtosis(test_sal$capitalloss)###### 22.07395
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(test_sal$capitalloss,horizontal = TRUE)
hist(test_sal$capitalloss)
barplot(test_sal$capitalloss)
#qqplot
qqnorm(test_sal$capitalloss)
qqline(test_sal$capitalloss)
pnorm(test_sal$capitalloss,89.0419,406.2832)
-------------------------------------------------------------------
#Measures of Central Tendency                hoursperweek from test data
mean(test_sal$hoursperweek)  ### -40.95159
median(test_sal$hoursperweek)##### 40
#mode
getmode(test_sal$hoursperweek)  ##### 40
#Measures of Dispersion
var(test_sal$hoursperweek)  ###### 145.5119
sd(test_sal$hoursperweek)  #### 12.06283
range(test_sal$hoursperweek) ##### 1  99
rangevalue <- function(x){max(x)-min(x)}
rangevalue(test_sal$hoursperweek) ##### 98
#Measures of skewness
library(moments)
#Measures of skewness
skewness(test_sal$hoursperweek)  #### 0.3594659
#Measures of Kurtosis 
kurtosis(test_sal$hoursperweek)###### 6.265828
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(test_sal$hoursperweek,horizontal = TRUE)
hist(test_sal$hoursperweek)
barplot(test_sal$hoursperweek)
#qqplot
qqnorm(test_sal$hoursperweek)
qqline(test_sal$hoursperweek)
exphoursperweek<-exp(test_sal$hoursperweek)
qqnorm(exphoursperweek)
qqline(exphoursperweek)
pnorm(test_sal$hoursperweek,-40.95159,12.06283)  
-------------------------------------------------------------------
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)  
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
#Visualization - Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +  ggtitle("Box Plot")
plot(train_sal$workclass,train_sal$Salary)
plot(train_sal$education,train_sal$Salary)
plot(train_sal$educationno,train_sal$Salary)
plot(train_sal$maritalstatus,train_sal$Salary)
plot(train_sal$occupation,train_sal$Salary)
plot(train_sal$relationship,train_sal$Salary)
plot(train_sal$race,train_sal$Salary)
plot(train_sal$sex,train_sal$Salary)
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary))+geom_boxplot()+ggtitle("Box Plot")
plot(train_sal$native,train_sal$Salary)
#Density Plot 
ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Age - Density Plot")
ggplot(data=train_sal,aes(x = train_sal$workclass, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Workclass Density Plot")
ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("education Density Plot")
ggplot(data=train_sal,aes(x = train_sal$educationno, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("educationno Density Plot")
ggplot(data=train_sal,aes(x = train_sal$maritalstatus, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("maritalstatus Density Plot")
ggplot(data=train_sal,aes(x = train_sal$occupation, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("occupation Density Plot")
ggplot(data=train_sal,aes(x = train_sal$sex, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("sex Density Plot")
ggplot(data=train_sal,aes(x = train_sal$relationship, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Relationship Density Plot")
ggplot(data=train_sal,aes(x = train_sal$race, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Race Density Plot")
ggplot(data=train_sal,aes(x = train_sal$capitalgain, fill = train_sal$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalgain Density Plot")
ggplot(data=train_sal,aes(x = train_sal$capitalloss, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalloss Density Plot")
ggplot(data=train_sal,aes(x = train_sal$hoursperweek, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Hoursperweek Density Plot")
ggplot(data=train_sal,aes(x = train_sal$native, fill = train_sal$Salary))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("native Density Plot")
# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model
Model_pred <- predict(Model,test_sal)
library(gmodels)
CrossTable(Model_pred,test_sal$Salary,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','Actual'))
length(train_sal$Salary)
mean(Model_pred==test_sal$Salary) ###81.87251% accuracy
confusionMatrix(Model_pred,test_sal$Salary)
table(Model_pred)
table(test_sal$Salary)
####Improving model performance
Model_1 <- naiveBayes(train_sal$Salary ~ ., data = train_sal,laplace = 1)
Model_1
Model_pred_1 <- predict(Model_1,test_sal)
CrossTable(Model_pred_1,test_sal$Salary,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','Actual'))
mean(Model_pred_1==test_sal$Salary) ###81.85923% accuracy
confusionMatrix(Model_pred_1,test_sal$Salary)
table(Model_pred_1)
table(test_sal$Salary)
---------------------------------------------------------------------------
##########Using Bagging
library(naivebayes)
library(adabag)
library(mlbench)
if (bag == TRUE) { 
  k <- 1
  while (k == 1){
    boostrap <- train_sal(1:n, replace = TRUE, prob = pesos)
    fit <- naivebayes(salary ~ ., data = train_sal[boostrap, -1],
                      control = control)
    k <- length(fit$frame$var)
  }
  flearn <- predict(fit, newdata = test_sal, type = "class")
  ind <- as.numeric(vardep != flearn)
  err <- sum(pesos * ind)
}
train_sal<-as.data.frame(train_sal)
test_sal<-as.data.frame(test_sal)
sal.bagging <- bagging(Salary ~.,data=train_sal,bag = TRUE,mfinal=5)
?naivebayes()
#Using the pruning option
sal.bagging.pred <- predict.bagging(sal.bagging,newdata=test_sal, newmfinal=3)
sal.bagging.pred$confusion
sal.bagging.pred$error ##0.1610226
#######################Using Boosting
sal.naivebayes <- naive_bayes(Salary~.,data=train_sal,maxdepth=5,laplace = 3)
sal.naivebayes.pred <- predict(sal.naivebayes,newdata=test_sal,type="class")
tb <- table(sal.naivebayes.pred,test_sal[,14])
er
error.naivebayes <- 1-(sum(diag(tb))/sum(tb))
tb
error.naivebayes  ###0.1816069
sal.adaboost <- boosting(Salary ~.,data=train_sal[1:30161,],mfinal=3, coeflearn="Zhu")
                
sal.adaboost.pred <- predict.boosting(sal.adaboost,newdata=test_sal[1:15060,])
sal.adaboost.pred$confusion
sal.adaboost.pred$error   ##0.1779548
#comparing error evolution in training and test set
errorevol(sal.adaboost,newdata=train_sal[1:30161, ])->evol.train
errorevol(sal.adaboost,newdata=test_sal[1:15060, ])->evol.test
plot.errorevol(evol.test,evol.train)


