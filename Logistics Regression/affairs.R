
affair <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Logistics Regression\\affairs.csv")
attach(affair)
View(affair)
sum(is.na(affair))
affair <- na.omit(affair) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(affair)
colnames(affair)
##claimants <- claimants[,-1] # Removing the first column which is is an Index
-------------------------------------------------
#Measures of Central Tendency                X
mean(affair$X)  ###1059.722
median(affair$X)#####1009
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(affair$X)  ######4
#Measures of Dispersion
var(affair$X)  ######837050.4
sd(affair$X)  ####914.9046
range(affair$X)##### 4 9029
rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$X) #####9025
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(affair$X)  ####5.39999
#Measures of Kurtosis 
kurtosis(affair$X)######47.97975
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(affair$X,horizontal = TRUE)
hist(affair$X)
barplot(affair$X)
str(affair)
#qqplot
qqnorm(affair$X)
qqline(affair$X)
logx<-log(affair$X)
qqnorm(logx)
qqline(logx)
sqrtx<-sqrt(affair$X)
qqnorm(sqrtx)
qqline(sqrtx)
sqrx<-(affair$X * affair$X)
qqnorm(sqrx)
qqline(sqrx)###Normalisation achieved
affair$X <- as.factor(affair$X)
install.packages(psych)
library(psych)
describe(affair)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
-----------------------------------------------------------------------
#Measures of Central Tendency                AGE
mean(affair$age)  ### -32.48752 
median(affair$age)##### 32
#mode
getmode(affair$age)  ######27
#Measures of Dispersion
var(affair$age)  ###### 86.28109
sd(affair$age)  ####9.288762
range(affair$age)##### 17.5 57.0
rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$age) #####39.5
#Measures of skewness
library(moments)
#Measures of skewness
skewness(affair$age)  ### 0.8869999
#Measures of Kurtosis 
kurtosis(affair$age)###### 3.220077
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(affair$age,horizontal = TRUE)
hist(affair$age)
barplot(affair$age)
#qqplot
qqnorm(affair$age)
qqline(affair$age)
expage<-exp(affair$age)
qqnorm(expage)
qqline(expage)  ####Normalisation achieved
affair$age <- as.factor(affair$age)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Yearsmarried
mean(affair$yearsmarried)  ### 8.177696 
median(affair$yearsmarried)##### 7
#mode
getmode(affair$yearsmarried)  ######15
#Measures of Dispersion
var(affair$yearsmarried)  ###### 31.03942
sd(affair$yearsmarried)  #### 5.571303
range(affair$yearsmarried)##### 0.125 15.000
rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$yearsmarried) #### 14.875
#Measures of skewness
skewness(affair$yearsmarried)  ### 0.07799352
#Measures of Kurtosis 
kurtosis(affair$yearsmarried)###### 1.432516
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(affair$yearsmarried,horizontal = TRUE)
hist(affair$yearsmarried)
barplot(affair$yearsmarried)
str(affair)
#qqplot
qqnorm(affair$yearsmarried)
qqline(affair$yearsmarried)
expym<-exp(affair$yearsmarried)
qqnorm(expym)
qqline(expym)
logym<-log(affair$yearsmarried)
qqnorm(logym)
recym<-(1/affair$yearsmarried)
qqnorm(recym)
qqline(recym)
rec2ym<-(1/(affair$yearsmarried * affair$yearsmarried))
qqnorm(rec2ym)
qqline(rec2ym)
rec4ym<-(1/(affair$yearsmarried * affair$yearsmarried)) * (1/(affair$yearsmarried * affair$yearsmarried))
qqnorm(rec4ym)
qqline(rec4ym)
affair$yearsmarried <- as.factor(affair$yearsmarried)
------------------------------------------------------------------
#Measures of Central Tendency                religiousness
mean(affair$religiousness)  ### 3.116473 
median(affair$religiousness)##### 3
#mode
getmode(affair$religiousness)  ###### 4
#Measures of Dispersion
var(affair$religiousness)  ###### 1.363078
sd(affair$religiousness)  #### 1.167509
range(affair$religiousness)##### 1 5
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$religiousness) #### 4
#Measures of skewness
skewness(affair$religiousness)  ### -0.08880018
#Measures of Kurtosis 
kurtosis(affair$religiousness)###### 1.990046
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(affair$religiousness,horizontal = TRUE)
hist(affair$religiousness)
barplot(affair$yearsmarried)
##str(affair)
#qqplot
qqnorm(affair$religiousness)
qqline(affair$religiousness)
expre<-exp(affair$religiousness)
qqnorm(expre)
qqline(expre)
logre<-log(affair$religiousness)
qqnorm(logre)
recre<-(1/affair$religiousness)
qqnorm(recre)
qqline(recre)
rec2re<-(1/(affair$religiousness * affair$religiousness))
qqnorm(rec2re)
qqline(rec2re)
rec4re<-(1/(affair$religiousness * affair$religiousness)) * (1/(affair$religiousness * affair$religiousness))
qqnorm(rec4re)
qqline(rec4re)
rec8re<-(1/(affair$religiousness * affair$religiousness)) * (1/(affair$religiousness * affair$religiousness)) * (1/(affair$religiousness * affair$religiousness)) * (1/(affair$religiousness * affair$religiousness))
qqnorm(rec8re)
qqline(rec8re)
affair$religiousness <- as.factor(affair$religiousness)
-------------------------------------------------------------------------------
#Measures of Central Tendency                education
mean(affair$education)  ### -16.16639 
median(affair$education)##### 16
#mode
getmode(affair$education)  ###### 14
#Measures of Dispersion
var(affair$education)  ###### 5.772268
sd(affair$education)  #### 2.402555
range(affair$education)##### 9  20
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$education) #### 11
#Measures of skewness
skewness(affair$education)  ### -0.2496477
#Measures of Kurtosis 
kurtosis(affair$education)###### 2.690708
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(affair$education,horizontal = TRUE)
hist(affair$education)
barplot(affair$education)
#qqplot
qqnorm(affair$education)
qqline(affair$education)
expedu<-exp(affair$education)
qqnorm(expedu)
qqline(expedu)
logedu<-log(affair$education)
qqnorm(logedu)
sqredu<-sqrt(affair$education)
qqnorm(sqredu)
qqline(sqredu)
recedu<-(1/affair$education)
qqnorm(recedu)
qqline(recedu)
rec2edu<-(1/(affair$education * affair$education))
qqnorm(rec2edu)
qqline(rec2edu)
rec4edu<-(1/(affair$education * affair$education)) * (1/(affair$education * affair$education))
qqnorm(rec4edu)
qqline(rec4edu)
rec8edu<-(1/(affair$education * affair$education)) * (1/(affair$education * affair$education)) * (1/(affair$education * affair$education)) * (1/(affair$education * affair$education))
qqnorm(rec8edu)
qqline(rec8edu)
affair$education <- as.factor(affair$education)
-----------------------------------------------------------------------------
#Measures of Central Tendency                occupation
mean(affair$occupation)  ### -4.194676 
median(affair$occupation)##### 5
#mode
getmode(affair$occupation)  ###### 5
#Measures of Dispersion
var(affair$occupation)  ###### 3.310372
sd(affair$occupation)  #### 1.819443
range(affair$occupation)##### 1  7
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(affair$occupation) #### 6
#Measures of skewness
skewness(affair$occupation)  ### -0.7387374
#Measures of Kurtosis 
kurtosis(affair$occupation)###### 2.220779
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(affair$occupation,horizontal = TRUE)
hist(affair$occupation)
barplot(affair$occupation)
##str(affair)
#qqplot
qqnorm(affair$occupation)
qqline(affair$occupation)
expocc<-exp(affair$occupation)
qqnorm(expocc)
qqline(expocc)
logocc<-log(affair$occupation)
qqnorm(logocc)
sqrocc<-sqrt(affair$occupation)
qqnorm(sqrocc)
qqline(sqrocc)
recocc<-(1/affair$occupation)
qqnorm(recocc)
qqline(recocc)
rec2occ<-(1/(affair$occupation * affair$occupation))
qqnorm(rec2occ)
qqline(rec2occ)
rec4occ<-(1/(affair$occupation * affair$occupation)) * (1/(affair$occupation * affair$occupation))
qqnorm(rec4occ)
qqline(rec4occ)
rec8occ<-(1/(affair$occupation * affair$occupation)) * (1/(affair$occupation * affair$occupation)) * (1/(affair$occupation * affair$occupation)) * (1/(affair$occupation * affair$occupation))
qqnorm(rec8occ)
qqline(rec8occ)
affair$occupation <- as.factor(affair$occupation)
affair$rating<-as.factor(affair$rating)
------------------------------------------------------------------
# Preparing a linear regression 
mod_lm <- lm(affairs~.,data=affair)
pred1 <- predict(mod_lm,affair)
pred1
plot(affair$affairs,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)
# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exclude those values 
# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
affair$gender<-as.factor(affair$gender)
affair$children<-as.factor(affair$children)
affair$affairs<-as.factor(affair$affairs)
affair$rating<-as.factor(affair$rating)
model <- glm(affairs~.,data=affair,family = "binomial")
View(model)
plot(model)
str(affair)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,affair,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare between different models
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,affair$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.8069884
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
# Creating new column to store the above values
for(i in seq_along(affair)){
  affair[[i]] <- as.data.frame(affair[[i]])
}
affair[,"prob"] <- prob
affair[,"pred_values"] <- pred_values
affair[,"yes_no"] <- yes_no
View(affair[,c(1,11:13)])
View(affair)
length(affair$occupation)
length(affair$pred_values)
na.omit(affair)
table(affair$affairs,affair$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59
# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
data(ROCR.simple)
View(ROCR.simple)
?performance()
rocrpred<-prediction(ROCR.simple$predictions,ROCR.simple$labels)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,8)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
