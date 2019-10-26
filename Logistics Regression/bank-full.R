
bank <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Logistics Regression\\bank-full.csv")
attach(bank)
View(bank)
sum(is.na(bank))
bank <- na.omit(bank) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(bank)
colnames(bank)
##claimants <- claimants[,-1] # Removing the first column which is is an Index
-------------------------------------------------
#Measures of Central Tendency                age
mean(bank$age)  ### -40.93621
median(bank$age)##### 39
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(bank$age)  ######32
#Measures of Dispersion
var(bank$age)  ###### 112.7581
sd(bank$age)  #### 10.61876
range(bank$age)##### 18  95
rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$age) #####  77
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(bank$age)  #### 0.6847952
#Measures of Kurtosis 
kurtosis(bank$age)###### 3.319402
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(bank$age,horizontal = TRUE)
hist(bank$age)
barplot(bank$age)
str(bank)
#qqplot
qqnorm(bank$age)
qqline(bank$age)
logx<-log(bank$age)
qqnorm(logx)
qqline(logx)
expx<-exp(bank$age)
qqnorm(expx)
qqline(expx)#####Normalisation of age achieved
sqrtx<-sqrt(bank$age)
qqnorm(sqrtx)
qqline(sqrtx)
sqrx<-(bank$age * bank$age)
qqnorm(sqrx)
qqline(sqrx)###Normalisation achieved
pnorm(bank$age,-40.93621,10.61876)
install.packages(psych)
library(psych)
describe(bank)
bank$age <- as.factor(bank$age)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
-----------------------------------------------------------------------
#Measures of Central Tendency                balance
mean(bank$balance)  ### -1362.272 
median(bank$balance)##### 448
#mode
getmode(bank$balance)  ###### 0
#Measures of Dispersion
var(bank$balance)  ###### 9270599
sd(bank$balance)  #### 3044.766
range(bank$balance)##### -8019 102127
rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$balance) ##### 110146
#Measures of skewness
library(moments)
#Measures of skewness
skewness(bank$balance)  ### 8.360031
#Measures of Kurtosis 
kurtosis(bank$balance)###### 143.7358
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$balance,horizontal = TRUE)
hist(bank$balance)
barplot(bank$balance)
#qqplot
qqnorm(bank$balance)
qqline(bank$balance)
sqrtbal<-sqrt(bank$balance)
qqnorm(sqrtbal)
qqline(sqrtbal) 
pnorm(bank$balance,-1362.272,3044.766)
bank$balance <- as.factor(bank$balance)
-----------------------------------------------------------------------    
#Measures of Central Tendency                day
mean(bank$day)  ### -15.80642 
median(bank$day)##### 16
#mode
getmode(bank$day)  ######  20
#Measures of Dispersion
var(bank$day)  ###### 69.26361
sd(bank$day)  #### 8.322476
range(bank$day)##### 1  31
rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$day) #### 30
#Measures of skewness
skewness(bank$day)  ### 0.09307593
#Measures of Kurtosis 
kurtosis(bank$day)###### 1.940087
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$day,horizontal = TRUE)
hist(bank$day)
barplot(bank$day)
str(bank)
#qqplot
qqnorm(bank$day)
qqline(bank$day)
expday<-exp(bank$day)
qqnorm(expday)
qqline(expday)  ###slightly normalisation achieved
logday<-log(bank$day)
qqnorm(logday)
qqline(logday)
sqrday<- bank$day * bank$day
qqnorm(sqrday)
sqrtday<-sqrt(bank$day)
qqnorm(sqrtday)
qqline(sqrtday)
recday<-(1/bank$day)
qqnorm(recday)
qqline(recday)
rec2day<-(1/(bank$day * bank$day))
qqnorm(rec2day)
qqline(rec2day)
rec4day<-(1/(bank$day * bank$day)) * (1/(bank$day * bank$day))
qqnorm(rec4day)
qqline(rec4day)
rec8day<- (1/(bank$day * bank$day)) * (1/(bank$day * bank$day))* (1/(bank$day * bank$day)) * (1/(bank$day * bank$day))
qqnorm(rec8day)
qqline(rec8day)
pnorm(bank$day,-15.80642,8.322476)
bank$day <- as.factor(bank$day)
------------------------------------------------------------------
#Measures of Central Tendency                duration
mean(bank$duration)  ### 258.1631 
median(bank$duration)##### 180
#mode
getmode(bank$duration)  ###### 124
#Measures of Dispersion
var(bank$duration)  ###### 66320.57
sd(bank$duration)  #### 257.5278
range(bank$duration)##### 0  4918
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$duration) #### 4918
#Measures of skewness
skewness(bank$duration)  ### 3.144109
#Measures of Kurtosis 
kurtosis(bank$duration)###### 18.15084
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$duration,horizontal = TRUE)
hist(bank$duration)
barplot(bank$duration)
##str(affair)
#qqplot
qqnorm(bank$duration)
qqline(bank$duration)
sqrtdu<-sqrt(bank$duration)
qqnorm(sqrtdu)
qqline(sqrtdu)
sqrdu<- bank$duration * bank$duration
qqnorm(sqrdu)
qqline(sqrdu) #### Normalisation achieved
pnorm(bank$duration,258.1631,257.5278)
bank$duration <- as.factor(bank$duration)
-------------------------------------------------------------------------------
#Measures of Central Tendency                campaign
mean(bank$campaign)  ### -2.763841 
median(bank$campaign)##### 2
#mode
getmode(bank$campaign)  ###### 1
#Measures of Dispersion
var(bank$campaign)  ###### 9.597733
sd(bank$campaign)  #### 3.098021
range(bank$campaign)##### 1  63
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$campaign) #### 62
#Measures of skewness
skewness(bank$campaign)  ### 4.898325
#Measures of Kurtosis 
kurtosis(bank$campaign)###### 39.24331
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$campaign,horizontal = TRUE)
hist(bank$campaign)
barplot(bank$campaign)
#qqplot
qqnorm(bank$campaign)
qqline(bank$campaign)
expcam<-exp(bank$campaign)
qqnorm(expcam)
qqline(expcam)#####Normalisation achieved
pnorm(bank$campaign,-2.763841,3.098021)
bank$campaign <- as.factor(bank$campaign)
-----------------------------------------------------------------------------
#Measures of Central Tendency                pdays
mean(bank$pdays)  ### -40.19783 
median(bank$pdays)##### -1
#mode
getmode(bank$pdays)  ###### -1
#Measures of Dispersion
var(bank$pdays)  ###### 10025.77
sd(bank$pdays)  #### 100.1287
range(bank$pdays)##### -1  871
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$pdays) #### 872
#Measures of skewness
skewness(bank$pdays)  ### 2.615542
#Measures of Kurtosis 
kurtosis(bank$pdays)###### 6.933856
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$pdays,horizontal = TRUE)
hist(bank$pdays)
barplot(bank$pdays)
##str(affair)
#qqplot
qqnorm(bank$pdays)
qqline(bank$pdays)
sqrday<-(bank$pdays * bank$pdays)
qqnorm(sqrday)
qqline(sqrday)
recday<-(1/bank$pdays)
qqnorm(recday)
qqline(recday)
rec2day<-(1/(bank$pdays * bank$pdays))
qqnorm(rec2day)
qqline(rec2day)
rec4day<-(1/(bank$pdays * bank$pdays)) * (1/(bank$pdays * bank$pdays))
qqnorm(rec4day)
qqline(rec4day)
pnorm(bank$pdays,-40.19783,100.1287)
bank$pdays <- as.factor(bank$pdays)
------------------------------------------------------------------
#Measures of Central Tendency                previous
mean(bank$previous)  ### 0.5803234 
median(bank$previous)##### 0
#mode
getmode(bank$previous)  ###### 0
#Measures of Dispersion
var(bank$previous)  ###### 5.305841
sd(bank$previous)  #### 2.303441
range(bank$previous)##### 0  275
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(bank$previous) #### 275
#Measures of skewness
skewness(bank$previous)  ### 41.84368
#Measures of Kurtosis 
kurtosis(bank$previous)###### 4506.163
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(bank$previous,horizontal = TRUE)
hist(bank$previous)
barplot(bank$previous)
##str(affair)
#qqplot
qqnorm(bank$previous)
qqline(bank$previous)
sqrtpre<-sqrt(bank$previous)
qqnorm(sqrtpre)
qqline(sqrtpre)
sqrpre<-(bank$previous * bank$previous)
qqnorm(sqrpre)
qqline(sqrpre) ####Normalisation achieved
pnorm(bank$previous,0.5803234,2.303441)
bank$previous <- as.factor(bank$previous)
---------------------------------------------------------------------------
# Preparing a linear regression 
mod_lm <- lm(y~.,data=bank)
summary(mod_lm)
pred1 <- predict(mod_lm,bank)
pred1
plot(bank$y,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)
# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exclude those values 
# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
bank$job<-as.factor(bank$job)
bank$marital<-as.factor(bank$marital)
bank$education<-as.factor(bank$education)
bank$default<-as.factor(bank$default)
bank$housing<-as.factor(bank$housing)
bank$loan<-as.factor(bank$loan)
bank$contact<-as.factor(bank$contact)
bank$month<-as.factor(bank$month)
bank$poutcome<-as.factor(bank$poutcome)
bank$y<-as.factor(bank$y)
model <- glm(y~.,data=bank,family = "binomial")
View(model)
plot(model)
str(bank)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,affair,type="response")
summary(prob)
# We are going to use NULL and Residual Deviance to compare between different models
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank$y)
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
for(i in seq_along(bank)){
  bank[[i]] <- as.data.frame(bank[[i]])
}
##str(bank)
bank[,"job"] <- job
bank[,"marital"] <- marital
bank[,"education"] <- education
bank[,"default"] <- default
bank[,"housing"] <- housing
bank[,"loan"] <- loan
bank[,"contact"] <- contact
bank[,"month"] <- month
bank[,"poutcome"] <- poutcome
bank[,"y"] <- y
View(bank[,c(1:17)])
View(bank)
length(bank$age)
length(bank$pred_values)
na.omit(bank)
table(bank$y,bank$pred_values)
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
