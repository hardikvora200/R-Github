concrete <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Neural Network\\concrete.csv")
View(concrete)
attach(concrete)
sum(is.na(concrete))  ##0
concrete <- na.omit(concrete) # Omitting NA values from the Data if it is there
dim(concrete)### 1030  9
colnames(concrete)
set.seed(1030)
library(class)
-------------------------------------------------------------------------------
#Measures of Central Tendency                cement
mean(concrete$cement)  ### -281.1679
median(concrete$cement)##### 272.9
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(concrete$cement)  ###### 425
#Measures of Dispersion
var(concrete$cement)  ###### 10921.58
sd(concrete$cement)  #### 104.5064
range(concrete$cement)#####  102  540
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$cement) ##### 438
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(concrete$cement)  #### 0.5087389
#Measures of Kurtosis 
kurtosis(concrete$cement)######  2.476052
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(concrete$cement,horizontal = TRUE)
hist(concrete$cement)
barplot(concrete$cement)
str(concrete)
#qqplot
qqnorm(concrete$cement)
qqline(concrete$cement)
logcement<-log(concrete$cement)
qqnorm(logcement)
qqline(logcement)
sqrtcement<-sqrt(concrete$cement)
qqnorm(sqrtcement)
qqline(sqrtcement)
recement<-(1/(concrete$cement))
qqnorm(recement)
rec2cement<-(1/(concrete$cement)) * (1/(concrete$cement))
qqnorm(rec2cement)
rec4cement<-rec2cement * rec2cement
qqnorm(rec4cement)
qqline(rec4cement)
rec8cement<-rec4cement * rec4cement
qqnorm(rec8cement)
qqline(rec8cement)
library(psych)
describe(concrete)
pnorm(concrete$cement,-281.1679,104.5064)
-----------------------------------------------------------------------
#Measures of Central Tendency               ##### slag
mean(concrete$slag)  ### -73.89583
median(concrete$slag)##### 22
#mode
getmode(concrete$slag)  #####  0
#Measures of Dispersion
var(concrete$slag)  ######  7444.125
sd(concrete$slag)  ####  86.27934
range(concrete$slag) ##### 0.0   359.4
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$slag) ##### 359.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$slag)  ####  0.7995503
#Measures of Kurtosis 
kurtosis(concrete$slag)###### 2.488468
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$slag,horizontal = TRUE)
hist(concrete$slag)
barplot(concrete$slag)
#qqplot
qqnorm(concrete$slag)
qqline(concrete$slag)
sqrtslag<-sqrt(concrete$slag)
qqnorm(sqrtslag)
qqline(sqrtslag)
pnorm(concrete$slag,-73.89583,86.27934)
-----------------------------------------------------------------------    
#Measures of Central Tendency                ash
mean(concrete$ash)  ### -54.18835
median(concrete$ash)  ##### 0
#mode
getmode(concrete$ash)  ##### 0
#Measures of Dispersion
var(concrete$ash)  ###### 4095.617
sd(concrete$ash)  #### 63.997
range(concrete$ash) #####  0.0  200.
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$ash) ##### 200.1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$ash)  #### 0.536571
#Measures of Kurtosis 
kurtosis(concrete$ash)  ###### 1.671875
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$ash,horizontal = TRUE)
hist(concrete$ash)
barplot(concrete$ash)
#qqplot
qqnorm(concrete$ash)
qqline(concrete$ash)
sqrtash<-sqrt(concrete$ash)
qqnorm(sqrtash)
qqline(sqrtash)
expash<-exp(concrete$ash)
qqnorm(expash)
qqline(expash)
pnorm(concrete$ash,-54.18835,63.997)
------------------------------------------------------------------
#Measures of Central Tendency                water
mean(concrete$water)  ### 181.5673
median(concrete$water)  ##### 185
#mode
getmode(concrete$water)  ##### 192
#Measures of Dispersion
var(concrete$water)  ###### 456.0027
sd(concrete$water)  #### 21.35422
range(concrete$water) #####   121.8 247.0
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$water) ##### 125.2
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$water)  #### 0.07451966
#Measures of Kurtosis 
kurtosis(concrete$water)  ###### 3.11567
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$water,horizontal = TRUE)
hist(concrete$water)
barplot(concrete$water)
#qqplot
qqnorm(concrete$water)
qqline(concrete$water)
sqrtwater<-sqrt(concrete$water)
qqnorm(sqrtwater)
qqline(sqrtwater)
expwater<-exp(concrete$water)
qqnorm(expwater)
qqline(expwater)
pnorm(concrete$water,181.5673,21.35422)
-----------------------------------------------------------
#Measures of Central Tendency                superplastic
mean(concrete$superplastic)  ### -6.20466
median(concrete$superplastic)  ##### 6.4
#mode
getmode(concrete$superplastic)  ##### 0
#Measures of Dispersion
var(concrete$superplastic)  ###### 35.68678
sd(concrete$superplastic)  #### 5.973841
range(concrete$superplastic) #####  0.0 32.2
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$superplastic) ##### 32.2
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$superplastic)  #### 0.9058809
#Measures of Kurtosis 
kurtosis(concrete$superplastic)  ###### 4.398608
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$superplastic,horizontal = TRUE)
hist(concrete$superplastic)
barplot(concrete$superplastic)
#qqplot
qqnorm(concrete$superplastic)
qqline(concrete$superplastic)
sqrtsuplas<-sqrt(concrete$superplastic)
qqnorm(sqrtsuplas)
qqline(sqrtsuplas)
expsuplas<-exp(concrete$superplastic)
qqnorm(expsuplas)
qqline(expsuplas)
pnorm(concrete$superplastic,-6.20466,5.973841)
----------------------------------------------------------------
#Measures of Central Tendency                coarseegg
mean(concrete$coarseagg)  ### 972.9189
median(concrete$coarseagg)  ##### 968
#mode
getmode(concrete$coarseagg)  ##### 932
#Measures of Dispersion
var(concrete$coarseagg)  ###### 6045.677
sd(concrete$coarseagg)  #### 77.75395
range(concrete$coarseagg) #####  801 1145
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$coarseagg) ##### 344
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$coarseagg)  #### -0.04016115
#Measures of Kurtosis 
kurtosis(concrete$coarseagg)  ###### 2.398068
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$coarseagg,horizontal = TRUE)
hist(concrete$coarseagg)
barplot(concrete$coarseagg)
#qqplot
qqnorm(concrete$coarseagg)
qqline(concrete$coarseagg)
sqrtca<-sqrt(concrete$coarseagg)
qqnorm(sqrtca)
qqline(sqrtca)
logca<-log(concrete$coarseagg)
qqnorm(logca)
qqline(logca)
sqrtlogca<-sqrt(log(concrete$coarseagg))
qqnorm(sqrtlogca)
recca<-(1/(concrete$coarseagg))
qqnorm(recca)
qqline(recca)
rec2cca<-recca * recca
qqnorm(rec2cca)
qqline(rec2cca)
rec4cca<-rec2cca * rec2cca
qqnorm(rec4cca)
qqline(rec4cca)
rec8cca<-rec4cca * rec4cca
qqnorm(rec8cca)
rec16cca<-rec8cca * rec8cca
qqnorm(rec16cca)
rec32cca<-rec16cca * rec16cca
qqnorm(rec32cca)
qqline(rec32cca)
pnorm(concrete$coarseagg,972.9189,77.75395)
---------------------------------------------------------------
###Measures of Central Tendency                fineagg
mean(concrete$fineagg)  ### -773.5805
median(concrete$fineagg)  ##### 779.5
#mode
getmode(concrete$fineagg)  ##### 594
#Measures of Dispersion
var(concrete$fineagg)  ###### 6428.188
sd(concrete$fineagg)  #### 80.17598
range(concrete$fineagg) #####  594.0 992.6
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$fineagg) ##### 398.6
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$fineagg)  #### -0.252641
#Measures of Kurtosis 
kurtosis(concrete$fineagg)  ###### 2.892499
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$fineagg,horizontal = TRUE)
hist(concrete$fineagg)
barplot(concrete$fineagg)
#qqplot
qqnorm(concrete$fineagg)
qqline(concrete$fineagg)
sqrtfineagg<-sqrt(concrete$fineagg)
qqnorm(sqrtfineagg)
qqline(sqrtfineagg)
logfineagg<-log(concrete$fineagg)
qqnorm(logfineagg)
qqline(logfineagg)
recfineagg<-(1/(concrete$fineagg))
qqnorm(recfineagg)
qqline(recfineagg)
rec2fineagg<-recfineagg * recfineagg
qqnorm(rec2fineagg)
rec4fineagg<-rec2fineagg * rec2fineagg
qqnorm(rec4fineagg)
qqline(rec4fineagg)
rec8fineagg<-rec4fineagg * rec4fineagg
qqnorm(rec8fineagg)
rec16fineagg<-rec8fineagg * rec8fineagg
qqnorm(rec16fineagg)
rec32fineagg<-rec16fineagg * rec16fineagg
qqnorm(rec32fineagg)
qqline(rec32fineagg)
pnorm(concrete$fineagg,-773.5805,80.17598)
--------------------------------------------------------------
#Measures of Central Tendency                age
mean(concrete$age)  ### 45.66214
median(concrete$age)  ##### 28
#mode
getmode(concrete$age)  ##### 28
#Measures of Dispersion
var(concrete$age)  ###### 3990.438
sd(concrete$age)  #### 63.16991
range(concrete$age) #####  1  365
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$age) ##### 364
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$age)  #### 3.264415
#Measures of Kurtosis 
kurtosis(concrete$age)  ###### 15.10418
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$age,horizontal = TRUE)
hist(concrete$age)
barplot(concrete$age)
#qqplot
qqnorm(concrete$age)
qqline(concrete$age)
sqrtage<-sqrt(concrete$age)
qqnorm(sqrtage)
expage<-exp(concrete$age)
qqnorm(expage)
qqline(expage)
pnorm(concrete$age,45.66214,63.16991)
------------------------------------------------------------------------
#Measures of Central Tendency                strength
mean(concrete$strength)  ### 35.81796
median(concrete$strength)  ##### 34.445
#mode
getmode(concrete$strength)  ##### 33.4
#Measures of Dispersion
var(concrete$strength)  ###### 279.0818
sd(concrete$strength)  #### 16.70574
range(concrete$strength) #####  2.33 82.60
rangevalue <- function(x){max(x)-min(x)}
rangevalue(concrete$strength) ##### 80.27
#Measures of skewness
library(moments)
#Measures of skewness
skewness(concrete$strength)  #### 0.4163698
#Measures of Kurtosis 
kurtosis(concrete$strength)  ###### 2.681976
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(concrete$strength,horizontal = TRUE)
hist(concrete$strength)
barplot(concrete$strength)
#qqplot
qqnorm(concrete$strength)
qqline(concrete$strength)
sqrtstrength<-sqrt(concrete$strength)
qqnorm(sqrtstrength)
qqline(sqrtstrength)
pnorm(concrete$strength,35.81796,16.70574)
-------------------------------------------------------------
# Using multilayered feed forward nueral network
# package neuralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 
library(plyr)
####Startup$State <- as.numeric(revalue(Startup$State,c("New York"="0","California"="1","Florida"="2")))
###str(Startup)                                   
concrete<-as.data.frame(concrete)
plot(cement, strength)
plot(slag,strength)
plot(ash,strength)
plot(water, strength)
plot(superplastic,strength)
plot(coarseagg,strength)
plot(fineagg,strength)
plot(age,strength)
windows()
# Find the correlation between Output (strength) & inputs - SCATTER DIAGRAM
pairs(concrete)
# Correlation coefficient - Strength & Direction of correlation
cor(concrete)
summary(concrete)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}  
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
View(concrete_norm)
summary(concrete_norm)
##Startup_norm <- cbind(Startup_norm,Startup$State)
##colnames(Startup_norm)[4] <- "State"
concrete_train<-concrete_norm[1:800,]
concrete_test<-concrete_norm[801:1030,]
# Building model
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
windows()
plot(concrete_model, rep = "best") ###Error:- 5.217926, Steps:-3890
summary(concrete_model)
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
##set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength model_results$neurons
# Predicted strength Vs Actual strength of test data.
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
# Since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength
str_max <- max(concrete$strength)
str_min <- min(concrete$strength)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
Actualstrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(Actualstrength_pred)
####Improve the model performance
##set.seed(12345)
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train,hidden = 6)
windows()
plot(concrete_model2 ,rep = "best") ####Error:- 1.623624, Steps:-76985
summary(concrete_model2)
model_results2<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)  ##0.9326315
plot(predicted_strength2,concrete_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
par(mar = numeric(4), family = 'serif')
plotnet(strength_model2, alpha = 0.7)
####Cross validation of neural network model
# install relevant libraries
install.packages("boot")
install.packages("plyr")
# Load libraries
library(boot)
library(plyr)
# Initialize variables
set.seed(900)
k = 1000
RMSE.NN = NULL
List = list( )
## Scale data for neural network
?apply()
max = apply(concrete , 2 , max)
min = apply(concrete, 2 , min)
scaled = as.data.frame(scale(concrete, center = min, scale = max - min))
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$strength - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# Fit neural network model within nested for loop
library(neuralnet)
for(j in 1:800){
  for (i in 1:k) {
    ##index = sample(1:nrow(Forestfire),j )
    
    trainNN = scaled[1:800,]
    testNN = scaled[801:1030,]
    datatest = concrete[801:1030,]
    
    NN = neuralnet(strength ~ ., trainNN, hidden = 4, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:8)])
    predict_testNN = (predict_testNN$net.result*(max(concrete$strength)-min(concrete$strength)))+min(concrete$strength)
    
    RMSE.NN[i]<- (sum((datatest$strength - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)
## Prepare boxplot
boxplot(Matrix.RMSE[,349], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 34)")
## Variation of median RMSE 
install.packages("matrixStats")
library(matrixStats)
med = colMedians(Matrix.RMSE)
X = seq(2,350)
plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")


