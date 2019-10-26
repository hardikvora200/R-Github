Startup <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Neural Network\\50_Startups.csv")
View(Startup)
attach(Startup)
sum(is.na(Startup))  ##0
Startup <- na.omit(Startup) # Omitting NA values from the Data if it is there
dim(Startup)### 50  5
colnames(Startup)
set.seed(50)
library(class)
-------------------------------------------------------------------------------
#Measures of Central Tendency                R.D. Spend
mean(Startup$R.D.Spend)  ### -73721.62
median(Startup$R.D.Spend)##### 73051.08
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Startup$R.D.Spend)  ###### 0
#Measures of Dispersion
var(Startup$R.D.Spend)  ###### 2107017150
sd(Startup$R.D.Spend)  #### 45902.26
range(Startup$R.D.Spend)#####  0.0  165349.2
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Startup$R.D.Spend) ##### 165349.2
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Startup$R.D.Spend)  #### 0.1590405
#Measures of Kurtosis 
kurtosis(Startup$R.D.Spend)######  2.194932
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Startup$R.D.Spend,horizontal = TRUE)
hist(Startup$R.D.Spend)
barplot(Startup$R.D.Spend)
str(Startup)
#qqplot
qqnorm(Startup$R.D.Spend)
qqline(Startup$R.D.Spend)
library(psych)
describe(Startup)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(Startup$R.D.Spend,-73721.62,45902.26)
-----------------------------------------------------------------------
#Measures of Central Tendency               #####Administration
mean(Startup$Administration)  ### -121344.6
median(Startup$Administration)##### 122699.8
#mode
getmode(Startup$Administration)  #####  136897.8
#Measures of Dispersion
var(Startup$Administration)  ######  784997271
sd(Startup$Administration)  ####  28017.8
range(Startup$Administration) ##### 51283.14 182645.56
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Startup$Administration) ##### 131362.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Startup$Administration)  #### -0.4742301
#Measures of Kurtosis 
kurtosis(Startup$Administration)######  3.085538
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Startup$Administration,horizontal = TRUE)
hist(Startup$Administration)
barplot(Startup$Administration)
#qqplot
qqnorm(Startup$Administration)
qqline(Startup$Administration)
adm<-sqrt(Startup$Administration)
qqnorm(adm)
qqline(adm)
adma<-((Startup$Administration) * (Startup$Administration)) + (Startup$Administration)
qqnorm(adma)
qqline(adma)
pnorm(Startup$Administration,-121344.6,28017.8)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Marketing Spend
mean(Startup$Marketing.Spend)  ### -211025.1
median(Startup$Marketing.Spend)  ##### 212716.2
#mode
getmode(Startup$Marketing.Spend)  ##### 0
#Measures of Dispersion
var(Startup$Marketing.Spend)  ###### 14954920097
sd(Startup$Marketing.Spend)  #### 122290.3
range(Startup$Marketing.Spend) #####  0.0 471784.1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Startup$Marketing.Spend) ##### 471784.1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Startup$Marketing.Spend)  #### -0.04506632
#Measures of Kurtosis 
kurtosis(Startup$Marketing.Spend)  ###### 2.275967
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Startup$Marketing.Spend,horizontal = TRUE)
hist(Startup$Marketing.Spend)
barplot(Startup$Marketing.Spend)
#qqplot
qqnorm(Startup$Marketing.Spend)
qqline(Startup$Marketing.Spend)
mar<-sqrt(Startup$Marketing.Spend)
qqnorm(mar)
qqline(mar)
marl<-exp(Startup$Marketing.Spend)
qqnorm(marl)
qqline(marl)
marec<-Startup$Marketing.Spend ^ (1/3)
qqnorm(marec)
qqline(marec)
pnorm(Startup$Marketing.Spend,-211025.1,122290.3)
------------------------------------------------------------------
#Measures of Central Tendency                Profit
mean(Startup$Profit)  ##### 112012.6
median(Startup$Profit)##### 107978.2
#mode
getmode(Startup$Profit)  ##### 192261.8
#Measures of Dispersion
var(Startup$Profit)  ###### 1624588173
sd(Startup$Profit)  #### 40306.18
range(Startup$Profit) ##### 14681.4 192261.8
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Startup$Profit) ##### 177580.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Startup$Profit)  #### 0.02258638
#Measures of Kurtosis 
kurtosis(Startup$Profit)###### 2.824704
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Startup$Profit,horizontal = TRUE)
hist(Startup$Profit)
barplot(Startup$Profit)
#qqplot
qqnorm(Startup$Profit)
qqline(Startup$Profit)
logProfit<-log(Startup$Profit)
qqnorm(logProfit)
sqrtProfit<-sqrt(Startup$Profit)
qqnorm(sqrtProfit)
qqline(sqrtProfit)
pnorm(Startup$Profit,112012.6,40306.18)
-------------------------------------------------------------
# Using multilayered feed forward nueral network
# package neuralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 
library(plyr)
Startup$State <- as.numeric(revalue(Startup$State,c("New York"="0","California"="1","Florida"="2")))
str(Startup)                                   
Startup<-as.data.frame(Startup)
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startup)
# Correlation coefficient - Strength & Direction of correlation
cor(Startup)
summary(Startup)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}  
Startup_norm<-as.data.frame(lapply(Startup,FUN=normalize))
View(Startup_norm)
summary(Startup_norm)
##Startup_norm <- cbind(Startup_norm,Startup$State)
##colnames(Startup_norm)[4] <- "State"
Startup_train<-Startup_norm[1:35,]
Startup_test<-Startup_norm[36:50,]
# Building model
Startup_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startup_train)
str(Startup_model)
windows()
plot(Startup_model, rep = "best") ###Error:- 0.03103, Steps:- 266
summary(Startup_model)
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(Startup_model, alpha = 0.6)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(Startup_model,Startup_test[1:4])
str(model_results)
predicted_profit <- model_results$net.result
# predicted_strength model_results$neurons
# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,Startup_test$Profit)
plot(predicted_profit,Startup_test$Profit)
# Since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(Startup$Profit)
str_min <- min(Startup$Profit)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
####Improve the model performance
set.seed(12345)
Startup_model2 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = Startup_train,hidden = 4)

plot(Startup_model2 ,rep = "best") ####Error:- 0.02774, Steps:-64
summary(Startup_model2)
model_results2<-compute(Startup_model2,Startup_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,Startup_test$Profit)
plot(predicted_Profit2,Startup_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
par(mar = numeric(4), family = 'serif')
plotnet(Startup_model2, alpha = 0.7)
####Cross validation of neural network model
# install relevant libraries
install.packages("boot")
install.packages("plyr")
# Load libraries
library(boot)
library(plyr)
# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL
List = list( )
## Scale data for neural network
?apply()
max = apply(Startup , 2 , max)
min = apply(Startup, 2 , min)
scaled = as.data.frame(scale(Startup, center = min, scale = max - min))
# Fit neural network model within nested for loop
library(neuralnet)
for(j in 1:35){
  for (i in 1:k) {
    index = sample(1:nrow(Startup),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = Startup[-index,]
    
    NN = neuralnet(Profit ~ ., trainNN, hidden = 4, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:4)])
    predict_testNN = (predict_testNN$net.result*(max(Startup$Profit)-min(Startup$Profit)))+min(Startup$Profit)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)
## Prepare boxplot
boxplot(Matrix.RMSE[,34], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 34)")
## Variation of median RMSE 
install.packages("matrixStats")
library(matrixStats)
med = colMedians(Matrix.RMSE)
X = seq(1,35)
plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
