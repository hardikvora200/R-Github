library(graphics)
Startup<-read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Multilinear Regression\\50Startups.csv")
View(Startup)
attach(Startup)
summary(Startup)
###df_main[setdiff(names(df_main),keep.vec)]  
Startup<-Startup[c("R.D.Spend","Administration","Marketing.Spend","Profit")]
summary(Startup)
mean(Startup$R.D.Spend)##73721.62-------------R.D.Spend
median(Startup$R.D.Spend)##73051.08
var(Startup$R.D.Spend)### 2107017150
sd(Startup$R.D.Spend)###45902.26
library(moments)
skewness(Startup$R.D.Spend)###0.1590405
kurtosis(Startup$R.D.Spend)###2.194932
---------------------------------------------------------
  mean(Startup$Administration)##121344.6-------------Administration
median(Startup$Administration)##122699.8
var(Startup$Administration)### 784997271
sd(Startup$Administration)###28017.8
skewness(Startup$Administration)### -0.4742301
kurtosis(Startup$Administration)### 3.085538
----------------------------------------------------------
  mean(Startup$Marketing.Spend)##211025.1-------------Marketing_Spend
median(Startup$Marketing.Spend)##212716.2
var(Startup$Marketing.Spend)### 14954920097
sd(Startup$Marketing.Spend)###122290.3
skewness(Startup$Marketing.Spend)### -0.04506632
kurtosis(Startup$Marketing.Spend)### 2.275967
-----------------------------------------------------------
  mean(Startup$Profit)##112012.6-------------Profit
median(Startup$Profit)##107978.2
var(Startup$Profit)### 1624588173
sd(Startup$Profit)###40306.18
skewness(Startup$Profit)### 0.02258638
kurtosis(Startup$Profit)### 2.824704
-----------------------------------------------------------
  barplot(Startup$R.D.Spend)  
barplot(Startup$Administration)  
barplot(Startup$Marketing.Spend)  
barplot(Startup$Profit)  
------------------------------------------------
  boxplot(Startup$R.D.Spend)  
boxplot(Startup$Administration)  
boxplot(Startup$Marketing.Spend)  
boxplot(Startup$Profit) 
-----------------------------------------------
  hist(Startup$R.D.Spend)  
hist(Startup$Administration)  
hist(Startup$Marketing.Spend)  
hist(Startup$Profit) 
--------------------------------------------
  pnorm(Startup$R.D.Spend,73721.62,45902.26)  
pnorm(Startup$Administration, 121344.6,28017.8)
pnorm(Startup$Marketing.Spend,211025.1, 122290.3)
pnorm(Startup$Profit,112012.6, 40306.18)
-------------------------------------------------------
  ##df_main[setdiff(names(df_main),keep.vec)]  
  qqnorm(Startup$R.D.Spend)
qqline(Startup$R.D.Spend)
----------------------------------
qqnorm(Startup$Administration)
qqline(Startup$Administration)
adm<-sqrt(Startup$Administration)
qqnorm(adm)
qqline(adm)
adma<-((Startup$Administration) * (Startup$Administration)) + (Startup$Administration)
qqnorm(adma)
qqline(adma)
---------------------------------------------
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
-------------------------------
qqnorm(Startup$Profit)  
qqline(Startup$Profit)
----------------------------------------------
  ########Normalization completed
  -------------------------------------
qqplot(Startup$R.D.Spend,Startup$Profit)
qqplot(Startup$Administration, Startup$Profit)
qqplot(Startup$Marketing.Spend, Startup$Profit)
----------------------------------------------------
  #############scatter Diagram and Calculation of Correlation
  ----------------------------------------
plot(Startup$R.D.Spend,Startup$Profit)
cor(Startup$R.D.Spend,Startup$Profit)###0.9729005
plot(Startup$Administration, Startup$Profit)
cor(Startup$Administration, Startup$Profit)###0.2007166
plot(Startup$Marketing.Spend, Startup$Profit)
cor(Startup$Marketing.Spend, Startup$Profit)#####0.7477657
---------------------------------------------------
  ######Compute Correlation matrix and Partial Correlation matrix, Covariance matrix
  ---------------------------------------
pairs(Startup)####All scatter diagrams in one matrix like plot
cor(Startup)###Gives us a matrix like format
library(corpcor)
cor2pcor(cor(Startup))
cov(Startup)
-----------------------------------------------------
  #######Create Normalization function#####
normalize<-function(x)
{
  return((x-min(x))/max(x))-min(x)
}
#####test normalization function
normalize(c(Startup$R.D.Spend,Startup$Administration,Startup$Marketing.Spend,Startup$Profit))
View(normalize(Startup$R.D.Spend))
View(normalize(Startup$Administration))
View(normalize(Startup$Marketing.Spend))
View(normalize(Startup$Profit))
###Now normalize the whole data
Startup<-as.data.frame(lapply(Startup,normalize))
View(Startup)##############Got normalized data
#######Dividing into Training Data and Test Data
---------------------------------------------------------
Startup_train<-Startup[1:30,] 
View(Startup_train)
Startup_test<-Startup[31:50,] 
View(Startup_test)
------------------------------------------------
  #######Now plot the model 
  ----------------------------------------------
  cor2pcor(cor(Startup_train))
model.Startup_train <- lm(Startup$Profit ~ (Startup$R.D.Spend + Startup$Administration + Startup$Marketing.Spend))
summary(model.Startup_train)
### We got from summary(model.Corolla_train) that except Startup$Administration and Startup$MarketingSpend all are significant
#Multiple R Squared - 0.9507,Adjusted R-squared- 0.9475,
####Again doing transformation of Startup$Administration and Startup$Marketing.Spend
model.Startup_train1 <- lm(Startup$Profit ~ (Startup$R.D.Spend + ((Startup$Administration*Startup$Administration)+
                                                                    Startup$Administration) + exp(Startup$Marketing.Spend)))
summary(model.Startup_train1)
##After normalising Corolla$cc, our Multiple R-squared is 0.9504, Adjusted r-squared is 0.9471
###Still Startup$Administration and Startup$ are not significant

####Lets apply this to Corolla.test data
model.Startup_test <- lm(Startup$Profit ~ (Startup$R.D.Spend + ((Startup$Administration*Startup$Administration)+Startup$Administration) + exp(Startup$Marketing.Spend)))
summary(model.Startup_test)
###From model.Corolla_test in summary we got Multiple R-squared to 0.9504 and adjusted R-squared to 0.9471 

##Scatter plot matrix with correlations inserted in graph
panel.smooth<-function(x, ...)
{
  usr <- par("usr");on.exit(par(usr))
  par(usr=c(usr[1:2],0,5.5))
  h<-hist(x,plot=FALSE)
  breaks<-h$breaks; nB<-length(breaks)
  y<-h$counts; y<- y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col="grey90",..)
}
panel.cor<-function(x,y,digits=2,prefix = "",cex.cor,..)
{
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<-format(c(r,0.123456789),digits=digits[1])
  txt<-paste(prefix,txt,sep="")
  if(missing(cex.cor))cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
par(no.readonly=FALSE)
on.exit(expr=NULL, add= FALSE, after = TRUE)
##summary(panel.corolla(scatterplot(model.Corolla)))
pairs(Startup_train,upper.panel = panel.cor, main="Scatter plot matrix with correlation 
      coefficient clients")
pairs(Startup_test,upper.panel = panel.cor, main="Scatter plot matrix with correlation 
      coefficient clients")
###########Predict the model
pred1<-predict(model.Startup_train)
summary(pred1)####Min - 0.1654, 1st Qu.-0.3660, Median-0.5061, Mean-0.5062, 3rd Qu.-0.6287, Max-0.9250
pred2<-predict(model.Startup_test)
summary(pred2)###Min-0.1701,1st Qu.-0.3645,Median-0.5055...
#################################Checking normality of residuals
qqnorm(model.Startup_train$residuals)
qqline(model.Startup_train$residuals)#####most of the residuals are in line i.e.normal
qqnorm(model.Startup_test$residuals)
qqline(model.Startup_test$residuals)#####most of the residuals are in line i.e.normal
sqrt(mean(model.Startup_train$residuals^2))#####0.04605878
sqrt(mean(model.Startup_test$residuals^2))####0.04622923
###########Checking Confidence interval
confint(model.Startup_train,level=0.95)
confint(model.Startup_test,level=0.95)
#####Predict the data with interval of confidence and also prediction
predict(model.Startup_train, interval="confidence")
predict(model.Startup_test, interval="confidence")
predict(model.Startup_train, interval = "prediction")
predict(model.Startup_test, interval="prediction")
####Draw ggplot
##Startup_train$data<-unlist(Startup_train$data)
library(ggplot2)
ggplot(data=Startup_train, aes(x=Startup_train$R.D.Spend + ((Startup_train$Administration*Startup_train$Administration)+ Startup_train$Administration) + exp(Startup_train$Marketing.Spend)), y=pred1)+
  geom_point(color='blue') +geom_line(color='red', data=Startup_train,aes(x=(Startup_train$R.D.Spend + ((Startup_train$Administration*Startup_train$Administration)+Startup_train$Administration) + exp(Startup_train$Marketing.Spend)), y=pred1))
#line
ggplot(data=Startup_train,aes(Startup_train$R.D.Spend + ((Startup_train$Administration*Startup_train$Administration)+ 
                                                           Startup_train$Administration) + exp(Startup_train$Marketing.Spend),Startup_train$Profit))+stat_summary(fun.args = list(),fun.data = mean_cl_normal) +
  geom_smooth(method='lm')
ggplot(data=Startup_test, aes(x=Startup_test$R.D.Spend + ((Startup_test$Administration*Startup_test$Administration)+ Startup_test$Administration) + exp(Startup_test$Marketing.Spend)), y=pred1)+
  geom_point(color='blue') +geom_line(color='red', data=Startup_test,aes(x=(Startup_test$R.D.Spend + ((Startup_test$Administration*Startup_test$Administration)+Startup_test$Administration) + exp(Startup_test$Marketing.Spend)), y=pred1))
#line
ggplot(data=Startup_test,aes(Startup_test$R.D.Spend + ((Startup_test$Administration*Startup_test$Administration)+ 
                                                           Startup_test$Administration) + exp(Startup_test$Marketing.Spend),Startup_test$Profit))+stat_summary(fun.args = list(),fun.data = mean_cl_normal) +
  geom_smooth(method='lm')
?geom_point
library(influence.ME)
##Deletion Diagnostics for identifying influential variables
influence.measures(model.Startup_test)
infIndexPlot:influenceIndexPlot(model.Startup_test,id.n=6)##Indexplot of the influence 
###measures
influencePlot(model.Startup_test,id.n=6)
------------------------------------------
##Let us try to delete 15th,47th,49th,50th observations
model.Startup_train2<-lm(Startup_train$Profit ~ (Startup_train$R.D.Spend + 
((Startup_train$Administration*Startup_train$Administration)+Startup_train$Administration)+exp(Startup_train$Marketing.Spend)),data = Startup[-c(15,47,49,50),])

summary(model.Startup_train2)
##Variance Inflation factor is a formal way to check for collinearity and Doors is not 
##seems to be significant
vif(model.Startup_train2)
##plot(Corolla$Doors,Corolla$Price,col="dodgerblue4", pch=20)
influenceIndexPlot(model.Startup_train2,id.n=6)
influencePlot(model.Startup_train2,id.n=6)
###Now removing 2,7,15,16,28,47,49,50

model.Startup_train3<-lm(exp((exp(Startup_train$Profit))) ~ (Startup_train$R.D.Spend) + (exp(exp(Startup_train$Administration)) *exp(exp(Startup_train$Administration)) *exp(exp(Startup_train$Administration))) +exp(exp(Startup_train$Marketing.Spend)),data = Startup[-c(1,2,3,7,12,15,16,28,29,47,48,49,50),])
summary(model.Startup_train3)
model.Startup_test<-lm(exp((exp(Startup_test$Profit))) ~ (Startup_test$R.D.Spend) + (exp(exp(Startup_test$Administration)) *exp(exp(Startup_test$Administration)) *exp(exp(Startup_test$Administration))) +exp(Startup_test$Marketing.Spend)), data = Startup[-c(1,2,3,7,12,15,16,28,29,47,48,49,50),])
summary(model.Startup_test)
layout(matrix(c(1,2,3,4),2,2))

avPlots(model.Startup_train3)


