library(forecast)
library(fpp)
library(smooth)
library(tseries)
# Loading Cocacola Data
library(readxl)
Cocacola<-read_xlsx("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Forecasting\\CocaCola_Sales_Rawdata.xlsx") # Aviation.csv
View(Cocacola)
attach(Cocacola)
sum(is.na(Cocacola))  ###0
Cocacola<- na.omit(Cocacola) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Cocacola)### 42 2
colnames(Cocacola) ####"Quarter","Sales"
-------------------------------------------------
#Measures of Central Tendency                Sales
mean(Cocacola$Sales)  ### -2994.353
median(Cocacola$Sales)##### 2782.377
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Cocacola$Sales)  ###### 1734.827
#Measures of Dispersion
var(Cocacola$Sales)  ###### 956348.8
sd(Cocacola$Sales)  ####  977.9309
range(Cocacola$Sales)#####  1547.819   5253.000
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Cocacola$Sales) ##### 3705.181
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Cocacola$Sales)  #### 0.6074295
#Measures of Kurtosis 
kurtosis(Cocacola$Sales)######  2.34338
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Cocacola$Sales,horizontal = TRUE)
hist(Cocacola$Sales)
barplot(Cocacola$Sales)
str(Cocacola)
#qqplot
qqnorm(Cocacola$Sales)
qqline(Cocacola$Sales)
logSales<-log(Cocacola$Sales)     ####Normalisation achieved
qqnorm(logSales)
qqline(logSales)
install.packages(psych)
library(psych)
describe(Cocacola)
pnorm(Cocacola$Sales,-2994.353,977.9309)

windows()
plot(Cocacola$Sales,type="o")
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')
#Creating 4 dummy variables 
CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)
CocacolaData["t"]<- 1:42
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)
View(CocacolaData)
train<-CocacolaData[1:38,]
test<-CocacolaData[39:42,]
########################### LINEAR MODEL #############################
linear_model<-lm(Sales~t,data=train)
summary(linear_model) ###Multiple R-squared:0.8117, Adjusted R-squared:0.8065
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 591.5533

######################### Exponential #################################
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model) ####Multiple R-squared:0.8291, Adjusted R-squared:0.8243
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
View(expo_pred)
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo  #466.248

######################### Quadratic ####################################
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model) ####Multiple R-squared:0.8799, Adjusted R-squared:0.873
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
View(Quad_pred)
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad  #475.5618

######################### Additive Seasonality #########################
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model) ###Multiple R-squared:0.09682, Adjusted R-squared:0.01712
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1860.024

######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model) ###Multiple R-squared:0.8978, Adjusted R-squared:0.8855
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear   #464.9829

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model) ####Multiple R-squared:0.9676 , Adjusted R-squared:0.9625
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad ###301.738

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model) ###Multiple R-squared: 0.1041 Adjusted R-squared: 0.025
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #1963.39

######################## Multiplicative Seasonality Linear trend ###########
multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) ###Multiple R-squared: 0.9216, Adjusted R-squared: 0.9121
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea#225.5244

######################## Multiplicative Seasonality Quadratic trend ###########
multi_quad_sea_model<-lm(log_Sales~t+t_square+Q1+Q2+Q3+Q4,data = train)
summary(multi_quad_sea_model) ###Multiple R-squared: 0.9543, Adjusted R-squared: 0.9471
multi_quad_sea_pred<-data.frame(predict(multi_quad_sea_model,newdata=test,interval='predict'))
rmse_multi_quad_sea<-sqrt(mean((test$Sales-exp(multi_quad_sea_pred$fit))^2,na.rm = T))
rmse_multi_quad_sea  ####581.8457

############################ Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea","rmse_multi_quad_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea,rmse_multi_quad_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative seasonality with Linear has least RMSE value
new_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values
View(new_model_fin)
Quarter <- as.data.frame(CocacolaData$Quarter)
Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")
View(Final)
resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 
k <- arima(resid, order=c(1,0,0))
str(k)
View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 4)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(Cocacoladata,file="Cocacoladata.csv",col.names = F,row.names = F)
getwd()
####################### Predicting new data #############################
library(readxl)
test_data<-read_excel("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Forecasting\\CocaCola_Sales_Rawdata.xlsx",1)
View(test_data)
test_data<-as.data.frame(test_data)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit + pred_res$pred
View(pred_new)

# Converting data into time series object
amts<-ts(Cocacola$Sales,frequency = 4,start=c(86))
View(amts)
?ts()
# dividing entire data into training and testing data 
train<-amts[1:38]
test<-amts[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data
# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
# Plotting time series data
plot(amts) # Visualization shows that it has level, trend, seasonality => Additive seasonality
#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a  ####Coefficients(a) - 4020.406
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100   ###16.12634
# Now take alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab  #####Coefficients:- a - 4304.53578, b- 89.43012
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100  ###8.928086
# Now assuming alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100 ###3.549841

# With out optimum values - Keep only alpha
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred  ###Coefficients:-4456.709 , alpha:0.502
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100  #####9.093032

###Keep alpha and beta
hw_nab<-HoltWinters(train,gamma=F)
hw_nab #### alpha - 0.5747386, beta - 0.3105725, Coefficients:- 4581.1447, 182.7749
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100  ###8.62752
####Keep alpha,beta and gamma
hw_nabg<-HoltWinters(train)
hw_nabg ###alpha - 0.3784328, beta-0.2526015, gamma-0.8897278, Coefficients:-a-4200.72210,
#### b-118.93562, s1-556.79856,s2-13.14018,s3:- -204.24618, s4-732.44912
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100  ###2.397211
############################## STOP HERE ###############################
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma
new_model <- HoltWinters(amts)
new_model
plot(forecast(new_model,n.ahead=4))
# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=4))
########################################################################
############## Now use ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 
ses_a<-ses(train,alpha = 0.2) 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100   ####16.1243

# Consider alpha = 0.2, beta = 0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100  ###8.498967

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100  ####4.409854
# With out optimum values 
# Lets check simple exponential method
ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100  ###9.132635

# Check Holts winter method 
holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100  ####8.927388

# Holts winter Exponential method
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100 ### 3.826562

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 
new_model <- hw(amts,alpha = NULL,beta = NULL,gamma = NULL)
plot(forecast(new_model,h=4))
# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))
