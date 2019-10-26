Ailine<-readxl::read_xlsx("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\
\Forecasting\\Airlines+Data.xlsx") # read the Airline data
View(Airline) 
attach(Airline)
sum(is.na(Airline))
Airline<- na.omit(Airline) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Airline)### 96 2
colnames(Airline)
-------------------------------------------------
  #Measures of Central Tendency                Passengers
  mean(Airline$Passengers)  ### -213.7083
median(Airline$Passengers)##### 200
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Airline$Passengers)  ###### 229
#Measures of Dispersion
var(Airline$Passengers)  ###### 5172.23
sd(Airline$Passengers)  #### 71.91822
range(Airline$Passengers)##### 104  413
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Airline$Passengers) ##### 309
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Airline$Passengers)  #### 0.6273728
#Measures of Kurtosis 
kurtosis(Airline$Passengers)######  2.8166
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Airline$Passengers,horizontal = TRUE)
hist(Airline$Passengers)
barplot(Airline$Passengers)
str(Airline)
#qqplot
qqnorm(Airline$Passengers)
qqline(Airline$Passengers)
logPassengers<-log(Airline$Passengers)                              ####Normalisation 
achieved
qqnorm(logPassengers)
qqline(logPassengers)
install.packages(psych)
library(psych)
describe(Airline)
pnorm(Airline$Passengers,-213.7083,71.91822)
windows()
plot(Airline$Passengers,type="o")
#Creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )  ### Creating 
##dummies for 12 months
View(X)
?month.abb
colnames(X)<-month.abb # Assigning month names 
View(X)
Airlinedata<-cbind(Airline,X)
View(Airlinedata)
Airlinedata["t"]<- 1:96
View(Airlinedata)
Airlinedata["log_Passengers"]<-log(Airlinedata["Passengers"])
Airlinedata["t_square"]<-(Airlinedata["t"]) * (Airlinedata["t"])
attach(Airlinedata)
View(Airlinedata)
train<-Airlinedata[1:84,]
test<-Airlinedata[85:96,]
########################### LINEAR MODEL #############################
linear_model<-lm(Passengers~t,data=train)
summary(linear_model) ###Multiple R-squared:0.7923, Adjusted R-squared:0.7898
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924

######################### Exponential #################################
expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model) ####Multiple R-squared:0.8239, Adjusted R-squared:0.8218
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736

######################### Quadratic ####################################
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)  ####Multiple R-squared:0.7963, Adjusted R-squared:0.7912
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189

######################### Additive Seasonality #########################
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model) ###Multiple R-squared:0.1674, Adjusted R-squared:0.0415
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct
                         +Nov,data=train)
summary(Add_sea_Linear_model) ###Multiple R-squared:0.9551, Adjusted R-squared:0.9475
Add_sea_Linear_pred<-data.frame(predict
                                (Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct
                       +Nov,data=train)
summary(Add_sea_Quad_model) ####Multiple R-squared:0.9572 , Adjusted R-squared:0.9488
Add_sea_Quad_pred<-data.frame(predict
                              (Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = 
                      train)
summary(multi_sea_model) ###Multiple R-squared: 0.1943, Adjusted R-squared: 0.06402
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #140.0632

######################## Multiplicative Seasonality Linear trend ###########
multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct
                        +Nov,data = train)
summary(multi_add_sea_model) ###Multiple R-squared: 0.9763, Adjusted R-squared: 0.9723
multi_add_sea_pred<-data.frame(predict
                               (multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = 
                                T))
rmse_multi_add_sea #10.51917

######################## Multiplicative Seasonality Quadratic trend ###########
multi_sea_quad_model<-lm(log_Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep
                         +Oct+Nov,data = train)
summary(multi_sea_quad_model) ###Multiple R-squared: 0.9777, Adjusted R-squared: 0.9736
multi_sea_quad_pred<-data.frame(predict
                                (multi_sea_quad_model,newdata=test,interval='predict'))
rmse_multi_sea_quad<-sqrt(mean((test$Passengers-exp(multi_sea_quad_pred$fit))^2,na.rm = 
                                 T))
rmse_multi_sea_quad #18.37201

############################ Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_se
a","rmse_multi_add_sea","rmse_multi_sea_quad"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea,rmse_multi_sea_quad))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative seasonality with Linear has least RMSE value
new_model <- lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
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
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 10)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(Airlinedata,file="Airlinedata.csv",col.names = F,row.names = F)
getwd()
####################### Predicting new data #############################
library(readxl)
test_data<-read_excel("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\
\Forecasting\\Airlines+Data.xlsx",1)
View(test_data)
test_data<-as.data.frame(test_data)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit + pred_res$pred
View(pred_new)

# Converting data into time series object
amts<-ts(Airline$Passengers,frequency = 12,start=c(86))
View(amts)
?ts()
# dividing entire data into training and testing data 
train<-amts[1:86]
test<-amts[87:96]
# converting time series object
train<-ts(train,frequency = 10)
test<-ts(test,frequency = 10)
# Plotting time series data
plot(amts) # Visualization shows that it has level, trend, seasonality => Additive 
seasonality
#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a  ####Coefficients-282.3698
hwa_pred<-data.frame(predict(hw_a,n.ahead=10))
# By looking at the plot the forecasted values are not showing any characters of train 
###data 
plot(forecast(hw_a,h=10))
hwa_mape<-MAPE(hwa_pred$fit,test)*100   ### 20.72572
# Now take alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab  
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 10))
# by looking at the plot the forecasted values are still missing some characters 
##exhibited by train data
plot(forecast(hw_ab,h=10))
hwab_mape<-MAPE(hwab_pred$fit,test)*100  ###  14.11192
# Now assuming alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 10))
# by looking at the plot the characters of forecasted values are closely following 
##historical data
plot(forecast(hw_abg,h=10))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100 ### 14.58873

# With out optimum values - Keep only alpha
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 10))
hwna_pred  
plot(forecast(hw_na,h=10))
hwna_mape<-MAPE(hwna_pred$fit,test)*100  ##### 22.3825

###Keep alpha and beta
hw_nab<-HoltWinters(train,gamma=F)
hw_nab 
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=10))
hwnab_pred
plot(forecast(hw_nab,h=10))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100  ### 17.12434
####Keep alpha,beta and gamma
hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead=10))
hwnabg_pred
plot(forecast(hw_nabg,h=10))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100  ### 25.65041
############################## STOP HERE ###############################
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c
                    (hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
# Based on the MAPE value who choose holts winter exponential tecnique which assumes the 
##time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma
new_model <- HoltWinters(amts,alpha = 0.2,beta = 0.1,gamma = F)
new_model
plot(forecast(new_model,n.ahead=10))
# Forecasted values for the next 10 months
forecast_new <- data.frame(predict(new_model,n.ahead=10))
########################################################################
############## Now use ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 
ses_a<-ses(train,alpha = 0.2) 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=10))
plot(forecast(ses_a,n.ahead=10))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100   ####20.72572

# Consider alpha = 0.2, beta = 0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=10))
plot(forecast(holt_ab,h=10))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100  ### 40.28076

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 10))
plot(forecast(hw_abg_new,h=10))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100  #### 44.9797
# With out optimum values 
# Lets check simple exponential method
ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 10))
sesna_pred
plot(forecast(ses_na,h=10))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100  ### 22.38241

# Check Holts winter method 
holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=10))
holtnab_pred
plot(forecast(holt_nab,h=10))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100  #### 19.34602

# Holts winter Exponential method
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=10))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=10))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100 ### 20.34145

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new
"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the 
###time series
# Data level, trend, seasonality characters 
new_model <- holt(amts,alpha = NULL,beta = NULL)
plot(forecast(new_model,h=10))
# Forecasted values for the next 10 months
forecast_new <- data.frame(predict(new_model,h=10))
