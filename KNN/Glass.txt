Glass <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\KNN\\glass.csv")
View(Glass)
attach(Glass)
sum(is.na(Glass))
Glass <- na.omit(Glass) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Glass)### 214 10
colnames(Glass)
#####Load Data
set.seed(1)
library(class)
d = read.table("glass.csv", sep=",", header = FALSE)
d = data.frame(d)  
d
-------------------------------------------------
#Measures of Central Tendency                RI
mean(Glass$RI)  ### -1.518365
median(Glass$RI)##### 1.51768
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Glass$RI)  ###### 1.52152
#Measures of Dispersion
var(Glass$RI)  ###### 9.222541e-06
sd(Glass$RI)  #### 0.003036864
range(Glass$RI)##### 1.51115 1.53393
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$RI) ##### 0.02278
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Glass$RI)  #### 1.614015
#Measures of Kurtosis 
kurtosis(Glass$RI)######  7.789354
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Glass$RI,horizontal = TRUE)
hist(Glass$RI)
barplot(Glass$RI)
str(Glass)
#qqplot
qqnorm(Glass$RI)
qqline(Glass$RI)
expRI<-exp(Glass$RI)
qqnorm(expRI)
qqline(expRI)
sqrtRI<-sqrt(Glass$RI)
qqnorm(sqrtRI)
qqline(sqrtRI)
recRI<-(1/Glass$RI)
qqnorm(recRI)
qqline(recRI)
rec2RI<-recRI * recRI
algRI<-(Glass$RI * Glass$RI) + Glass$RI
qqnorm(algRI)
qqline(algRI)
qqnorm(rec2RI)
install.packages(psych)
library(psych)
describe(Glass)
pnorm(Glass$RI,-1.518365,0.003036864)
-----------------------------------------------------------------------
#Measures of Central Tendency                Na
mean(Glass$Na)  ### -13.40785
median(Glass$Na)##### 13.3
#mode
getmode(Glass$Na)  #####  13.21
#Measures of Dispersion
var(Glass$Na)  ###### 0.6668414
sd(Glass$Na)  ####  0.8166036
range(Glass$Na) ##### 10.73 17.38
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Na) ##### 6.65
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Na)  #### 0.4509917
#Measures of Kurtosis 
kurtosis(Glass$Na)###### 5.953477
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Na,horizontal = TRUE)
hist(Glass$Na)
barplot(Glass$Na)
#qqplot
qqnorm(Glass$Na)
qqline(Glass$Na)###Normalisation achieved
pnorm(Glass$Na,-13.40785,0.8166036)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Mg
mean(Glass$Mg)  ### -2.684533
median(Glass$Mg)##### 3.48
#mode
getmode(Glass$Mg)  ##### 0
#Measures of Dispersion
var(Glass$Mg)  ###### 2.08054
sd(Glass$Mg)  #### 1.442408
range(Glass$Mg) ##### 0.00 4.49
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Mg) ##### 4.49
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Mg)  #### -1.144465
#Measures of Kurtosis 
kurtosis(Glass$Mg)###### 2.571298
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Mg,horizontal = TRUE)
hist(Glass$Mg)
barplot(Glass$Mg)
#qqplot
qqnorm(Glass$Mg)
qqline(Glass$Mg)
expMg<-exp(Glass$Mg)
qqnorm(expMg)
sqrtMg<-sqrt(Glass$Mg)
qqnorm(sqrtMg)
algMg<-(Glass$Mg * Glass$Mg) + Glass$Mg
qqnorm(algMg)
qqline(algMg)
sqrMg<-(Glass$Mg * Glass$Mg)
qqnorm(sqrMg)
recexpMg<-(1/(exp(Glass$Mg)))
qqnorm(recexpMg)
qqline(recexpMg)
pnorm(Glass$Mg,-2.684533,1.442408)
------------------------------------------------------------------
#Measures of Central Tendency                Al
mean(Glass$Al)  ### 1.444907
median(Glass$Al)##### 1.36
#mode
getmode(Glass$Al)  ##### 1.54
#Measures of Dispersion
var(Glass$Al)  ###### 0.2492702
sd(Glass$Al)  #### 0.4992696
range(Glass$Al) ##### 0.29 3.50
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Al) ##### 3.21
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Al)  #### 0.9009179
#Measures of Kurtosis 
kurtosis(Glass$Al)###### 4.984832
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Al,horizontal = TRUE)
hist(Glass$Al)
barplot(Glass$Al)
#qqplot
qqnorm(Glass$Al)
qqline(Glass$Al)
logAl<-log(Glass$Al)
qqnorm(logAl)
qqline(logAl)
expAl<-exp(Glass$Al)
qqnorm(expAl)
qqline(expAl)
pnorm(Glass$Al,1.444907,0.4992696)
-------------------------------------------------------------------
#Measures of Central Tendency                airborne
mean(Glass$Si)  ### -72.65093
median(Glass$Si)##### 72.79
#mode
getmode(Glass$Si)  ##### 72.99
#Measures of Dispersion
var(Glass$Si)  ###### 0.5999212
sd(Glass$Si)  #### 0.7745458
range(Glass$Si) ##### 69.81 75.41
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Si) ##### 5.6
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Si)  #### -0.7253173
#Measures of Kurtosis 
kurtosis(Glass$Si)###### 5.871105
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Si,horizontal = TRUE)
hist(Glass$Si)
barplot(Glass$Si)
#qqplot
qqnorm(Glass$Si)
qqline(Glass$Si)
pnorm(Glass$Si,-72.65093,0.7745458)
-------------------------------------------------------------
#Measures of Central Tendency                K
mean(Glass$K)  ###  -0.4970561
median(Glass$K)##### 0.555
#mode
getmode(Glass$K)  ##### 0
#Measures of Dispersion
var(Glass$K)  ###### 0.4253542
sd(Glass$K)  #### 0.6521918
range(Glass$K) ##### 0.00   6.21
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$K) ##### 6.21
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$K)  #### 6.505636
#Measures of Kurtosis 
kurtosis(Glass$K)###### 56.39233
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$K,horizontal = TRUE)
hist(Glass$K)
barplot(Glass$K)
#qqplot
qqnorm(Glass$K)
qqline(Glass$K)
expK<-exp(Glass$K)
qqnorm(expK)
qqline(expK)
pnorm(Glass$K,-0.4970561,0.6521918)
--------------------------------------------------------------
#Measures of Central Tendency                Ca
mean(Glass$Ca)  ### 8.956963
median(Glass$Ca)##### 8.6
#mode
getmode(Glass$Ca)  ##### 8.43
#Measures of Dispersion
var(Glass$Ca)  ###### 2.025366
sd(Glass$Ca)  #### 1.423153
range(Glass$Ca) ##### 5.43 16.19
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Ca) ##### 10.76
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Ca)  #### 2.032677
#Measures of Kurtosis 
kurtosis(Glass$Ca)###### 9.498968
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Ca,horizontal = TRUE)
hist(Glass$Ca)
barplot(Glass$Ca)
#qqplot
qqnorm(Glass$Ca)
qqline(Glass$Ca)
expCa<-exp(Glass$Ca)
qqnorm(expCa)
qqline(expCa)
pnorm(Glass$Ca,8.956963,1.423153)
-------------------------------------------------------------------
#Measures of Central Tendency                Ba
mean(Glass$Ba)  ### -0.1750467
median(Glass$Ba)##### 0
#mode
getmode(Glass$Ba)  ##### 0
#Measures of Dispersion
var(Glass$Ba)  ###### 0.247227
sd(Glass$Ba)  #### 0.4972193
range(Glass$Ba) ##### 0.00 3.15
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Ba) ##### 3.15
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Ba)  #### 3.392431
#Measures of Kurtosis 
kurtosis(Glass$Ba)###### 15.22207
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Ba,horizontal = TRUE)
hist(Glass$Ba)
barplot(Glass$Ba)
#qqplot
qqnorm(Glass$Ba)
qqline(Glass$Ba)
pnorm(Glass$Ba,-0.1750467,0.4972193)
---------------------------------------------------------------------
#Measures of Central Tendency                Fe
mean(Glass$Fe)  ### -0.05700935
median(Glass$Fe)##### 0
#mode
getmode(Glass$Fe)  ##### 0
#Measures of Dispersion
var(Glass$Fe)  ###### 0.0094943
sd(Glass$Fe)  #### 0.0974387
range(Glass$Fe) ##### 0.00 0.51
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Fe) ##### 0.51
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Fe)  ####  1.742007
#Measures of Kurtosis 
kurtosis(Glass$Fe)###### 5.572318
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Fe,horizontal = TRUE)
hist(Glass$Fe)
barplot(Glass$Fe)
#qqplot
qqnorm(Glass$Fe)
qqline(Glass$Fe)
expFe<-exp(Glass$Fe)
qqnorm(expFe)
qqline(expFe)
sqrtFe<-sqrt(Glass$Fe)
qqnorm(sqrtFe)
qqline(sqrtFe)
recexpFe<-(1/(exp(Glass$Fe)))
qqnorm(recexpFe)
qqline(recexpFe)
pnorm(Glass$Fe,-0.05700935,0.0974387)  
---------------------------------------------------------------
#Measures of Central Tendency                Type
mean(Glass$Type)  ### -2.780374
median(Glass$Type)##### 2
#mode
getmode(Glass$Type)  ##### 2
#Measures of Dispersion
var(Glass$Type)  ###### 4.425716
sd(Glass$Type)  #### 2.103739
range(Glass$Type) ##### 1  7
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Glass$Type) ##### 6
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Glass$Type)  #### 1.107085
#Measures of Kurtosis 
kurtosis(Glass$Type)###### 2.699063
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Glass$Type,horizontal = TRUE)
hist(Glass$Type)
barplot(Glass$Type)
#qqplot
qqnorm(Glass$Type)
qqline(Glass$Type)
expType<-exp(Glass$Type)
qqnorm(expType)
qqline(expType)
pnorm(Glass$Type,-2.780374,2.103739)
------------------------------------------------------------------
#####Data Conditioning in which Pythogenic traits used for classification
names(d) <- c("RI", "Na", "Mg","Al","Si","K","Ca","Ba","Fe","Type")
type <- table(d$Type)
d_target <- d[, 10]
d_key <- d[, 1]  
####table of type
table(Glass$Type)
names(type) <- c("building_windows_float_processed", "building_windows_non_float_processed",
                  "vehicle_windows_float_processed", "vehicle_windows_non_float_processed ",
                  "containers", "tableware", "headlamps")
type
##types<-types[-8]
summary(d)
str(d)  
# Replace type 1,2,3,4,5,6,7 with mammal,bird,reptile,fish, amphibian, insect, crustacean
# Type is factor with 7 levels. We also replacing these 7 entries with mammal,bird,reptile,fish, amphibian, insect, crustacean
Glass$Type <- factor(Glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("building_windows_float_processed", "building_windows_non_float_processed",
"vehicle_windows_float_processed", "vehicle_windows_non_float_processed ","containers", "tableware", "headlamps"))

# table or proportation of enteries in the datasets.
round(prop.table(table(Glass$Type))*100,1)
summary(Glass[c("RI","Na", "Mg")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
View(Glass)
#Apply the normalization function to wbcd dataset
Glass_norm <- as.data.frame(lapply(Glass[1:9], norm))
View(Glass_norm)
#create training and test datasets
Glass_train <- Glass_norm[1:170,]
Glass_test <- Glass_norm[171:214,]
#Get labels for training and test datasets
Glass_train_labels <- Glass[1:170,10]
Glass_test_labels <- Glass[171:214,10]
# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(1,170,1))
{
  train_Glass_pred <- knn(train=Glass_train,test=Glass_train,cl=Glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_Glass_pred==Glass_train_labels))
  test_Glass_pred <- knn(train = Glass_train, test = Glass_test, cl = Glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_Glass_pred==Glass_test_labels))
}
# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,170,1),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(1,170,1),test_acc,type="l",main="Test_accuracy",col="red")
acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,170,1)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
Glass_pred <- knn(train = Glass_train, test = Glass_test, cl = Glass_train_labels, k=11)

