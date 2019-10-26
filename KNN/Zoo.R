Zoo <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\KNN\\Zoo.csv")
View(Zoo)
attach(Zoo)
sum(is.na(Zoo))
Zoo <- na.omit(Zoo) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(Zoo)### 600 6
colnames(Zoo)
#####Load Data
set.seed(1)
library(class)
d = read.table("Zoo.csv", sep=",", header = FALSE)
d = data.frame(d)  
-------------------------------------------------
#Measures of Central Tendency                hair
mean(Zoo$hair)  ### -0.4257426
median(Zoo$hair)##### 0
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Zoo$hair)  ###### 0
#Measures of Dispersion
var(Zoo$hair)  ###### 0.2469307
sd(Zoo$hair)  #### 0.4969212
range(Zoo$hair)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$hair) ##### 1
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Zoo$hair)  #### 0.3003606
#Measures of Kurtosis 
kurtosis(Zoo$hair)######  1.090217
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Zoo$hair,horizontal = TRUE)
hist(Zoo$hair)
barplot(Zoo$hair)
str(Zoo)
#qqplot
qqnorm(Zoo$hair)
qqline(Zoo$hair)
exphair<-exp(Zoo$hair)
qqnorm(exphair)
qqline(exphair)
sqrthair<-sqrt(exp(Zoo$hair))
qqnorm(sqrthair)
rechair<-(1/exp(Zoo$hair))
qqnorm(rechair)
qqline(rechair)
rec2hair<-(1/exp(Zoo$hair)) * (1/exp(Zoo$hair))
qqnorm(rec2hair)
rec4hair<-rec2hair * rec2hair
qqnorm(rec4hair)
rec8hair<-rec4hair * rec4hair
qqnorm(rec8hair)
rec16hair<-rec8hair * rec8hair
install.packages(psych)
library(psych)
describe(Zoo)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(Zoo$hair,-0.4257426,0.4969212)
-----------------------------------------------------------------------
#Measures of Central Tendency                feathers
mean(Zoo$feathers)  ### -0.1980198
median(Zoo$feathers)##### 0
#mode
getmode(Zoo$feathers)  #####  0
#Measures of Dispersion
var(Zoo$feathers)  ###### 0.160396
sd(Zoo$feathers)  ####  0.4004947
range(Zoo$feathers) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$feathers) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$feathers)  #### 1.515557
#Measures of Kurtosis 
kurtosis(Zoo$feathers)###### 3.296914
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$feathers,horizontal = TRUE)
hist(Zoo$feathers)
barplot(Zoo$feathers)
#qqplot
qqnorm(Zoo$feathers)
qqline(Zoo$feathers)###Normalisation achieved
pnorm(Zoo$feathers,-0.1980198,0.4004947)
-----------------------------------------------------------------------    
#Measures of Central Tendency                eggs
mean(Zoo$eggs)  ### -0.5841584
median(Zoo$eggs)##### 1
#mode
getmode(Zoo$eggs)  ##### 1
#Measures of Dispersion
var(Zoo$eggs)  ###### 0.2453465
sd(Zoo$eggs)  #### 0.4953247
range(Zoo$eggs) ##### 0 1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$eggs) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$eggs)  #### -0.3415059
#Measures of Kurtosis 
kurtosis(Zoo$eggs)###### 1.116626
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$eggs,horizontal = TRUE)
hist(Zoo$eggs)
barplot(Zoo$eggs)
#qqplot
qqnorm(Zoo$eggs)
qqline(Zoo$eggs)
pnorm(Zoo$eggs,-0.5841584,0.4953247)
------------------------------------------------------------------
#Measures of Central Tendency                milk
mean(Zoo$milk)  ### 0.4059406
median(Zoo$milk)##### 0
#mode
getmode(Zoo$milk)  ##### 0
#Measures of Dispersion
var(Zoo$milk)  ###### 0.2435644
sd(Zoo$milk)  #### 0.4935224
range(Zoo$milk) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$milk) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$milk)  #### 0.383077
#Measures of Kurtosis 
kurtosis(Zoo$milk)###### 1.146748
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$milk,horizontal = TRUE)
hist(Zoo$milk)
barplot(Zoo$milk)
#qqplot
qqnorm(Zoo$milk)
qqline(Zoo$milk)
pnorm(Zoo$milk,0.4059406,0.4935224)
-------------------------------------------------------------------
#Measures of Central Tendency                airborne
mean(Zoo$airborne)  ### -0.2376238
median(Zoo$airborne)##### 0
#mode
getmode(Zoo$airborne)  ##### 0
#Measures of Dispersion
var(Zoo$airborne)  ######0.1829703
sd(Zoo$airborne)  #### 0.4277503
range(Zoo$airborne) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$airborne) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$airborne)  #### 1.232892
#Measures of Kurtosis 
kurtosis(Zoo$airborne)###### 2.520022
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$airborne,horizontal = TRUE)
hist(Zoo$airborne)
barplot(Zoo$airborne)
#qqplot
qqnorm(Zoo$airborne)
qqline(Zoo$airborne)
pnorm(Zoo$airborne,-0.2376238,0.4277503)
-------------------------------------------------------------
#Measures of Central Tendency                aquatic
mean(Zoo$aquatic)  ### -0.3564356
median(Zoo$aquatic)##### 0
#mode
getmode(Zoo$aquatic)  ##### 0
#Measures of Dispersion
var(Zoo$aquatic)  ###### 0.2316832
sd(Zoo$aquatic)  #### 0.4813348
range(Zoo$aquatic) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$aquatic) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$aquatic)  #### 0.5995012
#Measures of Kurtosis 
kurtosis(Zoo$aquatic)###### 1.359402
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$aquatic,horizontal = TRUE)
hist(Zoo$aquatic)
barplot(Zoo$aquatic)
#qqplot
qqnorm(Zoo$aquatic)
qqline(Zoo$aquatic)
recaquatic<-(1/exp(Zoo$aquatic))
qqnorm(recaquatic)
qqline(recaquatic)
pnorm(Zoo$aquatic,-0.3564356,0.4813348)
--------------------------------------------------------------
#Measures of Central Tendency                Predator
mean(Zoo$predator)  ### 0.5544554
median(Zoo$predator)##### 1
#mode
getmode(Zoo$predator)  ##### 1
#Measures of Dispersion
var(Zoo$predator)  ###### 0.249505
sd(Zoo$predator)  #### 0.4995047
range(Zoo$predator) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$predator) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$predator)  #### -0.2191252
#Measures of Kurtosis 
kurtosis(Zoo$predator)###### 1.048016
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$predator,horizontal = TRUE)
hist(Zoo$predator)
barplot(Zoo$predator)
#qqplot
qqnorm(Zoo$predator)
qqline(Zoo$predator)
pnorm(Zoo$predator,0.5544554,0.4995047)
-------------------------------------------------------------------
#Measures of Central Tendency                Toothed
mean(Zoo$toothed)  ### -0.6039604
median(Zoo$toothed)##### 1
#mode
getmode(Zoo$toothed)  ##### 1
#Measures of Dispersion
var(Zoo$toothed)  ###### 0.2415842
sd(Zoo$toothed)  #### 0.4915121
range(Zoo$toothed) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$toothed) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$toothed)  #### 0.04384163
#Measures of Kurtosis 
kurtosis(Zoo$toothed)###### 1.180738
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$toothed,horizontal = TRUE)
hist(Zoo$toothed)
barplot(Zoo$toothed)
#qqplot
qqnorm(Zoo$toothed)
qqline(Zoo$toothed)
pnorm(Zoo$toothed,-0.6039604,0.4915121)
---------------------------------------------------------------------
#Measures of Central Tendency                backbone
mean(Zoo$backbone)  ### -0.8217822
median(Zoo$backbone)##### 1
#mode
getmode(Zoo$backbone)  ##### 1
#Measures of Dispersion
var(Zoo$backbone)  ###### 0.1479208
sd(Zoo$backbone)  #### 0.3846047
range(Zoo$backbone) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$backbone) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$backbone)  #### -1.681659
#Measures of Kurtosis 
kurtosis(Zoo$backbone)###### 3.827979
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$backbone,horizontal = TRUE)
hist(Zoo$backbone)
barplot(Zoo$backbone)
#qqplot
qqnorm(Zoo$backbone)
qqline(Zoo$backbone)
pnorm(Zoo$backbone,-0.8217822,0.3846047)  
---------------------------------------------------------------
#Measures of Central Tendency                breathes
mean(Zoo$breathes)  ### -0.7920792
median(Zoo$breathes)##### 1
#mode
getmode(Zoo$breathes)  ##### 1
#Measures of Dispersion
var(Zoo$breathes)  ###### 0.1663366
sd(Zoo$breathes)  #### 0.4078439
range(Zoo$breathes) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$breathes) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$breathes)  #### -1.439453
#Measures of Kurtosis 
kurtosis(Zoo$breathes)###### 3.072024
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$breathes,horizontal = TRUE)
hist(Zoo$breathes)
barplot(Zoo$breathes)
#qqplot
qqnorm(Zoo$breathes)
qqline(Zoo$breathes)
pnorm(Zoo$breathes,-0.7920792,0.4078439)
------------------------------------------------------------------
#Measures of Central Tendency                venomous
mean(Zoo$venomous)  ###  0.07920792
median(Zoo$venomous)##### 0
#mode
getmode(Zoo$venomous)  ##### 0
#Measures of Dispersion
var(Zoo$venomous)  ###### 0.07366337
sd(Zoo$venomous)  #### 0.27141
range(Zoo$venomous) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$venomous) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$venomous)  #### 3.116251
#Measures of Kurtosis 
kurtosis(Zoo$venomous)###### 10.71102
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$venomous,horizontal = TRUE)
hist(Zoo$venomous)
barplot(Zoo$venomous)
#qqplot
qqnorm(Zoo$venomous)
qqline(Zoo$venomous)
pnorm(Zoo$venomous,0.07920792,0.27141)  
-----------------------------------------------------------------------  
#Measures of Central Tendency                fins
mean(Zoo$fins)  ###  -0.1683168
median(Zoo$fins)##### 0
#mode
getmode(Zoo$fins)  ##### 0
#Measures of Dispersion
var(Zoo$fins)  ###### 0.1413861
sd(Zoo$fins)  #### 0.3760135
range(Zoo$fins) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$fins) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$fins)  #### 1.773008
#Measures of Kurtosis 
kurtosis(Zoo$fins)###### 4.143557
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$fins,horizontal = TRUE)
hist(Zoo$fins)
barplot(Zoo$fins)
#qqplot
qqnorm(Zoo$fins)
qqline(Zoo$fins)
pnorm(Zoo$fins,-0.1683168,0.3760135)   
-----------------------------------------------------------------
#Measures of Central Tendency                legs
mean(Zoo$legs)  ###  -2.841584
median(Zoo$legs)##### 4
#mode
getmode(Zoo$legs)  ##### 4
#Measures of Dispersion
var(Zoo$legs)  ###### 4.134653
sd(Zoo$legs)  #### 2.033385
range(Zoo$legs) ##### 0  8
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$legs) ##### 8
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$legs)  #### 0.1373478
#Measures of Kurtosis 
kurtosis(Zoo$legs)###### 2.371774
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$legs,horizontal = TRUE)
hist(Zoo$legs)
barplot(Zoo$legs)
#qqplot
qqnorm(Zoo$legs)
qqline(Zoo$legs)
explegs<-exp(Zoo$legs)
qqnorm(explegs)
qqline(explegs)
pnorm(Zoo$legs,-2.841584,2.033385)   
----------------------------------------------------------------------
#Measures of Central Tendency                tail
mean(Zoo$tail)  ###  0.7425743
median(Zoo$tail)##### 1
#mode
getmode(Zoo$tail)  ##### 1
#Measures of Dispersion
var(Zoo$tail)  ###### 0.1930693
sd(Zoo$tail)  #### 0.4393965
range(Zoo$tail) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$tail) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$tail)  #### -1.109631
#Measures of Kurtosis 
kurtosis(Zoo$tail)###### 2.231282
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$tail,horizontal = TRUE)
hist(Zoo$tail)
barplot(Zoo$tail)
#qqplot
qqnorm(Zoo$tail)
qqline(Zoo$tail)
pnorm(Zoo$tail,0.7425743,0.4393965)    
---------------------------------------------------------------
#Measures of Central Tendency                domestic
mean(Zoo$domestic)  ###  -0.1287129
median(Zoo$domestic)##### 0
#mode
getmode(Zoo$domestic)  ##### 0
#Measures of Dispersion
var(Zoo$domestic)  ###### 0.1132673
sd(Zoo$domestic)  #### 0.3365521
range(Zoo$domestic) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$domestic) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$domestic)  #### 2.217421
#Measures of Kurtosis 
kurtosis(Zoo$domestic)###### 5.916958
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$domestic,horizontal = TRUE)
hist(Zoo$domestic)
barplot(Zoo$domestic)
#qqplot
qqnorm(Zoo$domestic)
qqline(Zoo$domestic)
pnorm(Zoo$domestic,-0.1287129,0.3365521)   
--------------------------------------------------------------------
#Measures of Central Tendency                catsize
mean(Zoo$catsize)  ###  0.4356436
median(Zoo$catsize)##### 0
#mode
getmode(Zoo$catsize)  ##### 0
#Measures of Dispersion
var(Zoo$catsize)  ###### 0.2483168
sd(Zoo$catsize)  #### 0.498314
range(Zoo$catsize) ##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$catsize) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$catsize)  #### 0.259585
#Measures of Kurtosis 
kurtosis(Zoo$catsize)###### 1.067384
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$catsize,horizontal = TRUE)
hist(Zoo$catsize)
barplot(Zoo$catsize)
#qqplot
qqnorm(Zoo$catsize)
qqline(Zoo$catsize)
pnorm(Zoo$catsize,0.4356436,0.498314)     
-------------------------------------------------------------------
#Measures of Central Tendency                type
mean(Zoo$type)  ###  -2.831683
median(Zoo$type)##### 2
#mode
getmode(Zoo$type)  ##### 1
#Measures of Dispersion
var(Zoo$type)  ###### 4.421386
sd(Zoo$type)  #### 2.102709
range(Zoo$type) ##### 1  7
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Zoo$type) ##### 6
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Zoo$type)  #### 0.8396505
#Measures of Kurtosis 
kurtosis(Zoo$type)###### 2.277805
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Zoo$type,horizontal = TRUE)
hist(Zoo$type)
barplot(Zoo$type)
#qqplot
qqnorm(Zoo$type)
qqline(Zoo$type)
exptype<-exp(Zoo$type)
qqnorm(exptype)
qqline(exptype)
pnorm(Zoo$type,-2.831683,2.102709)    
-------------------------------------------------------------------
#####Data Conditioning in which Pythogenic traits used for classification
  names(d) <- c("hair", "feathers", "eggs", "milk", "airborne",
                "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
                "fins", "legs", "tail", "domestic", "catsize", "type")

types <- table(d$type)
d_target <- d[, 17]
d_key <- d[, 1]
d$animal <- NULL  
####table of type
table(Zoo$type)
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types
types<-types[-8]
summary(d)
str(d)  
# Replace type 1,2,3,4,5,6,7 with mammal,bird,reptile,fish, amphibian, insect, crustacean
# Type is factor with 7 levels. We also replacing these 7 entries with mammal,bird,reptile,fish, amphibian, insect, crustacean
Zoo$type <- factor(Zoo$type, levels = c("1","2","3","4","5","6","7"), labels = c("mammal","bird","reptile","fish","amphibian","insect","crustacean"))

# table or proportation of enteries in the datasets.
round(prop.table(table(Zoo$type))*100,1)
summary(Zoo[c("hair","feathers", "eggs")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
View(Zoo)
#Apply the normalization function to wbcd dataset
Zoo_norm <- as.data.frame(lapply(Zoo[2:17], norm))
View(Zoo_norm)
#create training and test datasets
Zoo_train <- Zoo_norm[1:70,]
Zoo_test <- Zoo_norm[71:101,]
#Get labels for training and test datasets
Zoo_train_labels <- Zoo[1:70,18]
Zoo_test_labels <- Zoo[71:101,18]
# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(1,70,1))
{
  train_Zoo_pred <- knn(train=Zoo_train,test=Zoo_train,cl=Zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_Zoo_pred==Zoo_train_labels))
  test_Zoo_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_Zoo_pred==Zoo_test_labels))
}
# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,70,1),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(1,70,1),test_acc,type="l",main="Test_accuracy",col="red")
acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,70,1)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
Zoo_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=12)

