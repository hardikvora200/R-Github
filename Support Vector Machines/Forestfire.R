Forestfire <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Support Vector Machines\\forestfires.csv")
View(Forestfire)
attach(Forestfire)
sum(is.na(Forestfire))  ##0
Forestfire <- na.omit(Forestfire) # Omitting NA values from the Data if it is there
dim(Forestfire)### 517  31
colnames(Forestfire)
set.seed(517)
library(class)
-------------------------------------------------------------------------------
#Measures of Central Tendency                FFMC
mean(Forestfire$FFMC)  ### -90.64468
median(Forestfire$FFMC)##### 91.6
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Forestfire$FFMC)  ###### 92.1
#Measures of Dispersion
var(Forestfire$FFMC)  ###### 30.47162
sd(Forestfire$FFMC)  #### 5.520111
range(Forestfire$FFMC)##### 18.7 96.2
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$FFMC) ##### 77.5
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Forestfire$FFMC)  #### -6.556512
#Measures of Kurtosis 
kurtosis(Forestfire$FFMC)######  69.4076
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$FFMC,horizontal = TRUE)
hist(Forestfire$FFMC)
barplot(Forestfire$FFMC)
str(Forestfire)
#qqplot
qqnorm(Forestfire$FFMC)
qqline(Forestfire$FFMC)
logFFMC<-log(Forestfire$FFMC)
qqnorm(logFFMC)
qqline(logFFMC)
library(psych)
describe(Forestfire)
pnorm(Forestfire$FFMC,-90.64468,5.520111)
-----------------------------------------------------------------------
#Measures of Central Tendency                DMC
mean(Forestfire$DMC)  ####  -110.8723
median(Forestfire$DMC)##### 108.3
getmode(Forestfire$DMC)  ###### 99
#Measures of Dispersion
var(Forestfire$DMC)  ###### 4101.952
sd(Forestfire$DMC)  #### 64.04648
range(Forestfire$DMC)##### 1.1  291.3
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$DMC) ##### 290.2
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Forestfire$DMC)  #### 0.545908
#Measures of Kurtosis 
kurtosis(Forestfire$DMC)######  3.191263
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$DMC,horizontal = TRUE)
hist(Forestfire$DMC)
barplot(Forestfire$DMC)
#qqplot
qqnorm(Forestfire$DMC)
qqline(Forestfire$DMC)
logDMC<-log(Forestfire$DMC)
qqnorm(logDMC)
qqline(logDMC)
expDMC<-exp(Forestfire$DMC)
qqnorm(expDMC)
qqline(expDMC)
library(psych)
pnorm(Forestfire$DMC,-110.8723,64.04648)
-----------------------------------------------------------------------   
#Measures of Central Tendency                DC
mean(Forestfire$DC)  ####  -547.94
median(Forestfire$DC)##### 664.2
getmode(Forestfire$DC)  ###### 745.3
#Measures of Dispersion
var(Forestfire$DC)  ###### 61536.84
sd(Forestfire$DC)  #### 248.0662
range(Forestfire$DC)##### 7.9  860.6
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$DC) ##### 852.7
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$DC)  #### -1.09725
#Measures of Kurtosis 
kurtosis(Forestfire$DC)######  2.745539
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$DC,horizontal = TRUE)
hist(Forestfire$DC)
barplot(Forestfire$DC)
#qqplot
qqnorm(Forestfire$DC)
qqline(Forestfire$DC)
logDC<-log(Forestfire$DC)
qqnorm(logDC)
qqline(logDC)
sqrtDC<-sqrt(Forestfire$DC)
qqnorm(sqrtDC)
qqline(sqrtDC)
recDC<-(1/(Forestfire$DC))
qqnorm(recDC)
qqline(recDC)
library(psych)
pnorm(Forestfire$DC,-547.94,248.0662)
-----------------------------------------------------------
#Measures of Central Tendency                ISI
mean(Forestfire$ISI)  ####  -9.021663
median(Forestfire$ISI)##### 8.4
getmode(Forestfire$ISI)  ###### 9.6
#Measures of Dispersion
var(Forestfire$ISI)  ###### 20.78883
sd(Forestfire$ISI)  #### 4.559477
range(Forestfire$ISI)##### 0.0  56.1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$ISI) ##### 56.1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$ISI)  #### 2.528961
#Measures of Kurtosis 
kurtosis(Forestfire$ISI)######  24.23949
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$ISI,horizontal = TRUE)
hist(Forestfire$ISI)
barplot(Forestfire$ISI)
#qqplot
qqnorm(Forestfire$ISI)
qqline(Forestfire$ISI)
library(psych)
pnorm(Forestfire$ISI,-9.021663,4.559477)
------------------------------------------------------------------
#Measures of Central Tendency                temp
mean(Forestfire$temp)  ####  18.88917
median(Forestfire$temp)##### 19.3
getmode(Forestfire$temp)  ###### 19.6
#Measures of Dispersion
var(Forestfire$temp)  ###### 33.7169
sd(Forestfire$temp)  #### 5.806625
range(Forestfire$temp)##### 2.2  33.3
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$temp) ##### 31.1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$temp)  #### -0.3302106
#Measures of Kurtosis 
kurtosis(Forestfire$temp)######  3.123269
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$temp,horizontal = TRUE)
hist(Forestfire$temp)
barplot(Forestfire$temp)
#qqplot
qqnorm(Forestfire$temp)
qqline(Forestfire$temp)
library(psych)
pnorm(Forestfire$temp,18.88917,5.806625)
----------------------------------------------------------------
#Measures of Central Tendency                RH
mean(Forestfire$RH)  ####  44.2882
median(Forestfire$RH)##### 42
getmode(Forestfire$RH)  ###### 27
#Measures of Dispersion
var(Forestfire$RH)  ###### 266.2598
sd(Forestfire$RH)  #### 16.31747
range(Forestfire$RH)##### 15  100
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$RH) ##### 85
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$RH)  #### 0.8603984
#Measures of Kurtosis 
kurtosis(Forestfire$RH)######   3.422374
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$RH,horizontal = TRUE)
hist(Forestfire$RH)
barplot(Forestfire$RH)
#qqplot
qqnorm(Forestfire$RH)
qqline(Forestfire$RH)
library(psych)
pnorm(Forestfire$RH,44.2882,16.31747)
-----------------------------------------------------------------
#Measures of Central Tendency                wind
mean(Forestfire$wind)  ####  -4.017602
median(Forestfire$wind)##### 4
getmode(Forestfire$wind)  ###### 3.1
#Measures of Dispersion
var(Forestfire$wind)  ###### 3.210019
sd(Forestfire$wind)  #### 1.791653
range(Forestfire$wind)##### 0.4 9.4
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$wind) ##### 9
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$wind)  #### 0.5693431
#Measures of Kurtosis 
kurtosis(Forestfire$wind)###### 3.042217
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$wind,horizontal = TRUE)
hist(Forestfire$wind)
barplot(Forestfire$wind)
#qqplot
qqnorm(Forestfire$wind)
qqline(Forestfire$wind)
logwind<-log(Forestfire$wind)
qqnorm(logwind)
expwind<-exp(Forestfire$wind)
qqnorm(expwind)
qqline(expwind)
library(psych)
pnorm(Forestfire$wind,-4.017602,1.791653)
---------------------------------------------------------------
##Measures of Central Tendency                rain
mean(Forestfire$rain)  ####   -0.02166344
median(Forestfire$rain)##### 0
getmode(Forestfire$rain)  ###### 0
#Measures of Dispersion
var(Forestfire$rain)  ###### 0.0875918
sd(Forestfire$rain)  #### 0.2959591
range(Forestfire$rain)##### 0.0  6.4
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$rain) ##### 6.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$rain)  ####  19.7588
#Measures of Kurtosis 
kurtosis(Forestfire$rain)###### 420.221
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$rain,horizontal = TRUE)
hist(Forestfire$rain)
barplot(Forestfire$rain)
#qqplot
qqnorm(Forestfire$rain)
qqline(Forestfire$rain)
library(psych)
pnorm(Forestfire$rain,-0.02166344,0.2959591)
---------------------------------------------------------------
##Measures of Central Tendency                area
mean(Forestfire$area)  ####   -12.84729
median(Forestfire$area)##### 0.52
getmode(Forestfire$area)  ###### 0
#Measures of Dispersion
var(Forestfire$area)  ###### 4052.063
sd(Forestfire$area)  #### 63.65582
range(Forestfire$area)##### 0.00 1090.84
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$area) ##### 1090.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$area)  ####  12.80963
#Measures of Kurtosis 
kurtosis(Forestfire$area)###### 195.2566
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$area,horizontal = TRUE)
hist(Forestfire$area)
barplot(Forestfire$area)
#qqplot
qqnorm(Forestfire$area)
qqline(Forestfire$area)
library(psych)
pnorm(Forestfire$area,-12.84729,63.65582)
---------------------------------------------------------------
##Measures of Central Tendency                dayfri
mean(Forestfire$dayfri)  ####   -0.1644101
median(Forestfire$dayfri)##### 0
getmode(Forestfire$dayfri)  ###### 0
#Measures of Dispersion
var(Forestfire$dayfri)  ###### 0.1376456
sd(Forestfire$dayfri)  #### 0.3710062
range(Forestfire$dayfri)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$dayfri) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$dayfri)  ####  1.810832
#Measures of Kurtosis 
kurtosis(Forestfire$dayfri)###### 4.279112
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$dayfri,horizontal = TRUE)
hist(Forestfire$dayfri)
barplot(Forestfire$dayfri)
#qqplot
qqnorm(Forestfire$dayfri)
qqline(Forestfire$dayfri)
library(psych)
pnorm(Forestfire$dayfri,-0.1644101,0.3710062)
-----------------------------------------------------------
##Measures of Central Tendency                daymon
mean(Forestfire$daymon)  ####   -0.1431335
median(Forestfire$daymon)##### 0
getmode(Forestfire$daymon)  ###### 0
#Measures of Dispersion
var(Forestfire$daymon)  ###### 0.122884
sd(Forestfire$daymon)  #### 0.3505481
range(Forestfire$daymon)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daymon) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daymon)  ####  2.038021
#Measures of Kurtosis 
kurtosis(Forestfire$daymon)###### 5.153529
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daymon,horizontal = TRUE)
hist(Forestfire$daymon)
barplot(Forestfire$daymon)
#qqplot
qqnorm(Forestfire$daymon)
qqline(Forestfire$daymon)
library(psych)
pnorm(Forestfire$daymon,-0.1431335,0.3505481)
-------------------------------------------------------------------------
##Measures of Central Tendency                daysat
mean(Forestfire$daysat)  ####   -0.1624758
median(Forestfire$daysat)##### 0
getmode(Forestfire$daysat)  ###### 0
#Measures of Dispersion
var(Forestfire$daysat)  ###### 0.1363411
sd(Forestfire$daysat)  #### 0.369244
range(Forestfire$daysat)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daysat) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daysat)  ####  1.829961
#Measures of Kurtosis 
kurtosis(Forestfire$daysat)###### 4.348757
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daysat,horizontal = TRUE)
hist(Forestfire$daysat)
barplot(Forestfire$daysat)
#qqplot
qqnorm(Forestfire$daysat)
qqline(Forestfire$daysat)
library(psych)
pnorm(Forestfire$daysat,-0.1624758,0.369244)
-------------------------------------------------------------
##Measures of Central Tendency                daysun
mean(Forestfire$daysun)  ####   -0.1837524
median(Forestfire$daysun)##### 0
getmode(Forestfire$daysun)  ###### 0
#Measures of Dispersion
var(Forestfire$daysun)  ###### 0.1502781
sd(Forestfire$daysun)  ####  0.3876572
range(Forestfire$daysun)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daysun) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daysun)  ####  1.633164
#Measures of Kurtosis
kurtosis(Forestfire$daysun)###### 3.667224
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daysun,horizontal = TRUE)
hist(Forestfire$daysun)
barplot(Forestfire$daysun)
#qqplot
qqnorm(Forestfire$daysun)
qqline(Forestfire$daysun)
library(psych)
pnorm(Forestfire$daysun,-0.1837524,0.3876572)
-------------------------------------------------------------------
##Measures of Central Tendency                daythu
mean(Forestfire$daythu)  ####   -0.1179884
median(Forestfire$daythu)##### 0
getmode(Forestfire$daythu)  ###### 0
#Measures of Dispersion
var(Forestfire$daythu)  ###### 0.1042688
sd(Forestfire$daythu)  ####  0.3229068
range(Forestfire$daythu)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daythu) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daythu)  #### 2.368371
#Measures of Kurtosis
kurtosis(Forestfire$daythu)###### 6.609182
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daythu,horizontal = TRUE)
hist(Forestfire$daythu)
barplot(Forestfire$daythu)
#qqplot
qqnorm(Forestfire$daythu)
qqline(Forestfire$daythu)
library(psych)
pnorm(Forestfire$daythu,-0.1179884,0.3229068)
---------------------------------------------------------------
##Measures of Central Tendency                daytue
mean(Forestfire$daytue)  ####    -0.1237911
median(Forestfire$daytue)##### 0
getmode(Forestfire$daytue)  ###### 0
#Measures of Dispersion
var(Forestfire$daytue)  ###### 0.1086771
sd(Forestfire$daytue)  ####  0.3296621
range(Forestfire$daytue)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daytue) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daytue)  #### 2.284602
#Measures of Kurtosis
kurtosis(Forestfire$daytue)###### 6.219405
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daytue,horizontal = TRUE)
hist(Forestfire$daytue)
barplot(Forestfire$daytue)
#qqplot
qqnorm(Forestfire$daytue)
qqline(Forestfire$daytue)
library(psych)
pnorm(Forestfire$daytue,-0.1237911,0.3296621)
----------------------------------------------------------------
##Measures of Central Tendency                daywed
mean(Forestfire$daywed)  ####    0.1044487
median(Forestfire$daywed)##### 0
getmode(Forestfire$daywed)  ###### 0
#Measures of Dispersion
var(Forestfire$daywed)  ###### 0.09372048
sd(Forestfire$daywed)  ####  0.306138
range(Forestfire$daywed)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$daywed) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$daywed)  #### 2.58664
#Measures of Kurtosis
kurtosis(Forestfire$daywed)###### 7.690705
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$daywed,horizontal = TRUE)
hist(Forestfire$daywed)
barplot(Forestfire$daywed)
#qqplot
qqnorm(Forestfire$daywed)
qqline(Forestfire$daywed)
library(psych)
pnorm(Forestfire$daywed,0.1044487,0.306138)
--------------------------------------------------------------------
##Measures of Central Tendency                monthapr
mean(Forestfire$monthapr)  ####    0.01740812
median(Forestfire$monthapr)##### 0
getmode(Forestfire$monthapr)  ###### 0
#Measures of Dispersion
var(Forestfire$monthapr)  ###### 0.01713823
sd(Forestfire$monthapr)  ####  0.1309131
range(Forestfire$monthapr)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthapr) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthapr)  #### 7.379848
#Measures of Kurtosis
kurtosis(Forestfire$monthapr)###### 55.46216
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthapr,horizontal = TRUE)
hist(Forestfire$monthapr)
barplot(Forestfire$monthapr)
#qqplot
qqnorm(Forestfire$monthapr)
qqline(Forestfire$monthapr)
library(psych)
pnorm(Forestfire$monthapr,0.01740812,0.1309131)
-----------------------------------------------------------
##Measures of Central Tendency                monthaug
mean(Forestfire$monthaug)  ####    -0.3558994
median(Forestfire$monthaug)##### 0
getmode(Forestfire$monthaug)  ###### 0
#Measures of Dispersion
var(Forestfire$monthaug)  ###### 0.2296793
sd(Forestfire$monthaug)  ####  0.4792487
range(Forestfire$monthaug)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthaug) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthaug)  #### 0.6019428
#Measures of Kurtosis
kurtosis(Forestfire$monthaug)###### 1.362335
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthaug,horizontal = TRUE)
hist(Forestfire$monthaug)
barplot(Forestfire$monthaug)
#qqplot
qqnorm(Forestfire$monthaug)
qqline(Forestfire$monthaug)
expaug<-exp(Forestfire$monthaug)
qqnorm(expaug)
qqline(expaug)
library(psych)
pnorm(Forestfire$monthaug,-0.3558994,0.4792487)
----------------------------------------------------------------
##Measures of Central Tendency                monthdec
mean(Forestfire$monthdec)  ####   0.01740812
median(Forestfire$monthdec)##### 0
getmode(Forestfire$monthdec)  ###### 0
#Measures of Dispersion
var(Forestfire$monthdec)  ###### 0.01713823
sd(Forestfire$monthdec)  ####  0.1309131
range(Forestfire$monthdec)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthdec) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthdec)  #### 7.379848
#Measures of Kurtosis
kurtosis(Forestfire$monthdec)###### 55.46216
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthdec,horizontal = TRUE)
hist(Forestfire$monthdec)
barplot(Forestfire$monthdec)
#qqplot
qqnorm(Forestfire$monthdec)
qqline(Forestfire$monthdec)
expdec<-exp(Forestfire$monthdec)
qqnorm(expdec)
qqline(expdec)
library(psych)
pnorm(Forestfire$monthdec,0.01740812,0.1309131)
-------------------------------------------------------------
##Measures of Central Tendency                monthfeb
mean(Forestfire$monthfeb)  ####   -0.03868472
median(Forestfire$monthfeb)##### 0
getmode(Forestfire$monthfeb)  ###### 0
#Measures of Dispersion
var(Forestfire$monthfeb)  ###### 0.03726028
sd(Forestfire$monthfeb)  ####  0.1930292
range(Forestfire$monthfeb)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthfeb) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthfeb)  #### 4.784375
#Measures of Kurtosis
kurtosis(Forestfire$monthfeb)###### 23.89024
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthfeb,horizontal = TRUE)
hist(Forestfire$monthfeb)
barplot(Forestfire$monthfeb)
#qqplot
qqnorm(Forestfire$monthfeb)
qqline(Forestfire$monthfeb)
library(psych)
pnorm(Forestfire$monthfeb,-0.03868472,0.1930292)
---------------------------------------------------------------
##Measures of Central Tendency                monthjan
mean(Forestfire$monthjan)  ####   -0.003868472
median(Forestfire$monthjan)##### 0
getmode(Forestfire$monthjan)  ###### 0
#Measures of Dispersion
var(Forestfire$monthjan)  ######  0.003860975
sd(Forestfire$monthjan)  ####  0.06213674
range(Forestfire$monthjan)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthjan) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthjan)  #### 15.98449
#Measures of Kurtosis
kurtosis(Forestfire$monthjan)###### 256.5039
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthjan,horizontal = TRUE)
hist(Forestfire$monthjan)
barplot(Forestfire$monthjan)
#qqplot
qqnorm(Forestfire$monthjan)
qqline(Forestfire$monthjan)
library(psych)
pnorm(Forestfire$monthjan,-0.003868472,0.06213674)
-----------------------------------------------------------------
##Measures of Central Tendency                monthjul
mean(Forestfire$monthjul)  #### -0.06189555
median(Forestfire$monthjul)##### 0
getmode(Forestfire$monthjul)  ###### 0
#Measures of Dispersion
var(Forestfire$monthjul)  ######  0.05817702
sd(Forestfire$monthjul)  ####  0.2411991
range(Forestfire$monthjul)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthjul) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthjul)  #### 3.636238
#Measures of Kurtosis
kurtosis(Forestfire$monthjul)###### 14.22223
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthjul,horizontal = TRUE)
hist(Forestfire$monthjul)
barplot(Forestfire$monthjul)
#qqplot
qqnorm(Forestfire$monthjul)
qqline(Forestfire$monthjul)
library(psych)
pnorm(Forestfire$monthjul,-0.06189555,0.2411991)
-----------------------------------------------------------------
##Measures of Central Tendency                monthjun
mean(Forestfire$monthjun)  #### -0.03288201
median(Forestfire$monthjun)##### 0
getmode(Forestfire$monthjun)  ###### 0
#Measures of Dispersion
var(Forestfire$monthjun)  ###### 0.03186241
sd(Forestfire$monthjun)  ####  0.1785005
range(Forestfire$monthjun)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthjun) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthjun)  #### 5.238871
#Measures of Kurtosis
kurtosis(Forestfire$monthjun)###### 28.44576
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthjun,horizontal = TRUE)
hist(Forestfire$monthjun)
barplot(Forestfire$monthjun)
#qqplot
qqnorm(Forestfire$monthjun)
qqline(Forestfire$monthjun)
library(psych)
pnorm(Forestfire$monthjun,-0.03288201,0.1785005)
---------------------------------------------------------------------
##Measures of Central Tendency                monthmar
mean(Forestfire$monthmar)  #### -0.1044487
median(Forestfire$monthmar)##### 0
getmode(Forestfire$monthmar)  ###### 0
#Measures of Dispersion
var(Forestfire$monthmar)  ###### 0.09372048
sd(Forestfire$monthmar)  ####  0.306138
range(Forestfire$monthmar)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthmar) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthmar)  #### 2.58664
#Measures of Kurtosis
kurtosis(Forestfire$monthmar)###### 7.690705
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthmar,horizontal = TRUE)
hist(Forestfire$monthmar)
barplot(Forestfire$monthmar)
#qqplot
qqnorm(Forestfire$monthmar)
qqline(Forestfire$monthmar)
library(psych)
pnorm(Forestfire$monthmar,-0.1044487,0.306138)
---------------------------------------------------------------
##Measures of Central Tendency                monthmay
mean(Forestfire$monthmay)  #### -0.003868472
median(Forestfire$monthmay)##### 0
getmode(Forestfire$monthmay)  ###### 0
#Measures of Dispersion
var(Forestfire$monthmay)  ###### 0.003860975
sd(Forestfire$monthmay)  ####  0.06213674
range(Forestfire$monthmay)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthmay) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthmay)  #### 15.98449
#Measures of Kurtosis
kurtosis(Forestfire$monthmay)######  256.5039
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthmay,horizontal = TRUE)
hist(Forestfire$monthmay)
barplot(Forestfire$monthmay)
#qqplot
qqnorm(Forestfire$monthmay)
qqline(Forestfire$monthmay)
library(psych)
pnorm(Forestfire$monthmay,-0.003868472,0.06213674)
------------------------------------------------------------------
##Measures of Central Tendency                monthnov
mean(Forestfire$monthnov)  #### 0.001934236
median(Forestfire$monthnov)##### 0
getmode(Forestfire$monthnov)  ###### 0
#Measures of Dispersion
var(Forestfire$monthnov)  ###### 0.001934236
sd(Forestfire$monthnov)  ####  0.04397995
range(Forestfire$monthnov)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthnov) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthnov)  #### 22.671161
#Measures of Kurtosis
kurtosis(Forestfire$monthnov)######  515.0019
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthnov,horizontal = TRUE)
hist(Forestfire$monthnov)
barplot(Forestfire$monthnov)
#qqplot
qqnorm(Forestfire$monthnov)
qqline(Forestfire$monthnov)
library(psych)
pnorm(Forestfire$monthnov,0.001934236,0.04397995)
--------------------------------------------------------------
##Measures of Central Tendency                monthoct
mean(Forestfire$monthoct)  #### 0.02901354
median(Forestfire$monthoct)##### 0
getmode(Forestfire$monthoct)  ###### 0
#Measures of Dispersion
var(Forestfire$monthoct)  ###### 0.02822635
sd(Forestfire$monthoct)  ####  0.168007
range(Forestfire$monthoct)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthoct) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthoct)  #### 5.612178
#Measures of Kurtosis
kurtosis(Forestfire$monthoct)######  32.49655
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthoct,horizontal = TRUE)
hist(Forestfire$monthoct)
barplot(Forestfire$monthoct)
#qqplot
qqnorm(Forestfire$monthoct)
qqline(Forestfire$monthoct)
library(psych)
pnorm(Forestfire$monthoct,0.02901354,0.168007)
---------------------------------------------------------------
##Measures of Central Tendency                monthsep
mean(Forestfire$monthsep)  #### -0.3326886
median(Forestfire$monthsep)##### 0
getmode(Forestfire$monthsep)  ###### 0
#Measures of Dispersion
var(Forestfire$monthsep)  ###### 0.2224371
sd(Forestfire$monthsep)  ####  0.4716324
range(Forestfire$monthsep)##### 0  1
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Forestfire$monthsep) ##### 1
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Forestfire$monthsep)  #### 0.7101864
#Measures of Kurtosis
kurtosis(Forestfire$monthsep)######  1.504365
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Forestfire$monthsep,horizontal = TRUE)
hist(Forestfire$monthsep)
barplot(Forestfire$monthsep)
#qqplot
qqnorm(Forestfire$monthsep)
qqline(Forestfire$monthsep)
library(psych)
pnorm(Forestfire$monthsep,-0.3326886,0.4716324)
--------------------------------------------------------
#Visualization - Plot and ggplot for training data
ggplot(data=Forestfire_train,aes(x=Forestfire_train$size_category, y = Forestfire_train$temp, fill = Forestfire_train$size_category))+geom_boxplot() +  ggtitle("Box Plot")
ggplot(data=Forestfire_train,aes(x=Forestfire_train$size_category, y = Forestfire_train$RH, fill = Forestfire_train$size_category))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=Forestfire_train,aes(x=Forestfire_train$size_category, y = Forestfire_train$wind, fill = Forestfire_train$size_category))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=Forestfire_train,aes(x=Forestfire_train$size_category, y = Forestfire_train$rain, fill = Forestfire_train$size_category))+geom_boxplot()+ggtitle("Box Plot")
plot(Forestfire_train$temp,Forestfire_train$size_category)
plot(Forestfire_train$RH,Forestfire_train$size_category)
plot(Forestfire_train$wind,Forestfire_train$size_category)
plot(Forestfire_train$rain,Forestfire_train$size_category)
#Density Plot 
ggplot(data=Forestfire_train,aes(x = Forestfire_train$temp, fill = Forestfire_train$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("temp - Density Plot")
ggplot(data=Forestfire_train,aes(x = Forestfire_train$RH, fill = Forestfire_train$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("RH Density Plot")
ggplot(data=Forestfire_train,aes(x = Forestfire_train$wind, fill = Forestfire_train$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("wind Density Plot")
ggplot(data=Forestfire_train,aes(x = Forestfire_train$rain, fill = Forestfire_train$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("rain Density Plot")
#Visualization - Plot and ggplot for test data
ggplot(data=Forestfire_test,aes(x=Forestfire_test$size_category, y = Forestfire_test$temp, fill = Forestfire_test$size_category))+geom_boxplot() +  ggtitle("Box Plot")
ggplot(data=Forestfire_test,aes(x=Forestfire_test$size_category, y = Forestfire_test$RH, fill = Forestfire_test$size_category))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=Forestfire_test,aes(x=Forestfire_test$size_category, y = Forestfire_test$wind, fill = Forestfire_test$size_category))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=Forestfire_test,aes(x=Forestfire_test$size_category, y = Forestfire_test$rain, fill = Forestfire_test$size_category))+geom_boxplot()+ggtitle("Box Plot")
plot(Forestfire_test$temp,Forestfire_test$size_category)
plot(Forestfire_test$RH,Forestfire_test$size_category)
plot(Forestfire_test$wind,Forestfire_test$size_category)
plot(Forestfire_test$rain,Forestfire_test$size_category)
#Density Plot 
ggplot(data=Forestfire_test,aes(x = Forestfire_test$temp, fill = Forestfire_test$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("temp - Density Plot")
ggplot(data=Forestfire_test,aes(x = Forestfire_test$RH, fill = Forestfire_test$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("RH Density Plot")
ggplot(data=Forestfire_test,aes(x = Forestfire_test$wind, fill = Forestfire_test$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("wind Density Plot")
ggplot(data=Forestfire_test,aes(x = Forestfire_test$rain, fill = Forestfire_test$size_category))+geom_density(alpha = 0.9, color = 'Violet')
ggtitle("rain Density Plot")
-----------------------------------------------------------
# For SVM all the features must be in numeric 
# All the feature values should be in same range 
# If not we should normalize 
# SVM model will perform Rescaling automatically
library(kernlab)
library(caret)
library(plyr)
class(Forestfire)
str(Forestfire)
hist(Forestfire$area)  ####Area has lot of zeroes
rug(Forestfire$area)
####Transform area value to x
Forestfire1<-mutate(Forestfire, x = log(area + 1))
hist(Forestfire1$x)
summary(Forestfire)
# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed
# Apply Normalization technique to the whole dataset :
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Forestfire$temp = normalize(Forestfire$temp)
Forestfire$RH   = normalize(Forestfire$RH)
Forestfire$wind = normalize(Forestfire$wind)
Forestfire$rain = normalize(Forestfire$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Forestfire), replace = TRUE, prob = c(0.7,0.3))
Forestfire_train <- Forestfire[ind==1,]
Forestfire_test  <- Forestfire[ind==2,]
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 
# kvsm() function uses gaussian RBF kernel 
# Building model 
model1<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "vanilladot")
model1
Area_pred <- predict(model1, Forestfire_test)
table(Area_pred,Forestfire_test$size_category)
agreement <- Area_pred == Forestfire_test$size_category
table(agreement)
prop.table(table(agreement))
mean(Area_pred==Forestfire_test$size_category) ###75.59%
################################################################
### kernel = rbfdot 
model_rbfdot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "rbfdot")
pred_rbfdot<-predict(model_rbfdot,newdata=Forestfire_test)
mean(pred_rbfdot==Forestfire_test$size_category) ###76.19%
################################################################
### kernel = besseldot 
model_besseldot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "besseldot")
pred_besseldot<-predict(model_besseldot,newdata=Forestfire_test)
mean(pred_besseldot==Forestfire_test$size_category) ####75.59%
################################################################
### kernel = polydot 
model_polydot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "polydot")
pred_polydot<-predict(model_polydot,newdata=Forestfire_test)
mean(pred_polydot==Forestfire_test$size_category)  ####75.59%
##################################################################
### kernel = tanhdot 
model_tanhdot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "tanhdot")
pred_tanhdot<-predict(model_tanhdot,newdata=Forestfire_test)
mean(pred_tanhdot==Forestfire_test$size_category)  ###58.93%
#################################################################
### kernel = laplacedot 
model_laplacedot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "laplacedot")
pred_laplacedot<-predict(model_laplacedot,newdata=Forestfire_test)
mean(pred_laplacedot==Forestfire_test$size_category)  ####76.19%
###################################################################
### kernel = anovadot 
model_anovadot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "anovadot")
pred_anovadot<-predict(model_anovadot,newdata=Forestfire_test)
mean(pred_anovadot==Forestfire_test$size_category) ####75.59%
###################################################################
### kernel = splinedot 
model_splinedot<-ksvm(size_category~temp+rain+wind+RH,data= Forestfire_train,kernel = "splinedot")
pred_splinedot<-predict(model_splinedot,newdata=Forestfire_test)
mean(pred_splinedot==Forestfire_test$size_category)  ####59.52%

