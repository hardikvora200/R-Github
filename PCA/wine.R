Wine <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\PCA\\wine.csv")
View(Wine)
attach(Wine)
sum(is.na(Wine))  ##0
Wine <- na.omit(Wine) # Omitting NA values from the Data if it is there
dim(Wine)### 178  14
colnames(Wine)
set.seed(178)
library(class)
-------------------------------------------------------------------------------
#Measures of Central Tendency                Type
mean(Wine$Type)  ##  -1.938202
median(Wine$Type)##### 2
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Wine$Type)  ###### 2
#Measures of Dispersion
var(Wine$Type)  ###### 0.6006792
sd(Wine$Type)  #### 0.775035
range(Wine$Type)#####  1  3
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Type) ##### 2
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(Wine$Type)  #### 0.1065237
#Measures of Kurtosis 
kurtosis(Wine$Type)###### 1.68056
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(Wine$Type,horizontal = TRUE)
hist(Wine$Type)
barplot(Wine$Type)
str(Wine)
#qqplot
qqnorm(Wine$Type)
qqline(Wine$Type)
expType<-exp(Wine$Type)
qqnorm(expType)
qqline(expType)
logType<-log(Wine$Type)
qqnorm(logType)
qqline(logType)
recType<-(1/(Wine$Type))
qqnorm(recType)
qqline(recType)
library(psych)
describe(Wine)
pnorm(Wine$Type,-1.938202,0.775035)
-----------------------------------------------------------------------
#Measures of Central Tendency               #####Alcohol
mean(Wine$Alcohol)  ### -13.00062
median(Wine$Alcohol)##### 13.05
#mode
getmode(Wine$Alcohol)  #####  13.05
#Measures of Dispersion
var(Wine$Alcohol)  ###### 0.6590623
sd(Wine$Alcohol)  #### 0.8118265
range(Wine$Alcohol) ##### 11.03 14.83
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Alcohol) ##### 3.8
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Alcohol)  #### -0.05104747
#Measures of Kurtosis 
kurtosis(Wine$Alcohol)######   2.13774
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Alcohol,horizontal = TRUE)
hist(Wine$Alcohol)
barplot(Wine$Alcohol)
#qqplot
qqnorm(Wine$Alcohol)
qqline(Wine$Alcohol)
sqrtalc<-sqrt(Wine$Alcohol)
qqnorm(sqrtalc)
qqline(sqrtalc)
algalc<-((Wine$Alcohol) * (Wine$Alcohol)) + (Wine$Alcohol)
qqnorm(algalc)
qqline(algalc)
expalc<-exp(Wine$Alcohol)
qqnorm(expalc)
recalc<-(1/(Wine$Alcohol))
qqnorm(recalc)
rec2alc<-recalc * recalc
qqnorm(rec2alc)
qqline(rec2alc)
pnorm(Wine$Alcohol,-13.00062,0.8118265)
-----------------------------------------------------------------------    
#Measures of Central Tendency                Malic
mean(Wine$Malic)  ### -2.336348
median(Wine$Malic)  ##### 1.865
#mode
getmode(Wine$Malic)  ##### 1.73
#Measures of Dispersion
var(Wine$Malic)  ###### 1.248015
sd(Wine$Malic)  #### 1.117146
range(Wine$Malic) #####  0.74 5.80
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Malic) ##### 5.06
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Malic)  #### 1.030869
#Measures of Kurtosis 
kurtosis(Wine$Malic)  ###### 3.257348
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Malic,horizontal = TRUE)
hist(Wine$Malic)
barplot(Wine$Malic)
#qqplot
qqnorm(Wine$Malic)
qqline(Wine$Malic)
sqrtWineMalic<-sqrt(Wine$Malic)
qqnorm(sqrtWineMalic)
qqline(sqrtWineMalic)
expMalic<-exp(Wine$Malic)
qqnorm(expMalic)
qqline(expMalic)
pnorm(Wine$Malic,-2.336348,1.117146)
------------------------------------------------------------------
###Measures of Central Tendency                Malic
mean(Wine$Malic)  ### -2.336348
median(Wine$Malic)  ##### 1.865
#mode
getmode(Wine$Malic)  ##### 1.73
#Measures of Dispersion
var(Wine$Malic)  ###### 1.248015
sd(Wine$Malic)  #### 1.117146
range(Wine$Malic) #####  0.74 5.80
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Malic) ##### 5.06
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Malic)  #### 1.030869
#Measures of Kurtosis 
kurtosis(Wine$Malic)  ###### 3.257348
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Malic,horizontal = TRUE)
hist(Wine$Malic)
barplot(Wine$Malic)
#qqplot
qqnorm(Wine$Malic)
qqline(Wine$Malic)
sqrtWineMalic<-sqrt(Wine$Malic)
qqnorm(sqrtWineMalic)
qqline(sqrtWineMalic)
expMalic<-exp(Wine$Malic)
qqnorm(expMalic)
qqline(expMalic)
pnorm(Wine$Malic,-2.336348,1.117146)
---------------------------------------------------------------
###Measures of Central Tendency                Ash
mean(Wine$Ash)  ### -2.366517
median(Wine$Ash)  ##### 2.36
#mode
getmode(Wine$Ash)  ##### 2.3
#Measures of Dispersion
var(Wine$Ash)  ###### 0.07526464
sd(Wine$Ash)  #### 0.274344
range(Wine$Ash) #####  1.36  3.23
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Ash) ##### 1.87
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Ash)  #### -0.1752068
#Measures of Kurtosis 
kurtosis(Wine$Ash)  ######  4.078576
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Ash,horizontal = TRUE)
hist(Wine$Ash)
barplot(Wine$Ash)
#qqplot
qqnorm(Wine$Ash)
qqline(Wine$Ash)
pnorm(Wine$Ash,-2.366517,0.274344)
---------------------------------------------------------
###Measures of Central Tendency                Alcalinity
mean(Wine$Alcalinity)  ### -19.49494
median(Wine$Alcalinity)  ##### 19.5
#mode
getmode(Wine$Alcalinity)  ##### 20
#Measures of Dispersion
var(Wine$Alcalinity)  ###### 11.15269
sd(Wine$Alcalinity)  #### 3.339564
range(Wine$Alcalinity) #####  10.6 30.0
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Alcalinity) ##### 19.4
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Alcalinity)  #### 0.2112473
#Measures of Kurtosis 
kurtosis(Wine$Alcalinity)  ######  3.440823
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Alcalinity,horizontal = TRUE)
hist(Wine$Alcalinity)
barplot(Wine$Alcalinity)
#qqplot
qqnorm(Wine$Alcalinity)
qqline(Wine$Alcalinity)
pnorm(Wine$Alcalinity,-19.49494,3.339564)
-----------------------------------------------------------------------
###Measures of Central Tendency                Magnesium
mean(Wine$Magnesium)  ### -99.74157
median(Wine$Magnesium)  ##### 98
#mode
getmode(Wine$Magnesium)  ##### 88
#Measures of Dispersion
var(Wine$Magnesium)  ###### 203.9893
sd(Wine$Magnesium)  #### 14.28248
range(Wine$Magnesium) #####  70  162
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Magnesium) ##### 92
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Magnesium)  #### 1.088915
#Measures of Kurtosis 
kurtosis(Wine$Magnesium)  ######  5.012806
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Magnesium,horizontal = TRUE)
hist(Wine$Magnesium)
barplot(Wine$Magnesium)
#qqplot
qqnorm(Wine$Magnesium)
qqline(Wine$Magnesium)
logMag<-log(Wine$Magnesium)
qqnorm(logMag)
qqline(logMag)
pnorm(Wine$Magnesium,-99.74157,14.28248)
---------------------------------------------------------------
###Measures of Central Tendency                Phenols
mean(Wine$Phenols)  ### -2.295112
median(Wine$Phenols)  ##### 2.355
#mode
getmode(Wine$Phenols)  ##### 2.2
#Measures of Dispersion
var(Wine$Phenols)  ###### 0.3916895
sd(Wine$Phenols)  ####  0.625851
range(Wine$Phenols) #####  0.98 3.88
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Phenols) ##### 2.9
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Phenols)  #### 0.08590677
#Measures of Kurtosis 
kurtosis(Wine$Phenols)  ######  2.154143
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Phenols,horizontal = TRUE)
hist(Wine$Phenols)
barplot(Wine$Phenols)
#qqplot
qqnorm(Wine$Phenols)
qqline(Wine$Phenols)
logPhe<-log(Wine$Phenols)
qqnorm(logPhe)
qqline(logPhe)
expPhe<-exp(Wine$Phenols)
qqnorm(expPhe)
sqrtPhe<-sqrt(Wine$Phenols)
qqnorm(sqrtPhe)
qqline(sqrtPhe)
recPhe<-Wine$Phenols
qqnorm(recPhe)
qqline(recPhe)
rec2Phe<-recPhe * recPhe
qqnorm(rec2Phe)
qqline(rec2Phe)
pnorm(Wine$Phenols,-2.295112,0.625851)
--------------------------------------------------------------
###Measures of Central Tendency                Flavanoids
mean(Wine$Flavanoids)  ### 2.02927
median(Wine$Flavanoids)  ##### 2.135
#mode
getmode(Wine$Flavanoids)  ##### 2.65
#Measures of Dispersion
var(Wine$Flavanoids)  ###### 0.9977187
sd(Wine$Flavanoids)  ####  0.9988587
range(Wine$Flavanoids) #####0.34 5.08
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Flavanoids) ##### 4.74
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Flavanoids)  #### 0.02512948
#Measures of Kurtosis 
kurtosis(Wine$Flavanoids)  ###### 2.110635
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Flavanoids,horizontal = TRUE)
hist(Wine$Flavanoids)
barplot(Wine$Flavanoids)
#qqplot
qqnorm(Wine$Flavanoids)
qqline(Wine$Flavanoids)
logFlava<-log(Wine$Flavanoids)
qqnorm(logFlava)
qqline(logFlava)
expFlava<-exp(Wine$Flavanoids)
qqnorm(expFlava)
qqline(expFlava)
recFlava<-(1/(Wine$Flavanoids))
qqnorm(recFlava)
qqline(recFlava)
rec2Flava<-recFlava * recFlava
qqnorm(rec2Flava)
qqline(rec2Flava)
rec4Flava<-rec2Flava * rec2Flava
qqnorm(rec4Flava)
qqline(rec4Flava)
pnorm(Wine$Flavanoids,2.02927,0.9988587)
----------------------------------------------------------
###Measures of Central Tendency                NonFlavanoids
mean(Wine$Nonflavanoids)  ###  0.3618539
median(Wine$Nonflavanoids)  ##### 0.34
#mode
getmode(Wine$Nonflavanoids)  ##### 0.26
#Measures of Dispersion
var(Wine$Nonflavanoids)  ###### 0.01548863
sd(Wine$Nonflavanoids)  ####  0.1244533
range(Wine$Nonflavanoids) #####  0.13  0.66
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Nonflavanoids) ##### 0.53
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Nonflavanoids)  #### 0.446349
#Measures of Kurtosis 
kurtosis(Wine$Nonflavanoids)  ###### 2.347048
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Nonflavanoids,horizontal = TRUE)
hist(Wine$Nonflavanoids)
barplot(Wine$Nonflavanoids)
#qqplot
qqnorm(Wine$Nonflavanoids)
qqline(Wine$Nonflavanoids)
logNonFlava<-log(Wine$Nonflavanoids)
qqnorm(logNonFlava)
qqline(logNonFlava)
pnorm(Wine$Nonflavanoids,0.3618539,0.1244533)
-----------------------------------------------------------------
###Measures of Central Tendency                Proanthocyanins
mean(Wine$Proanthocyanins)  ###  -1.590899
median(Wine$Proanthocyanins)  ##### 1.555
#mode
getmode(Wine$Proanthocyanins)  ##### 1.35
#Measures of Dispersion
var(Wine$Proanthocyanins)  ###### 0.3275947
sd(Wine$Proanthocyanins)  ####  0.5723589
range(Wine$Proanthocyanins) #####  0.41  3.58
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Proanthocyanins) ##### 3.17
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Proanthocyanins)  #### 0.512769
#Measures of Kurtosis 
kurtosis(Wine$Proanthocyanins)  ###### 3.505671
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Proanthocyanins,horizontal = TRUE)
hist(Wine$Proanthocyanins)
barplot(Wine$Proanthocyanins)
#qqplot
qqnorm(Wine$Proanthocyanins)
qqline(Wine$Proanthocyanins)
pnorm(Wine$Proanthocyanins,-1.590899,0.5723589)
-------------------------------------------------------------------
###Measures of Central Tendency                Color
mean(Wine$Color)  ###   -5.05809
median(Wine$Color)  ##### 4.69
#mode
getmode(Wine$Color)  ##### 3.8
#Measures of Dispersion
var(Wine$Color)  ######  5.374449
sd(Wine$Color)  ####  2.318286
range(Wine$Color) #####  1.28   13.00
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Color) ##### 11.72
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Color)  #### 0.8612481
#Measures of Kurtosis 
kurtosis(Wine$Color)  ###### 3.33737
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Color,horizontal = TRUE)
hist(Wine$Color)
barplot(Wine$Color)
#qqplot
qqnorm(Wine$Color)
qqline(Wine$Color)
pnorm(Wine$Color,-5.05809,2.318286)
------------------------------------------------------------------------
###Measures of Central Tendency                Hue
mean(Wine$Hue)  ###   0.9574494
median(Wine$Hue)  ##### 0.965
#mode
getmode(Wine$Hue)  ##### 1.04
#Measures of Dispersion
var(Wine$Hue)  ######  0.05224496
sd(Wine$Hue)  ####  0.2285716
range(Wine$Hue) #####  0.48   1.71
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Hue) ##### 1.23
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Hue)  ####  0.02091312
#Measures of Kurtosis 
kurtosis(Wine$Hue)  #####  2.631975
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Hue,horizontal = TRUE)
hist(Wine$Hue)
barplot(Wine$Hue)
#qqplot
qqnorm(Wine$Hue)
qqline(Wine$Hue)
pnorm(Wine$Hue,0.9574494,0.2285716)
-----------------------------------------------------------------
###Measures of Central Tendency                Dilution
mean(Wine$Dilution)  ###   -2.611685
median(Wine$Dilution)  ##### 2.78
#mode
getmode(Wine$Dilution)  ##### 2.87
#Measures of Dispersion
var(Wine$Dilution)  ######   0.5040864
sd(Wine$Dilution)  ####  0.7099904
range(Wine$Dilution) #####  1.27  4.00
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Dilution) ##### 2.73
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Dilution)  #### -0.3046899
#Measures of Kurtosis 
kurtosis(Wine$Dilution)  #####  1.910325
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Dilution,horizontal = TRUE)
hist(Wine$Dilution)
barplot(Wine$Dilution)
#qqplot
qqnorm(Wine$Dilution)
qqline(Wine$Dilution)
pnorm(Wine$Dilution,-2.611685,0.7099904)
----------------------------------------------------------------------
###Measures of Central Tendency                Proline
mean(Wine$Proline)  ### 746.8933
median(Wine$Proline)  ##### 673.5
#mode
getmode(Wine$Proline)  ##### 680
#Measures of Dispersion
var(Wine$Proline)  ######   99166.72
sd(Wine$Proline)  ####  314.9075
range(Wine$Proline) #####  278 1680
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Wine$Proline) ##### 1402
#Measures of skewness
library(moments)
#Measures of skewness
skewness(Wine$Proline)  #### 0.7613362
#Measures of Kurtosis 
kurtosis(Wine$Proline)  ##### 2.725
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(Wine$Proline,horizontal = TRUE)
hist(Wine$Proline)
barplot(Wine$Proline)
#qqplot
qqnorm(Wine$Proline)
qqline(Wine$Proline)
logProline<-log(Wine$Proline)
qqnorm(logProline)
qqline(logProline)
pnorm(Wine$Proline,746.8933,314.9075)
#################################################
?princomp()
cor(Wine) 
pcaObj<-prcomp(Wine[], cor = TRUE, scores = TRUE)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components
# Comp.1 having highest importance (highest variance)
biplot(pcaObj)
# It Shows the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$loadings
pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
Wine<-cbind(Wine,pcaObj$scores[,1:3])
View(Wine)
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-Wine[,15:17]
View(clus_data)
# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage
plot(fit1) # Displaying Dendrogram
groups1<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters
membership_1<-as.matrix(groups1) # cluster numbering 
View(membership_1)
final1<-cbind(membership_1,Wine) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="Wine_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit2<-hclust(dist1,method="single") # method here is single linkage
plot(fit2) # Displaying Dendrogram
groups2<-cutree(fit2,5) # Cutting the dendrogram for 5 clusters
membership_2<-as.matrix(groups2) # cluster numbering 
View(membership_2)
final2<-cbind(membership_2,Wine) # binding column wise with orginal data
View(final2)
View(aggregate(final2[,-c(16:18)],by=list(membership_2),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final2,file="Wine1_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit3<-hclust(dist1,method="average") # method here is average linkage
plot(fit3) # Displaying Dendrogram
groups3<-cutree(fit3,5) # Cutting the dendrogram for 5 clusters
membership_3<-as.matrix(groups3) # cluster numbering 
View(membership_3)
final3<-cbind(membership_3,Wine) # binding column wise with orginal data
View(final3)
View(aggregate(final3[,-c(16:18)],by=list(membership_3),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final3,file="Wine2_clustered.csv",row.names = F,col.names = F)
getwd()
#######################################################################
fit4<-hclust(dist1,method="centroid") # method here is centroid linkage
plot(fit4) # Displaying Dendrogram
groups4<-cutree(fit4,5) # Cutting the dendrogram for 5 clusters
membership_4<-as.matrix(groups4) # cluster numbering 
View(membership_4)
final4<-cbind(membership_4,Wine) # binding column wise with orginal data
View(final4)
View(aggregate(final4[,-c(16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final4,file="Wine3_clustered.csv",row.names = F,col.names = F)
getwd()
-----------------------------------------------------------------------------
dist2<-dist(norm_clus,method = "manhattan") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit5<-hclust(dist2,method="complete") # method here is complete linkage
plot(fit5) # Displaying Dendrogram
groups5<-cutree(fit5,4) # Cutting the dendrogram for 4 clusters
membership_5<-as.matrix(groups5) # cluster numbering 
View(membership_5)
final5<-cbind(membership_5,Wine) # binding column wise with orginal data
View(final5)
View(aggregate(final5[,-c(16:18)],by=list(membership_5),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final5,file="Wine4_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit6<-hclust(dist2,method="single") # method here is single linkage
plot(fit6) # Displaying Dendrogram
groups6<-cutree(fit6,3) # Cutting the dendrogram for 3 clusters
membership_6<-as.matrix(groups6) # cluster numbering 
View(membership_6)
final6<-cbind(membership_6,Wine) # binding column wise with orginal data
View(final6)
View(aggregate(final6[,-c(16:18)],by=list(membership_6),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final6,file="Wine5_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit7<-hclust(dist2,method="average") # method here is average linkage
plot(fit7) # Displaying Dendrogram
groups7<-cutree(fit7,7) # Cutting the dendrogram for 7 clusters
membership_7<-as.matrix(groups7) # cluster numbering 
View(membership_7)
final7<-cbind(membership_7,Wine) # binding column wise with orginal data
View(final7)
View(aggregate(final7[,-c(16:18)],by=list(membership_7),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final7,file="Wine6_clustered.csv",row.names = F,col.names = F)
getwd()
#######################################################################
fit8<-hclust(dist2,method="centroid") # method here is centroid linkage
plot(fit8) # Displaying Dendrogram
groups8<-cutree(fit8,5) # Cutting the dendrogram for 5 clusters
membership_8<-as.matrix(groups8) # cluster numbering 
View(membership_8)
final8<-cbind(membership_8,Wine) # binding column wise with orginal data
View(final8)
View(aggregate(final8[,-c(16:18)],by=list(membership_8),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final8,file="Wine7_clustered.csv",row.names = F,col.names = F)
getwd()
----------------------------------------------------------------------------
dist3<-dist(norm_clus,method = "canberra") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit9<-hclust(dist3,method="complete") # method here is complete linkage
plot(fit9) # Displaying Dendrogram
groups9<-cutree(fit9,7) # Cutting the dendrogram for 7 clusters
membership_9<-as.matrix(groups9) # cluster numbering 
View(membership_9)
final9<-cbind(membership_9,Wine) # binding column wise with orginal data
View(final9)
View(aggregate(final9[,-c(16:18)],by=list(membership_9),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final9,file="Wine8_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit10<-hclust(dist3,method="single") # method here is single linkage
plot(fit10) # Displaying Dendrogram
groups10<-cutree(fit10,8) # Cutting the dendrogram for 8 clusters
membership_10<-as.matrix(groups10) # cluster numbering 
View(membership_10)
final10<-cbind(membership_10,Wine) # binding column wise with orginal data
View(final10)
View(aggregate(final10[,-c(16:18)],by=list(membership_10),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final10,file="Wine9_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit11<-hclust(dist3,method="average") # method here is average linkage
plot(fit11) # Displaying Dendrogram
groups11<-cutree(fit11,9) # Cutting the dendrogram for 9 clusters
membership_11<-as.matrix(groups11) # cluster numbering 
View(membership_11)
final11<-cbind(membership_11,Wine) # binding column wise with orginal data
View(final11)
View(aggregate(final11[,-c(16:18)],by=list(membership_11),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final11,file="Wine10_clustered.csv",row.names = F,col.names = F)
getwd()
#######################################################################
fit12<-hclust(dist3,method="centroid") # method here is centroid linkage
plot(fit12) # Displaying Dendrogram
groups12<-cutree(fit12,5) # Cutting the dendrogram for 5 clusters
membership_12<-as.matrix(groups12) # cluster numbering 
View(membership_12)
final12<-cbind(membership_12,Wine) # binding column wise with orginal data
View(final12)
View(aggregate(final12[,-c(16:18)],by=list(membership_12),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final12,file="Wine11_clustered.csv",row.names = F,col.names = F)
getwd()
-----------------------------------------------------------------------
dist4<-dist(norm_clus,method = "maximum") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit13<-hclust(dist4,method="complete") # method here is complete linkage
plot(fit13) # Displaying Dendrogram
groups13<-cutree(fit13,7) # Cutting the dendrogram for 7 clusters
membership_13<-as.matrix(groups13) # cluster numbering 
View(membership_13)
final13<-cbind(membership_13,Wine) # binding column wise with orginal data
View(final13)
View(aggregate(final13[,-c(16:18)],by=list(membership_13),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final13,file="Wine12_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit14<-hclust(dist4,method="single") # method here is single linkage
plot(fit14) # Displaying Dendrogram
groups14<-cutree(fit14,8) # Cutting the dendrogram for 8 clusters
membership_14<-as.matrix(groups14) # cluster numbering 
View(membership_14)
final14<-cbind(membership_14,Wine) # binding column wise with orginal data
View(final14)
View(aggregate(final14[,-c(16:18)],by=list(membership_14),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final14,file="Wine13_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit15<-hclust(dist4,method="average") # method here is average linkage
plot(fit15) # Displaying Dendrogram
groups15<-cutree(fit15,9) # Cutting the dendrogram for 9 clusters
membership_15<-as.matrix(groups15) # cluster numbering 
View(membership_15)
final15<-cbind(membership_15,Wine) # binding column wise with orginal data
View(final15)
View(aggregate(final15[,-c(16:18)],by=list(membership_15),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final15,file="Wine14_clustered.csv",row.names = F,col.names = F)
getwd()
######################################################################
fit16<-hclust(dist4,method="centroid") # method here is centroid linkage
plot(fit16) # Displaying Dendrogram
groups16<-cutree(fit16,5) # Cutting the dendrogram for 5 clusters
membership_16<-as.matrix(groups16) # cluster numbering 
View(membership_16)
final16<-cbind(membership_16,Wine) # binding column wise with orginal data
View(final16)
View(aggregate(final16[,-c(16:18)],by=list(membership_16),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final16,file="Wine15_clustered.csv",row.names = F,col.names = F)
getwd()
---------------------------------------------------------------
dist5<-dist(norm_clus,method = "binary") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit17<-hclust(dist5,method="complete") # method here is complete linkage
plot(fit17) # Displaying Dendrogram
groups17<-cutree(fit17,7) # Cutting the dendrogram for 7 clusters
membership_17<-as.matrix(groups17) # cluster numbering 
View(membership_17)
final17<-cbind(membership_17,Wine) # binding column wise with orginal data
View(final17)
View(aggregate(final17[,-c(16:18)],by=list(membership_17),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final17,file="Wine16_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit18<-hclust(dist5,method="single") # method here is single linkage
plot(fit18) # Displaying Dendrogram
groups18<-cutree(fit18,4) # Cutting the dendrogram for 4 clusters
membership_18<-as.matrix(groups18) # cluster numbering 
View(membership_18)
final18<-cbind(membership_18,Wine) # binding column wise with orginal data
View(final18)
View(aggregate(final18[,-c(16:18)],by=list(membership_18),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final18,file="Wine17_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit19<-hclust(dist5,method="average") # method here is average linkage
plot(fit19) # Displaying Dendrogram
groups19<-cutree(fit19,3) # Cutting the dendrogram for 3 clusters
membership_19<-as.matrix(groups19) # cluster numbering 
View(membership_19)
final19<-cbind(membership_19,Wine) # binding column wise with orginal data
View(final19)
View(aggregate(final19[,-c(16:18)],by=list(membership_19),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final19,file="Wine18_clustered.csv",row.names = F,col.names = F)
getwd()
######################################################################
fit20<-hclust(dist5,method="centroid") # method here is centroid linkage
plot(fit20) # Displaying Dendrogram
groups20<-cutree(fit20,4) # Cutting the dendrogram for 4 clusters
membership_20<-as.matrix(groups20) # cluster numbering 
View(membership_20)
final20<-cbind(membership_20,Wine) # binding column wise with orginal data
View(final20)
View(aggregate(final20[,-c(16:18)],by=list(membership_20),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final20,file="Wine19_clustered.csv",row.names = F,col.names = F)
getwd()
----------------------------------------------------------------------
dist6<-dist(norm_clus,method = "minkowski") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
fit21<-hclust(dist6,method="complete") # method here is complete linkage
plot(fit21) # Displaying Dendrogram
groups21<-cutree(fit21,7) # Cutting the dendrogram for 7 clusters
membership_21<-as.matrix(groups21) # cluster numbering 
View(membership_21)
final21<-cbind(membership_21,Wine) # binding column wise with orginal data
View(final21)
View(aggregate(final21[,-c(16:18)],by=list(membership_21),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final21,file="Wine20_clustered.csv",row.names = F,col.names = F)
getwd()
#################################################################
fit22<-hclust(dist6,method="single") # method here is single linkage
plot(fit22) # Displaying Dendrogram
groups22<-cutree(fit22,4) # Cutting the dendrogram for 4 clusters
membership_22<-as.matrix(groups22) # cluster numbering 
View(membership_22)
final22<-cbind(membership_22,Wine) # binding column wise with orginal data
View(final22)
View(aggregate(final22[,-c(16:18)],by=list(membership_22),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final22,file="Wine21_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
fit23<-hclust(dist6,method="average") # method here is average linkage
plot(fit23) # Displaying Dendrogram
groups23<-cutree(fit23,3) # Cutting the dendrogram for 3 clusters
membership_23<-as.matrix(groups23) # cluster numbering 
View(membership_23)
final23<-cbind(membership_23,Wine) # binding column wise with orginal data
View(final23)
View(aggregate(final23[,-c(16:18)],by=list(membership_23),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final23,file="Wine22_clustered.csv",row.names = F,col.names = F)
getwd()
######################################################################
fit24<-hclust(dist6,method="centroid") # method here is centroid linkage
plot(fit24) # Displaying Dendrogram
groups24<-cutree(fit24,4) # Cutting the dendrogram for 4 clusters
membership_24<-as.matrix(groups24) # cluster numbering 
View(membership_24)
final24<-cbind(membership_24,Wine) # binding column wise with orginal data
View(final24)
View(aggregate(final24[,-c(16:18)],by=list(membership_24),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final24,file="Wine23_clustered.csv",row.names = F,col.names = F)
getwd()
#####################################################################
####Lets see K Means clustering
View(Wine)
text(Wine,rownames(Wine))
plot(Wine)
km<-kmeans(Wine,7)  ###Randomly 7 clusters were chosen
str(km)
install.packages("animation")
library(animation)
km1<-kmeans.ani(Wine,7)
str(km1)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Wine[,1:17]) 
View(normalized_data)
#####Our objective is to find what is happening behind the scenes
###Elbow curve and k~sqrt(n/2) will decide the 'k' value
###In this way the distance between each and every data point for each and every centroid
## of cluster is calculated
##To which cluster centroid, a specific data point is close to it, it will go and form a cluster with it
wssplot <- function(normalized_data, nc=12, seed=178){
  wss <- (nrow(normalized_data)-1)*sum(apply(data,2,var))
  ###Determine number of clusters
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
title(sub="k-Means clustering Scree-plot")                         
####Selecting k for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
k<-kselection(Wine[,],parallel=TRUE,k_threshold=0.9)
?kselection
View(Wine)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k<-kselection(Wine[,],parallel=TRUE,k_threshold=0.9)
k
####f(k) finds 2 cluster
View()
mydata<-Wine[1:178,1:17]
View(mydata)
normalized_data<-scale(mydata[,2:17])
View(normalized_data)
fit<-kmeans(normalized_data,10)  #####14 cluster solution
str(fit)
final2<-data.frame(mydata,fit$cluster)
final2


  