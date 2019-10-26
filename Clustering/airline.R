airline <- read_excel("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Clustering\\EastWestAirlines.xlsx")
View(airline)
attach(airline)
sum(is.na(airline))
airline <- na.omit(airline) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(airline)
colnames(airline)
-------------------------------------------------
#Measures of Central Tendency                Balance
mean(airline$Balance)  ### -73601.33
median(airline$Balance)#####  43097
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(airline$Balance)  ######   1000
#Measures of Dispersion
var(airline$Balance)  ######  10155734648
sd(airline$Balance)  ####  100775.7
range(airline$Balance)##### 0 1704838
rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Balance) #####  1704838
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(airline$Balance)  #### 5.00231
#Measures of Kurtosis 
kurtosis(airline$Balance)######   47.10124
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(airline$Balance,horizontal = TRUE)
hist(airline$Balance)
barplot(airline$Balance)
str(airline)
#qqplot
qqnorm(airline$Balance)
qqline(airline$Balance)
sqrtbal<-sqrt(airline$Balance)
qqnorm(sqrtbal)
qqline(sqrtbal)
sqrbal<-(airline$Balance * airline$Balance)
qqnorm(sqrbal)
qqline(sqrbal)  ####Normalisation achieved
install.packages(psych)
library(psych)
psych::describe(airline)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(airline$Balance,-73601.33,100775.7)
-----------------------------------------------------------------------
#Measures of Central Tendency                Qual_miles
mean(airline$Qual_miles)  ### -144.1145
median(airline$Qual_miles)##### 0
#mode
getmode(airline$Qual_miles)  ##### 0
#Measures of Dispersion
var(airline$Qual_miles)  ###### 598555.7
sd(airline$Qual_miles)  ####  773.6638
range(airline$Qual_miles) #####   0    11148 
rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Qual_miles) ##### 11148
#Measures of skewness
library(moments)
#Measures of skewness
skewness(airline$Qual_miles)  ###  7.509577
#Measures of Kurtosis 
kurtosis(airline$Qual_miles)###### 70.60325
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Qual_miles,horizontal = TRUE)
hist(airline$Qual_mile)
barplot(airline$Qual_miles)
#qqplot
qqnorm(airline$Qual_miles)
qqline(airline$Qual_miles)
sqrtmile<-sqrt(airline$Qual_miles)
qqnorm(sqrtmile)
qqline(sqrtmile)  ###Normalisation achieved
pnorm(airline$Qual_miles,-144.1145,773.6638)
-----------------------------------------------------------------------    
#Measures of Central Tendency                cc1_miles
mean(airline$cc1_miles)  ### -2.059515
median(airline$cc1_miles)##### 1
#mode
getmode(airline$cc1_miles)  ###### 1
#Measures of Dispersion
var(airline$cc1_miles)  ######  1.895907
sd(airline$cc1_miles)  #### 1.376919
range(airline$cc1_miles)##### 1  5
rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$cc1_miles) ####  4
#Measures of skewness
skewness(airline$cc1_miles)  ### 0.8572472
#Measures of Kurtosis 
kurtosis(airline$cc1_miles)######  2.250927
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$cc1_miles,horizontal = TRUE)
hist(airline$cc1_miles)
barplot(airline$cc1_miles)
#qqplot
qqnorm(airline$cc1_miles)
qqline(airline$cc1_miles)
expcc1<-exp(airline$cc1_miles)
qqnorm(expcc1)
qqline(expcc1)  
logcc1<-log(airline$cc1_miles)
qqnorm(logcc1)
sqrcc1<-airline$cc1_miles * airline$cc1_miles
qqnorm(sqrcc1)
sqrtcc1<-sqrt(airline$cc1_miles)
qqnorm(sqrtcc1)
qqline(sqrtcc1)
rec_cc1<-(1/(airline$cc1_miles))
qqnorm(rec_cc1)
rec2_cc1<-(1/(airline$cc1_miles * airline$cc1_miles))
qqnorm(rec2_cc1) 
pnorm(airline$cc1_miles,-2.059515,1.376919)
------------------------------------------------------------------
#Measures of Central Tendency                cc2_miles
mean(airline$cc2_miles)  ### 1.014504 
median(airline$cc2_miles)##### 1
#mode
getmode(airline$cc2_miles)  ###### 1
#Measures of Dispersion
var(airline$cc2_miles)  ###### 0.0218006
sd(airline$cc2_miles)  #### 0.1476503
range(airline$cc2_miles)##### 1 3
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$cc2_miles) #### 2
#Measures of skewness
skewness(airline$cc2_miles)  ### 11.20205
#Measures of Kurtosis 
kurtosis(airline$cc2_miles)###### 133.5495
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$cc2_miles,horizontal = TRUE)
hist(airline$cc2_miles)
barplot(airline$cc2_miles)
##str(affair)
#qqplot
qqnorm(airline$cc2_miles)
qqline(airline$cc2_miles)
expcc2<-exp(airline$cc2_miles)
qqnorm(expcc2)
qqline(expcc2)  ###Normalisation achieved
pnorm(airline$cc2_miles,1.014504,0.1476503)
------------------------------------------------------------------------
#Measures of Central Tendency                cc3_miles
mean(airline$cc3_miles)  ### 1.012253 
median(airline$cc3_miles)##### 1
#mode
getmode(airline$cc3_miles)  ###### 1
#Measures of Dispersion
var(airline$cc3_miles)  ###### 0.03811896
sd(airline$cc3_miles)  #### 0.1952408
range(airline$cc3_miles)##### 1 5
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$cc3_miles) #### 4
#Measures of skewness
skewness(airline$cc3_miles)  ### 17.18263
#Measures of Kurtosis 
kurtosis(airline$cc3_miles)###### 308.1118
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$cc3_miles,horizontal = TRUE)
hist(airline$cc3_miles)
barplot(airline$cc3_miles)
##str(affair)
#qqplot
qqnorm(airline$cc3_miles)
qqline(airline$cc3_miles)
expcc3<-exp(airline$cc3_miles)
qqnorm(expcc3)
qqline(expcc3)  ###Normalisation achieved
pnorm(airline$cc3_miles,1.012253,0.1952408)
----------------------------------------------------------------
#Measures of Central Tendency                Bonus_miles
mean(airline$Bonus_miles)  ### 17144.85 
median(airline$Bonus_miles)##### 7171
#mode
getmode(airline$Bonus_miles)  ###### 0
#Measures of Dispersion
var(airline$Bonus_miles)  ###### 583269247
sd(airline$Bonus_miles)  #### 24150.97
range(airline$Bonus_miles)##### 0  263685
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Bonus_miles) #### 263685
#Measures of skewness
skewness(airline$Bonus_miles)  ### 2.839962
#Measures of Kurtosis 
kurtosis(airline$Bonus_miles)###### 13.60365
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Bonus_miles,horizontal = TRUE)
hist(airline$Bonus_miles)
barplot(airline$Bonus_miles)
##str(affair)
#qqplot
qqnorm(airline$Bonus_miles)
qqline(airline$Bonus_miles)
sqrtbonus<-sqrt(airline$Bonus_miles)
qqnorm(sqrtbonus)
qqline(sqrtbonus)###Normalisation achieved  
pnorm(airline$Bonus_miles,17144.85,24150.97)
-------------------------------------------------------------------------
#Measures of Central Tendency                Bonus_trans
mean(airline$Bonus_trans)  ### -11.6019 
median(airline$Bonus_trans)##### 12
#mode
getmode(airline$Bonus_trans)  ###### 0
#Measures of Dispersion
var(airline$Bonus_trans)  ###### 92.23317
sd(airline$Bonus_trans)  #### 9.60381
range(airline$Bonus_trans)##### 0 86
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Bonus_trans) #### 86
#Measures of skewness
skewness(airline$Bonus_trans)  ### 1.156494
#Measures of Kurtosis 
kurtosis(airline$Bonus_trans)###### 2.737935
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Bonus_trans,horizontal = TRUE)
hist(airline$Bonus_trans)
barplot(airline$Bonus_trans)
##str(affair)
#qqplot
qqnorm(airline$Bonus_trans)
qqline(airline$Bonus_trans)
expbonus<-exp(airline$Bonus_trans)
qqnorm(expbonus)
qqline(expbonus)####Normalisation achieved  
pnorm(airline$Bonus_trans,-11.6019,9.60381)
---------------------------------------------------------------
#Measures of Central Tendency                Flight_miles_12mo
mean(airline$Flight_miles_12mo)  ### -460.0558 
median(airline$Flight_miles_12mo)##### 0
#mode
getmode(airline$Flight_miles_12mo)  ###### 0
#Measures of Dispersion
var(airline$Flight_miles_12mo)  ###### 1960586
sd(airline$Flight_miles_12mo)  #### 1400.209
range(airline$Flight_miles_12mo)##### 0 30817
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Flight_miles_12mo) #### 30817
#Measures of skewness
skewness(airline$Flight_miles_12mo)  ### 7.446077
#Measures of Kurtosis 
kurtosis(airline$Flight_miles_12mo)######  94.59225
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Flight_miles_12mo,horizontal = TRUE)
hist(airline$Flight_miles_12mo)
barplot(airline$Flight_miles_12mo)
##str(affair)
#qqplot
qqnorm(airline$Flight_miles_12mo)
qqline(airline$Flight_miles_12mo)  ##Normalisation looks good
pnorm(airline$Flight_miles_12mo,-460.0558,1400.209)
--------------------------------------------------------
#Measures of Central Tendency                Flight_trans_12
mean(airline$Flight_trans_12)  ### 1.373593 
median(airline$Flight_trans_12)##### 0
#mode
getmode(airline$Flight_trans_12)  ###### 0
#Measures of Dispersion
var(airline$Flight_trans_12)  ###### 14.38816
sd(airline$Flight_trans_12)  #### 3.793172
range(airline$Flight_trans_12)##### 0 53
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Flight_trans_12) #### 53
#Measures of skewness
skewness(airline$Flight_trans_12)  ### 5.486343
#Measures of Kurtosis 
kurtosis(airline$Flight_trans_12)######  42.89997
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Flight_trans_12,horizontal = TRUE)
hist(airline$Flight_trans_12)
barplot(airline$Flight_trans_12)
##str(affair)
#qqplot
qqnorm(airline$Flight_trans_12)
qqline(airline$Flight_trans_12)  
expflitrans<-exp(airline$Flight_trans_12)
qqnorm(expflitrans)
qqline(expflitrans)##Normalisation looks good
pnorm(airline$Flight_trans_12,1.373593,3.793172)
--------------------------------------------------------------
#Measures of Central Tendency                Days_since_enroll
mean(airline$Days_since_enroll)  ### 4118.559 
median(airline$Days_since_enroll)##### 4096
#mode
getmode(airline$Days_since_enroll)  ###### 8296
#Measures of Dispersion
var(airline$Days_since_enroll)  ###### 4264781
sd(airline$Days_since_enroll)  #### 2065.135
range(airline$Days_since_enroll)##### 2 8296
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Days_since_enroll) #### 8294
#Measures of skewness
skewness(airline$Days_since_enroll)  ### 0.1200834
#Measures of Kurtosis 
kurtosis(airline$Days_since_enroll)###### -0.9688123
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Days_since_enroll,horizontal = TRUE)
hist(airline$Days_since_enroll)
barplot(airline$Days_since_enroll)
#qqplot
qqnorm(airline$Days_since_enroll)
qqline(airline$Days_since_enroll)  
logdaysinen<-log(airline$Days_since_enroll)
qqnorm(logdaysinen)
qqline(logdaysinen)
sqrtdaysinen<-sqrt(airline$Days_since_enroll)
qqnorm(sqrtdaysinen)
qqline(sqrtdaysinen)  ##Normalisation looks good
pnorm(airline$Days_since_enroll,4118.559,2065.135)
----------------------------------------------------------------
#Measures of Central Tendency                Award
mean(airline$Award)  ### 0.3703426 
median(airline$Award)##### 0
#mode
getmode(airline$Award)  ###### 0
#Measures of Dispersion
var(airline$Award)  ###### 0.2332473
sd(airline$Award)  #### 0.4829568
range(airline$Award)##### 0 1
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(airline$Award) #### 1
#Measures of skewness
skewness(airline$Award)  ### 0.5367974
#Measures of Kurtosis 
kurtosis(airline$Award)###### -1.712276
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(airline$Award,horizontal = TRUE)
hist(airline$Award)
barplot(airline$Award)
#qqplot
qqnorm(airline$Award)
qqline(airline$Award)  
expAward<-exp(airline$Award)
qqnorm(expAward)
qqline(expAward)
pnorm(airline$Award,0.3703426,0.4829568)
?dist()
--------------------------------------------------------------------
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(airline[,2:12]) #excluding the ID# columnbefore normalizing
View(normalized_data)
------------------------------------------------------------------------------------
d1 <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d1, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit, k=9, border="red")
groups <- cutree(fit, k=9) # cut tree into 9 clusters
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(airline, membership)
View(final)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1) ###Combine all the columns of final and place membership column in 1st column
write.csv(final, file="final.csv",row.names = F)
aggregate(airline[,-1],by=list(final$membership),mean)
################################
fit_1<-hclust(d1, method="single")
plot(fit_1) # display dendrogram
plot(fit_1, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_1, k=9, border="red")
groups_1 <- cutree(fit_1, k=9) # cut tree into 9 clusters
membership_1<-as.matrix(groups_1) # groups or cluster numbers
final_1 <- data.frame(airline, membership_1)
View(final_1)
final1_1<-final_1[,c(ncol(final_1),1:(ncol(final_1)-1))]
View(final1_1) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_1, file="final_1.csv",row.names = F)
aggregate(airline[,-1],by=list(final_1$membership_1),mean)
#######################################
fit_2<-hclust(d1,method="average")
plot(fit_2) # display dendrogram
plot(fit_2, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_2, k=9, border="red")
groups_2 <- cutree(fit_2, k=9) # cut tree into 9 clusters
membership_2<-as.matrix(groups_2) # groups or cluster numbers
final_2 <- data.frame(airline, membership_2)
View(final_2)
final1_2<-final_2[,c(ncol(final_2),1:(ncol(final_2)-1))]
View(final1_1) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_2, file="final_2.csv",row.names = F)
aggregate(airline[,-1],by=list(final_2$membership_2),mean)
#############################################
fit_3<-hclust(d1,method="centroid")
plot(fit_3) # display dendrogram
plot(fit_3, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_3, k=9, border="red")
groups_3 <- cutree(fit_3, k=9) # cut tree into 9 clusters
membership_3<-as.matrix(groups_3) # groups or cluster numbers
final_3 <- data.frame(airline, membership_3)
View(final_3)
final1_3<-final_3[,c(ncol(final_3),1:(ncol(final_3)-1))]
View(final1_3) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_3, file="final_3.csv",row.names = F)
aggregate(airline[,-1],by=list(final_3$membership_3),mean)
-----------------------------------------------------------------------------
d2 <- dist(normalized_data, method = "manhattan") # distance matrix for manhattan
fit_4 <- hclust(d2, method="complete")
plot(fit_4) # display dendrogram
plot(fit_4, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_4, k=11, border="red")
groups_4 <- cutree(fit_4, k=11) # cut tree into 11 clusters
membership_4<-as.matrix(groups_4) # groups or cluster numbers
final_4 <- data.frame(airline, membership_4)
View(final_4)
final1_4<-final[,c(ncol(final_4),1:(ncol(final_4)-1))]
View(final1_4) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_4, file="final_4.csv",row.names = F)
aggregate(airline[,-1],by=list(final_4$membership_4),mean)
######################################################################
fit_5 <- hclust(d2, method="single")
plot(fit_5) # display dendrogram
plot(fit_5, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_5, k=9, border="red")
groups_5 <- cutree(fit_5, k=9) # cut tree into 9 clusters
membership_5<-as.matrix(groups_5) # groups or cluster numbers
final_5 <- data.frame(airline, membership_5)
View(final_5)
final1_5<-final_5[,c(ncol(final_5),1:(ncol(final_5)-1))]
View(final1_5) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_5, file="final_5.csv",row.names = F)
aggregate(airline[,-1],by=list(final_5$membership_5),mean)
###############################################################
fit_6 <- hclust(d2, method="average")
plot(fit_6) # display dendrogram
plot(fit_6, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_6, k=9, border="red")
groups_6 <- cutree(fit_6, k=9) # cut tree into 9 clusters
membership_6<-as.matrix(groups_6) # groups or cluster numbers
final_6 <- data.frame(airline, membership_6)
View(final_6)
final1_6<-final_6[,c(ncol(final_6),1:(ncol(final_6)-1))]
View(final1_6) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_6, file="final_6.csv",row.names = F)
aggregate(airline[,-1],by=list(final_6$membership_6),mean)
#############################################################
fit_7 <- hclust(d2, method="centroid")
plot(fit_7) # display dendrogram
plot(fit_7, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_7, k=9, border="red")
groups_7 <- cutree(fit_7, k=9) # cut tree into 9 clusters
membership_7<-as.matrix(groups_7) # groups or cluster numbers
final_7 <- data.frame(airline, membership_7)
View(final_7)
final1_7<-final_7[,c(ncol(final_7),1:(ncol(final_7)-1))]
View(final1_7) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_7, file="final_7.csv",row.names = F)
aggregate(airline[,-1],by=list(final_7$membership_7),mean)
------------------------------------------------------------------------
d3 <- dist(normalized_data, method = "canberra") # distance matrix for canberra
fit_8 <- hclust(d3, method="complete")
plot(fit_8) # display dendrogram
plot(fit_8, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_8, k=9, border="red")
groups_8 <- cutree(fit_8, k=9) # cut tree into 9 clusters
membership_8<-as.matrix(groups_8) # groups or cluster numbers
final_8 <- data.frame(airline, membership_8)
View(final_8)
final1_8<-final_8[,c(ncol(final_8),1:(ncol(final_8)-1))]
View(final1_8) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_8, file="final_8.csv",row.names = F)
aggregate(airline[,-1],by=list(final_8$membership_8),mean)
#########################################################################
fit_9 <- hclust(d3, method="average")
plot(fit_9) # display dendrogram
plot(fit_9, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_9, k=9, border="red")
groups_9 <- cutree(fit_9, k=9) # cut tree into 9 clusters
membership_9<-as.matrix(groups_9) # groups or cluster numbers
final_9 <- data.frame(airline, membership_9)
View(final_9)
final1_9<-final_9[,c(ncol(final_9),1:(ncol(final_9)-1))]
View(final1_9) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_9, file="final_9.csv",row.names = F)
aggregate(airline[,-1],by=list(final_9$membership_9),mean)
#########################################################################
fit_10 <- hclust(d3, method="single")
plot(fit_10) # display dendrogram
plot(fit_10, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_10, k=9, border="red")
groups_10 <- cutree(fit_10, k=9) # cut tree into 9 clusters
membership_10<-as.matrix(groups_10) # groups or cluster numbers
final_10<-data.frame(airline, membership_10)
View(final_10)
final1_10<-final_10[,c(ncol(final_10),1:(ncol(final_10)-1))]
View(final1_10) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_10, file="final_10.csv",row.names = F)
aggregate(airline[,-1],by=list(final_10$membership_10),mean)
########################################################################
fit_11 <- hclust(d3, method="centroid")
plot(fit_11) # display dendrogram
plot(fit_11, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_11, k=9, border="red")
groups_11 <- cutree(fit_11, k=9) # cut tree into 9 clusters
membership_11<-as.matrix(groups_11) # groups or cluster numbers
final_11<-data.frame(airline, membership_11)
View(final_11)
final1_11<-final_11[,c(ncol(final_11),1:(ncol(final_11)-1))]
View(final1_11) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_11, file="final_11.csv",row.names = F)
aggregate(airline[,-1],by=list(final_11$membership_11),mean)
----------------------------------------------------------------------------
d4 <- dist(normalized_data, method = "maximum") # distance matrix for maximum
fit_12 <- hclust(d4, method="complete")
plot(fit_12) # display dendrogram
plot(fit_12, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_12, k=10, border="red")
groups_12 <- cutree(fit_12, k=10) # cut tree into 10 clusters
membership_12<-as.matrix(groups_12) # groups or cluster numbers
final_12 <- data.frame(airline, membership_12)
View(final_12)
final1_12<-final_12[,c(ncol(final_12),1:(ncol(final_12)-1))]
View(final1_12) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_12, file="final_12.csv",row.names = F)
aggregate(airline[,-1],by=list(final_12$membership_12),mean)
##################################################
fit_13 <- hclust(d4, method="average")
plot(fit_13) # display dendrogram
plot(fit_13, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_13, k=10, border="red")
groups_13 <- cutree(fit_13, k=10) # cut tree into 10 clusters
membership_13<-as.matrix(groups_13) # groups or cluster numbers
final_13 <- data.frame(airline, membership_13)
View(final_13)
final1_13<-final_13[,c(ncol(final_13),1:(ncol(final_13)-1))]
View(final1_13) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_13, file="final_13.csv",row.names = F)
aggregate(airline[,-1],by=list(final_13$membership_13),mean)
##################################################
fit_14 <- hclust(d4, method="single")
plot(fit_14) # display dendrogram
plot(fit_14, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_14, k=10, border="red")
groups_14 <- cutree(fit_14, k=10) # cut tree into 10 clusters
membership_14<-as.matrix(groups_14) # groups or cluster numbers
final_14 <- data.frame(airline, membership_14)
View(final_14)
final1_14<-final_14[,c(ncol(final_14),1:(ncol(final_14)-1))]
View(final1_14) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_14, file="final_14.csv",row.names = F)
aggregate(airline[,-1],by=list(final_14$membership_14),mean)
##################################################
fit_15 <- hclust(d4, method="centroid")
plot(fit_15) # display dendrogram
plot(fit_15, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_15, k=10, border="red")
groups_15 <- cutree(fit_15, k=10) # cut tree into 10 clusters
membership_15<-as.matrix(groups_15) # groups or cluster numbers
final_15 <- data.frame(airline, membership_15)
View(final_15)
final1_15<-final_15[,c(ncol(final_15),1:(ncol(final_15)-1))]
View(final1_15) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_15, file="final_15.csv",row.names = F)
aggregate(airline[,-1],by=list(final_15$membership_15),mean)
-----------------------------------------------------------------------------
d5 <- dist(normalized_data, method = "binary") # distance matrix for binary
fit_16 <- hclust(d5, method="complete")
plot(fit_16) # display dendrogram
plot(fit_16, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_16, k=2000, border="red")
groups_16 <- cutree(fit_16, k=2000) # cut tree into 2000 clusters
membership_16<-as.matrix(groups_16) # groups or cluster numbers
final_16 <- data.frame(airline, membership_16)
View(final_16)
final1_16<-final_16[,c(ncol(final_16),1:(ncol(final_16)-1))]
View(final1_16) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_16, file="final_16.csv",row.names = F)
aggregate(airline[,-1],by=list(final_16$membership_16),mean)
####################################################################
fit_17 <- hclust(d5, method="average")
plot(fit_17) # display dendrogram
plot(fit_17, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_17, k=2000, border="red")
groups_17 <- cutree(fit_17, k=2000) # cut tree into 2000 clusters
membership_17<-as.matrix(groups_17) # groups or cluster numbers
final_17 <- data.frame(airline, membership_17)
View(final_17)
final1_17<-final_17[,c(ncol(final_17),1:(ncol(final_17)-1))]
View(final1_17) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_17, file="final_17.csv",row.names = F)
aggregate(airline[,-1],by=list(final_17$membership_17),mean)
####################################################################
fit_18 <- hclust(d5, method="single")
plot(fit_18) # display dendrogram
plot(fit_18, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_18, k=2000, border="red")
groups_18 <- cutree(fit_18, k=2000) # cut tree into 2000 clusters
membership_18<-as.matrix(groups_18) # groups or cluster numbers
final_18 <- data.frame(airline, membership_18)
View(final_18)
final1_18<-final_18[,c(ncol(final_18),1:(ncol(final_18)-1))]
View(final1_18) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_18, file="final_18.csv",row.names = F)
aggregate(airline[,-1],by=list(final_18$membership_18),mean)
#######################################################################
fit_19 <- hclust(d5, method="centroid")
plot(fit_19) # display dendrogram
plot(fit_19, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_19, k=2000, border="red")
groups_19 <- cutree(fit_19, k=2000) # cut tree into 2000 clusters
membership_19<-as.matrix(groups_19) # groups or cluster numbers
final_19 <- data.frame(airline, membership_19)
View(final_19)
final1_19<-final_19[,c(ncol(final_19),1:(ncol(final_19)-1))]
View(final1_19) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_19, file="final_19.csv",row.names = F)
aggregate(airline[,-1],by=list(final_19$membership_19),mean)
------------------------------------------------------------------------------
d6 <- dist(normalized_data, method = "minkowski") # distance matrix for minkowski
fit_20 <- hclust(d6, method="complete")
plot(fit_20) # display dendrogram
plot(fit_20, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_20, k=35, border="red")
groups_20 <- cutree(fit_20, k=35) # cut tree into 35 clusters
membership_20<-as.matrix(groups_20) # groups or cluster numbers
final_20 <- data.frame(airline, membership_20)
View(final_20)
final1_20<-final_20[,c(ncol(final_20),1:(ncol(final_20)-1))]
View(final1_20) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_20, file="final_20.csv",row.names = F)
aggregate(airline[,-1],by=list(final_20$membership_20),mean)
#################################################################
fit_21 <- hclust(d6, method="average")
plot(fit_21) # display dendrogram
plot(fit_21, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_21, k=35, border="red")
groups_21 <- cutree(fit_21, k=35) # cut tree into 35 clusters
membership_21<-as.matrix(groups_21) # groups or cluster numbers
final_21 <- data.frame(airline, membership_21)
View(final_21)
final1_21<-final_21[,c(ncol(final_21),1:(ncol(final_21)-1))]
View(final1_21) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_21, file="final_21.csv",row.names = F)
aggregate(airline[,-1],by=list(final_21$membership_21),mean)
#################################################################
fit_22 <- hclust(d6, method="single")
plot(fit_22) # display dendrogram
plot(fit_22, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_22, k=35, border="red")
groups_22 <- cutree(fit_22, k=35) # cut tree into 35 clusters
membership_22<-as.matrix(groups_22) # groups or cluster numbers
final_22 <- data.frame(airline, membership_22)
View(final_22)
final1_22<-final_22[,c(ncol(final_22),1:(ncol(final_22)-1))]
View(final1_22) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_22, file="final_22.csv",row.names = F)
aggregate(airline[,-1],by=list(final_22$membership_22),mean)
################################################################
fit_23 <- hclust(d6, method="centroid")
plot(fit_23) # display dendrogram
plot(fit_23, hang=-1) ##Plot hierarchical clustering in a much better way so its easy for us to view
rect.hclust(fit_23, k=35, border="red")
groups_23 <- cutree(fit_23, k=35) # cut tree into 35 clusters
membership_23<-as.matrix(groups_23) # groups or cluster numbers
final_23 <- data.frame(airline, membership_23)
View(final_23)
final1_23<-final_23[,c(ncol(final_23),1:(ncol(final_23)-1))]
View(final1_23) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_23, file="final_23.csv",row.names = F)
aggregate(airline[,-1],by=list(final_23$membership_23),mean)
####Lets see K Means clustering
View(airline)
text(airline,rownames(airline))
plot(airline)
km<-kmeans(airline,7)  ###Randomly 7 clusters were chosen
str(km)
install.packages("animation")
library(animation)
km1<-kmeans.ani(airline,7)
str(km1)
#####Our objective is to find what is happening behind the scenes
###Elbow curve and k~sqrt(n/2) will decide the 'k' value
###In this way the distance between each and every data point for each and every centroid
## of cluster is calculated
##To which cluster centroid, a specific data point is close to it, it will go and form a cluster with it
wssplot <- function(normalized_data, nc=12, seed=1234){
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
k<-kselection(airline[,-1],parallel=TRUE,k_threshold=0.9)
?kselection
View(airline)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k<-kselection(airline[,-1],parallel=TRUE,k_threshold=0.9)
k####f(k) finds 2 cluster
##EastWestAirlines <- readxl::excel_format("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Clustering\\EastWestAirlines.xlsx")
View(EastWestAirlines)
mydata<-EastWestAirlines[1:3999,1:12]
View(mydata)
normalized_data<-scale(mydata[,2:12])
View(normalized_data)
fit<-kmeans(normalized_data,14)  #####14 cluster solution
str(fit)
final2<-data.frame(mydata,fit$cluster)
final2

