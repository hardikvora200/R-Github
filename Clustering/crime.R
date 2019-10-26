crime <- read.csv("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Clustering\\crime_data.csv")
View(crime)
attach(crime)
sum(is.na(crime))
crime <- na.omit(crime) # Omitting NA values from the Data if it is there
# na.omit => will omit the rows which has atleast 1 NA value
dim(crime)
colnames(crime)
-------------------------------------------------
#Measures of Central Tendency                Murder
mean(crime$Murder)  ### -7.788
median(crime$Murder)#####  7.25
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(crime$Murder)  ######   13.2
#Measures of Dispersion
var(crime$Murder)  ######  18.97047
sd(crime$Murder)  ####  4.35551
range(crime$Murder)##### 0.8  17.4
rangevalue <- function(x){max(x)-min(x)}
rangevalue(crime$Murder) #####  16.6
#Measures of skewness
install.packages("moments")
library(moments)
#Measures of skewness
skewness(crime$Murder)  #### 0.3820378
#Measures of Kurtosis 
kurtosis(crime$Murder)######   2.135329
#Graphical Representation:-Boxplot, Histogram, Barplot
boxplot(crime$Murder,horizontal = TRUE)
hist(crime$Murder)
barplot(crime$Murder)
str(crime)
#qqplot
qqnorm(crime$Murder)
qqline(crime$Murder)
logmur<-log(crime$Murder)
qqnorm(logmur)
qqline(logmur)
expmur<-exp(crime$Murder)
qqnorm(expmur)
qqline(expmur)###Normalisation achieved
sqrtmur<-sqrt(crime$Murder)
qqnorm(sqrtmur)
qqline(sqrtmur)### slightly Normalisation achieved
sqrmur<-(crime$Murder * crime$Murder)
qqnorm(sqrmur)
qqline(sqrmur)
install.packages(psych)
library(psych)
describe(crime)
# to calculate Z score
qnorm(0.950)#90%, 1.644854
qnorm(0.975)#95%, 1.959964
qnorm(0.995)#99%, 2.575829
pnorm(crime$Murder,-7.788,4.35551)
-----------------------------------------------------------------------
#Measures of Central Tendency                Assault
mean(crime$Assault)  ### -170.76
median(crime$Assault)##### 159
#mode
getmode(crime$Assault)  #####120
#Measures of Dispersion
var(crime$Assault)  ###### 6945.166
sd(crime$Assault)  ####  83.33766
range(crime$Assault) ##### 45  337
rangevalue <- function(x){max(x)-min(x)}
rangevalue(crime$Assault) ##### 292
#Measures of skewness
library(moments)
#Measures of skewness
skewness(crime$Assault)  ### 0.2273179
#Measures of Kurtosis 
kurtosis(crime$Assault)###### 1.93098
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(crime$Assault,horizontal = TRUE)
hist(crime$Assault)
barplot(crime$Assault)
#qqplot
qqnorm(crime$Assault)
qqline(crime$Assault)
expass<-exp(crime$Assault)
qqnorm(expass)
qqline(expass)  ####Normalisation achieved
pnorm(crime$Assault,-170.76,83.33766)
-----------------------------------------------------------------------    
#Measures of Central Tendency                UrbanPop
mean(crime$UrbanPop)  ### -65.54
median(crime$UrbanPop)##### 66
#mode
getmode(crime$UrbanPop)  ###### 80
#Measures of Dispersion
var(crime$UrbanPop)  ###### 209.5188
sd(crime$UrbanPop)  #### 14.47476
range(crime$UrbanPop)##### 32  91
rangevalue <- function(x){max(x)-min(x)}
rangevalue(crime$UrbanPop) #### 59
#Measures of skewness
skewness(crime$UrbanPop)  ### -0.2191719
#Measures of Kurtosis 
kurtosis(crime$UrbanPop)###### 2.21579
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(crime$UrbanPop,horizontal = TRUE)
hist(crime$UrbanPop)
barplot(crime$UrbanPop)
#qqplot
qqnorm(crime$UrbanPop)
qqline(crime$UrbanPop)
expup<-exp(crime$UrbanPop)
qqnorm(expup)
qqline(expup)   ########Normalisation achieved
pnorm(crime$UrbanPop,-65.54,14.47476)
------------------------------------------------------------------
#Measures of Central Tendency                rape
mean(crime$Rape)  ### 21.232 
median(crime$Rape)##### 20.1
#mode
getmode(crime$Rape)  ###### 16.3
#Measures of Dispersion
var(crime$Rape)  ###### 87.72916
sd(crime$Rape)  #### 9.366385
range(crime$Rape)##### 7.3  46.0
##rangevalue <- function(x){max(x)-min(x)}
rangevalue(crime$Rape) #### 38.7
#Measures of skewness
skewness(crime$Rape)  ### 0.7769613
#Measures of Kurtosis 
kurtosis(crime$Rape)###### 3.201898
#Graphical Representation :- Boxplot , Histogram, Barplot
boxplot(crime$Rape,horizontal = TRUE)
hist(crime$Rape)
barplot(crime$Rape)
##str(affair)
#qqplot
qqnorm(crime$Rape)
qqline(crime$Rape)
expra<-exp(crime$Rape)
qqnorm(expra)
qqline(expra)  ###Normalisation achieved
pnorm(crime$Rape,21.232,9.366385)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(crime[,2:5]) #excluding the university name columnbefore normalizing
View(normalized_data)
----------------------------------------------
d <- dist(normalized_data, method = "euclidean") # distance matrix for euclidean
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)
rect.hclust(fit, k=4, border="red")
groups <- cutree(fit, k=4) # cut tree into 4 clusters
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)
View(final)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1) ###Combine all the columns of final and place membership column in 1st column
write.csv(final, file="final.csv",row.names = F)
aggregate(crime[,-1],by=list(final$membership),mean)
###############################################################
fit_1 <- hclust(d, method="single")
plot(fit_1) # display dendrogram
plot(fit_1, hang=-1)
rect.hclust(fit_1, k=4, border="red")
groups_1 <- cutree(fit_1, k=4) # cut tree into 4 clusters
membership_1<-as.matrix(groups_1) # groups or cluster numbers
final_1 <- data.frame(crime, membership_1)
View(final_1)
final1_1<-final_1[,c(ncol(final_1),1:(ncol(final_1)-1))]
View(final1_1) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_1, file="final_1.csv",row.names = F)
aggregate(crime[,-1],by=list(final_1$membership_1),mean) 
###################################################################
fit_2 <- hclust(d, method="average")
plot(fit_2) # display dendrogram
plot(fit_2, hang=-1)
rect.hclust(fit_2, k=4, border="red")
groups_2 <- cutree(fit_2, k=4) # cut tree into 4 clusters
membership_2<-as.matrix(groups_2) # groups or cluster numbers
final_2 <- data.frame(crime, membership_2)
View(final_2)
final1_2<-final_2[,c(ncol(final_2),1:(ncol(final_2)-1))]
View(final1_2) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_2, file="final_2.csv",row.names = F)
aggregate(crime[,-1],by=list(final_2$membership_2),mean) 
####################################################################
fit_3 <- hclust(d, method="centroid")
plot(fit_3) # display dendrogram
plot(fit_3, hang=-1)
rect.hclust(fit_3, k=4, border="red")
groups_3 <- cutree(fit_3, k=4) # cut tree into 4 clusters
membership_3<-as.matrix(groups_3) # groups or cluster numbers
final_3 <- data.frame(crime, membership_3)
View(final_3)
final1_3<-final_3[,c(ncol(final_3),1:(ncol(final_3)-1))]
View(final1_3) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_3, file="final_3.csv",row.names = F)
aggregate(crime[,-1],by=list(final_3$membership_3),mean) 
-------------------------------------------------------------------
d2 <- dist(normalized_data, method = "manhattan") # distance matrix for manhattan
fit_4 <- hclust(d2, method="complete")
plot(fit_4) # display dendrogram
plot(fit_4, hang=-1)
rect.hclust(fit_4, k=4, border="red")
groups_4 <- cutree(fit_4, k=4) # cut tree into 4 clusters
membership_4<-as.matrix(groups_4) # groups or cluster numbers
final_4 <- data.frame(crime, membership_4)
View(final_4)
final1_4<-final_4[,c(ncol(final_4),1:(ncol(final_4)-1))]
View(final1_4) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_4, file="final_4.csv",row.names = F)
aggregate(crime[,-1],by=list(final_4$membership_4),mean)
################################################################
fit_5 <- hclust(d2, method="single")
plot(fit_5) # display dendrogram
plot(fit_5, hang=-1)
rect.hclust(fit_5, k=4, border="red")
groups_5 <- cutree(fit_5, k=4) # cut tree into 4 clusters
membership_5<-as.matrix(groups_5) # groups or cluster numbers
final_5 <- data.frame(crime, membership_5)
View(final_5)
final1_5<-final_5[,c(ncol(final_5),1:(ncol(final_5)-1))]
View(final1_5) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_5, file="final_5.csv",row.names = F)
aggregate(crime[,-1],by=list(final_5$membership_5),mean)
#######################################################################
fit_6 <- hclust(d2, method="average")
plot(fit_6) # display dendrogram
plot(fit_6, hang=-1)
rect.hclust(fit_6, k=4, border="red")
groups_6 <- cutree(fit_6, k=4) # cut tree into 4 clusters
membership_6<-as.matrix(groups_6) # groups or cluster numbers
final_6 <- data.frame(crime, membership_6)
View(final_6)
final1_6<-final_6[,c(ncol(final_6),1:(ncol(final_6)-1))]
View(final1_6) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_6, file="final_6.csv",row.names = F)
aggregate(crime[,-1],by=list(final_6$membership_6),mean)
###############################################################
fit_7 <- hclust(d2, method="centroid")
plot(fit_7) # display dendrogram
plot(fit_7, hang=-1)
rect.hclust(fit_7, k=4, border="red")
groups_7 <- cutree(fit_7, k=4) # cut tree into 4 clusters
membership_7<-as.matrix(groups_7) # groups or cluster numbers
final_7 <- data.frame(crime, membership_7)
View(final_7)
final1_7<-final_7[,c(ncol(final_7),1:(ncol(final_7)-1))]
View(final1_7) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_7, file="final_7.csv",row.names = F)
aggregate(crime[,-1],by=list(final_7$membership_7),mean)
-----------------------------------------------------------------
d3 <- dist(normalized_data, method = "maximum") # distance matrix for maximum
fit_8 <- hclust(d3, method="complete")
plot(fit_8) # display dendrogram
plot(fit_8, hang=-1)
rect.hclust(fit_8, k=4, border="red")
groups_8 <- cutree(fit_8, k=4) # cut tree into 4 clusters
membership_8<-as.matrix(groups_8) # groups or cluster numbers
final_8 <- data.frame(crime, membership_8)
View(final_8)
final1_8<-final_8[,c(ncol(final_8),1:(ncol(final_8)-1))]
View(final1_8) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_8, file="final_8.csv",row.names = F)
aggregate(crime[,-1],by=list(final_8$membership_8),mean)
###################################################################
fit_9 <- hclust(d3, method="single")
plot(fit_9) # display dendrogram
plot(fit_9, hang=-1)
rect.hclust(fit_9, k=4, border="red")
groups_9 <- cutree(fit_9, k=4) # cut tree into 4 clusters
membership_9<-as.matrix(groups_9) # groups or cluster numbers
final_9 <- data.frame(crime, membership_9)
View(final_9)
final1_9<-final_9[,c(ncol(final_9),1:(ncol(final_9)-1))]
View(final1_9) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_9, file="final_9.csv",row.names = F)
aggregate(crime[,-1],by=list(final_9$membership_9),mean)
###################################################################
fit_10 <- hclust(d3, method="average")
plot(fit_10) # display dendrogram
plot(fit_10, hang=-1)
rect.hclust(fit_10, k=4, border="red")
groups_10 <- cutree(fit_10, k=4) # cut tree into 4 clusters
membership_10<-as.matrix(groups_10) # groups or cluster numbers
final_10 <- data.frame(crime, membership_10)
View(final_10)
final1_10<-final_10[,c(ncol(final_10),1:(ncol(final_10)-1))]
View(final1_10) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_10, file="final_10.csv",row.names = F)
aggregate(crime[,-1],by=list(final_10$membership_10),mean)
#######################################################################
fit_11 <- hclust(d3, method="centroid")
plot(fit_11) # display dendrogram
plot(fit_11, hang=-1)
rect.hclust(fit_11, k=4, border="red")
groups_11 <- cutree(fit_11, k=4) # cut tree into 4 clusters
membership_11<-as.matrix(groups_11) # groups or cluster numbers
final_11 <- data.frame(crime, membership_11)
View(final_11)
final1_11<-final_11[,c(ncol(final_11),1:(ncol(final_11)-1))]
View(final1_11) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_11, file="final_11.csv",row.names = F)
aggregate(crime[,-1],by=list(final_11$membership_11),mean)
-----------------------------------------------------------------------
d4 <- dist(normalized_data, method = "binary") # distance matrix for binary
fit_12 <- hclust(d4, method="complete")
plot(fit_12) # display dendrogram
plot(fit_12, hang=-1)
rect.hclust(fit_12, k=34, border="red")
groups_12 <- cutree(fit_12, k=34) # cut tree into 34 clusters
membership_12<-as.matrix(groups_12) # groups or cluster numbers
final_12 <- data.frame(crime, membership_12)
View(final_12)
final1_12<-final_12[,c(ncol(final_12),1:(ncol(final_12)-1))]
View(final1_12) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_12, file="final_12.csv",row.names = F)
aggregate(crime[,-1],by=list(final_12$membership_12),mean)
###################################################################
fit_13 <- hclust(d4, method="single")
plot(fit_13) # display dendrogram
plot(fit_13, hang=-1)
rect.hclust(fit_13, k=34, border="red")
groups_13 <- cutree(fit_13, k=34) # cut tree into 34 clusters
membership_13<-as.matrix(groups_13) # groups or cluster numbers
final_13 <- data.frame(crime, membership_13)
View(final_13)
final1_13<-final_13[,c(ncol(final_13),1:(ncol(final_13)-1))]
View(final1_13) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_13, file="final_13.csv",row.names = F)
aggregate(crime[,-1],by=list(final_13$membership_13),mean)
####################################################################
fit_14 <- hclust(d4, method="average")
plot(fit_14) # display dendrogram
plot(fit_14, hang=-1)
rect.hclust(fit_14, k=34, border="red")
groups_14 <- cutree(fit_14, k=34) # cut tree into 34 clusters
membership_14<-as.matrix(groups_14) # groups or cluster numbers
final_14 <- data.frame(crime, membership_14)
View(final_14)
final1_14<-final_14[,c(ncol(final_14),1:(ncol(final_14)-1))]
View(final1_14) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_14, file="final_14.csv",row.names = F)
aggregate(crime[,-1],by=list(final_14$membership_14),mean)
####################################################################
fit_15 <- hclust(d4, method="centroid")
plot(fit_15) # display dendrogram
plot(fit_15, hang=-1)
rect.hclust(fit_15, k=34, border="red")
groups_15 <- cutree(fit_15, k=34) # cut tree into 34 clusters
membership_15<-as.matrix(groups_15) # groups or cluster numbers
final_15 <- data.frame(crime, membership_15)
View(final_15)
final1_15<-final_15[,c(ncol(final_15),1:(ncol(final_15)-1))]
View(final1_15) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_15, file="final_15.csv",row.names = F)
aggregate(crime[,-1],by=list(final_15$membership_15),mean)
----------------------------------------------------------------------------
d5 <- dist(normalized_data, method = "canberra") # distance matrix for canberra
fit_16 <- hclust(d5, method="complete")
plot(fit_16) # display dendrogram
plot(fit_16, hang=-1)
rect.hclust(fit_16, k=8, border="red")
groups_16 <- cutree(fit_16, k=8) # cut tree into 8 clusters
membership_16<-as.matrix(groups_16) # groups or cluster numbers
final_16 <- data.frame(crime, membership_16)
View(final_16)
final1_16<-final_16[,c(ncol(final_16),1:(ncol(final_16)-1))]
View(final1_16) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_16, file="final_16.csv",row.names = F)
aggregate(crime[,-1],by=list(final_16$membership_16),mean)
#######################################################################
fit_17 <- hclust(d5, method="single")
plot(fit_17) # display dendrogram
plot(fit_17, hang=-1)
rect.hclust(fit_17, k=8, border="red")
groups_17 <- cutree(fit_17, k=8) # cut tree into 8 clusters
membership_17<-as.matrix(groups_17) # groups or cluster numbers
final_17 <- data.frame(crime, membership_17)
View(final_17)
final1_17<-final_17[,c(ncol(final_17),1:(ncol(final_17)-1))]
View(final1_17) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_17, file="final_17.csv",row.names = F)
aggregate(crime[,-1],by=list(final_17$membership_17),mean)
###################################################################
fit_18 <- hclust(d5, method="average")
plot(fit_18) # display dendrogram
plot(fit_18, hang=-1)
rect.hclust(fit_18, k=8, border="red")
groups_18 <- cutree(fit_18, k=8) # cut tree into 8 clusters
membership_18<-as.matrix(groups_18) # groups or cluster numbers
final_18 <- data.frame(crime, membership_18)
View(final_18)
final1_18<-final_18[,c(ncol(final_18),1:(ncol(final_18)-1))]
View(final1_18) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_18, file="final_18.csv",row.names = F)
aggregate(crime[,-1],by=list(final_18$membership_18),mean)
###########################################################################
fit_19 <- hclust(d5, method="centroid")
plot(fit_19) # display dendrogram
plot(fit_19, hang=-1)
rect.hclust(fit_19, k=8, border="red")
groups_19 <- cutree(fit_19, k=8) # cut tree into 8 clusters
membership_19<-as.matrix(groups_19) # groups or cluster numbers
final_19 <- data.frame(crime, membership_19)
View(final_19)
final1_19<-final_19[,c(ncol(final_19),1:(ncol(final_19)-1))]
View(final1_19) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_19, file="final_19.csv",row.names = F)
aggregate(crime[,-1],by=list(final_19$membership_19),mean)
-------------------------------------------------------------------------
d6 <- dist(normalized_data, method = "minkowski") # distance matrix for minkowski
fit_20 <- hclust(d6, method="complete")
plot(fit_20) # display dendrogram
plot(fit_20, hang=-1)
rect.hclust(fit_20, k=14, border="red")
groups_20 <- cutree(fit_20, k=14) # cut tree into 14 clusters
membership_20<-as.matrix(groups_20) # groups or cluster numbers
final_20 <- data.frame(crime, membership_20)
View(final_20)
final1_20<-final_20[,c(ncol(final_20),1:(ncol(final_20)-1))]
View(final1_20) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_20, file="final.csv",row.names = F)
aggregate(crime[,-1],by=list(final_20$membership_20),mean)
####################################################################
fit_21 <- hclust(d6, method="single")
plot(fit_21) # display dendrogram
plot(fit_21, hang=-1)
rect.hclust(fit_21, k=14, border="red")
groups_21 <- cutree(fit_21, k=14) # cut tree into 14 clusters
membership_21<-as.matrix(groups_21) # groups or cluster numbers
final_21 <- data.frame(crime, membership_21)
View(final_21)
final1_21<-final_21[,c(ncol(final_21),1:(ncol(final_21)-1))]
View(final1_21) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_21, file="final_21.csv",row.names = F)
aggregate(crime[,-1],by=list(final_21$membership_21),mean)
#####################################################################
fit_22 <- hclust(d6, method="average")
plot(fit_22) # display dendrogram
plot(fit_22, hang=-1)
rect.hclust(fit_22, k=14, border="red")
groups_22 <- cutree(fit_22, k=14) # cut tree into 14 clusters
membership_22<-as.matrix(groups_22) # groups or cluster numbers
final_22 <- data.frame(crime, membership_22)
View(final_22)
final1_22<-final_22[,c(ncol(final_22),1:(ncol(final_22)-1))]
View(final1_22) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_22, file="final_22.csv",row.names = F)
aggregate(crime[,-1],by=list(final_22$membership_22),mean)
#####################################################################
fit_23 <- hclust(d6, method="centroid")
plot(fit_23) # display dendrogram
plot(fit_23, hang=-1)
rect.hclust(fit_23, k=14, border="red")
groups_23 <- cutree(fit_23, k=14) # cut tree into 14 clusters
membership_23<-as.matrix(groups_23) # groups or cluster numbers
final_23 <- data.frame(crime, membership_23)
View(final_23)
final1<-final_23[,c(ncol(final_23),1:(ncol(final_23)-1))]
View(final1_23) ###Combine all the columns of final and place membership column in 1st column
write.csv(final_23, file="final_23.csv",row.names = F)
aggregate(crime[,-1],by=list(final_23$membership_23),mean)
