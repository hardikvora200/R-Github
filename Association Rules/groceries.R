library(arules)
groceries<-read.transactions("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Association Rules\\groceries.csv",format="basket")
View(groceries)
summary(groceries)
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=25)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
groceries_rules.sorted = sort(groceries_rules, by="lift")
subset.matrix1 = is.subset(groceries_rules.sorted, groceries_rules.sorted)
subset.matrix1[lower.tri(subset.matrix, diag=T)] <- NA
redundant1 = colSums(subset.matrix1, na.rm=T) >= 1
which(redundant1)
groceries_rules.pruned<-groceries_rules.sorted[!redundant1]
inspect(groceries_rules.pruned)
View(groceries_rules.pruned)
inspect(sort(groceries_rules,by="lift"))
library(arulesViz)
?plot()
library(colorspace)
plot(groceries_rules,method = "scatterplot",jitter = 0,control=list(col=sequential_hcl(100)))
plot(groceries_rules,method = "two-key plot",jitter = 0,control=list(main = "Two-key plot", col=rainbow(5)))
## The following techniques work better with fewer rules
subrules <- subset(groceries_rules, lift>2.5)
subrules
is.na("subrules")
plot(subrules,method = "matrix", measure="lift")
plot(subrules,method = "matrix3D")
oneRule <- sample(groceries_rules, 1)
plot(oneRule,method = "doubledecker",data=groceries)
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(oneRule,method = "mosaic",data=groceries)
### increasing minlen from 3 to 5#####
library(arules)
data("Groceries")
summary(Groceries)
inspect(Groceries[1:10])
rules <- apriori(groceries,parameter = list(support = 0.001,confidence = 0.05,minlen=3))
rules.sorted = sort(rules, by="lift")
subset.matrix2 = is.subset(rules.sorted, rules.sorted)
subset.matrix2[lower.tri(subset.matrix2, diag=T)] <- NA
redundant2 = colSums(subset.matrix2, na.rm=T) >= 1
which(redundant2)
rules.pruned2<-rules.sorted[!redundant2]
inspect(rules.pruned2)
View(rules.pruned2)
inspect(rules[1:50])
library(colorspace)
plot(rules,method = "scatterplot",jitter = 0,control=list(col=sequential_hcl(100)))
plot(rules,method = "two-key plot",jitter = 0,control=list(main = "Two-key plot", col=rainbow(5)))
## The following techniques work better with fewer rules
subrules <- subset(rules, lift>2.5)
subrules
is.na("subrules")
plot(subrules,method = "matrix", measure="lift")
plot(subrules,method = "matrix3D")
oneRule <- sample(rules, 1)
plot(oneRule,method = "doubledecker",data=groceries)
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(oneRule,method = "mosaic",data=groceries)
rules <- sort(rules,by="lift")
inspect(rules[1:4])
###############Increasing support to 0.003 keeping minimum length - 5
library(arules)
inspect(groceries[1:10])
rules_1 <- apriori(groceries,parameter = list(support = 0.003,confidence = 0.05,minlen=5))
rules_1.sorted = sort(rules_1, by="lift")
subset.matrix3 = is.subset(rules_1.sorted, rules_1.sorted)
subset.matrix3[lower.tri(subset.matrix3, diag=T)] <- NA
redundant3 = colSums(subset.matrix3, na.rm=T) >= 1
which(redundant3)
rules_1.pruned3<-rules_1.sorted[!redundant3]
inspect(rules_1.pruned3)
View(rules_1.pruned3)
inspect(rules_1[1:5])
library(colorspace)
plot(rules_1,method = "scatterplot",jitter = 0,control=list(col=sequential_hcl(100)))
plot(rules_1,method = "two-key plot",jitter = 0,control=list(main = "Two-key plot", col=rainbow(5)))
## The following techniques work better with fewer rules
subrules <- subset(rules_1, lift>2.5)
subrules
is.na("subrules")
plot(subrules,method = "matrix", measure="lift")
plot(subrules,method = "matrix3D")
oneRule <- sample(rules_1, 1)
plot(oneRule,method = "doubledecker",data=groceries)
plot(rules_1,method = "grouped")
plot(rules_1,method = "graph")
plot(oneRule,method = "mosaic",data=groceries)
rules <- sort(rules_1,by="lift")
inspect(rules[1:4])
############Increasing Confidence to 0.35 keeping support 0.002 and minimum length =3 
library(arules)
inspect(groceries[1:10])
rules_2 <- apriori(groceries,parameter = list(support = 0.002,confidence = 0.35,minlen=3))
rules_2.sorted = sort(rules_2, by="lift")
subset.matrix4 = is.subset(rules_2.sorted, rules_2.sorted)
subset.matrix4[lower.tri(subset.matrix4, diag=T)] <- NA
redundant4 = colSums(subset.matrix4, na.rm=T) >= 1
which(redundant4)
rules_2.pruned4<-rules_2.sorted[!redundant4]
inspect(rules_2.pruned4)
View(rules_2.pruned4)
inspect(rules_2[1:5])
library(colorspace)
plot(rules_2,method = "scatterplot",jitter = 0,control=list(col=sequential_hcl(100)))
plot(rules_2,method = "two-key plot",jitter = 0,control=list(main = "Two-key plot", col=rainbow(5)))
## The following techniques work better with fewer rules
subrules <- subset(rules_2, lift>2.5)
subrules
is.na("subrules")
plot(subrules,method = "matrix", measure="lift")
plot(subrules,method = "matrix3D")
oneRule <- sample(rules_2, 1)
plot(oneRule,method = "doubledecker",data=groceries)
plot(rules_2,method = "grouped")
plot(rules_2,method = "graph")
plot(oneRule,method = "mosaic",data=groceries)
rules <- sort(rules_2,by="lift")
inspect(rules[1:4])
