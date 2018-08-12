#Hierarchial clustering
getwd()
set.seed(42)
setwd("C:/Users/aaroh/Documents/BA with R")
pharm.df <- read.csv("Pharmaceuticals(1).csv",header = TRUE) 
row.names(pharm.df) <- pharm.df[,1] 
# removing non-numerical values
pharm.df <- pharm.df[,-c(1,2,12,13,14)] 
library("VIM") 
aggr(pharm.df) 
summary(pharm.df)
boxplot(pharm.df, main="basic financial measures in pharmaceutical industry", xlab="financial measures", ylab="values of financial measures") 
boxplot(pharm.df$Market_Cap)
boxplot(pharm.df$Beta)
boxplot(pharm.df$PE_Ratio)
boxplot(pharm.df$ROE)
boxplot(pharm.df$ROA)
boxplot(pharm.df$Asset_Turnover)
boxplot(pharm.df$Leverage)
boxplot(pharm.df$Rev_Growth)
pharm.df$Market_Cap[pharm.df$Market_Cap > 73.84 + 1.5*IQR(pharm.df$Market_Cap)] <- 73.84 + 1.5*IQR(pharm.df$Market_Cap)
pharm.df$Leverage[pharm.df$Leverage > 0.6+1.5*IQR(pharm.df$Leverage)] <- 0.6+1.5*IQR(pharm.df$Leverage)
pharm.df$PE_Ratio[pharm.df$PE_Ratio > 27.90+ 1.5*IQR(pharm.df$PE_Ratio)] <- 27.90+ 1.5*IQR(pharm.df$PE_Ratio)
boxplot(pharm.df, main="basic financial measures in pharmaceutical industry", xlab="financial measures", ylab="values of financial measures") 
pharm.df.norm <- sapply(pharm.df,scale)
boxplot(pharm.df.norm,main="Normalized: basic financial measures in pharmaceutical industry", xlab="financial measures", ylab="values of financial measures") 
library(NbClust) 
devAskNewPage(ask=TRUE) 
nc <- NbClust(pharm.df.norm, distance="euclidean", min.nc=2, max.nc=10, method="average") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria") 
d <- dist(pharm.df.norm) 
fit.average <- hclust(d, method="average")
plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering") 
clusters <- cutree(fit.average, k=2) 
table(clusters) 
aggregate(pharm.df.norm, by=list(cluster=clusters), median) 
rect.hclust(fit.average, k=2) 
table(clusters)
aggregate(pharm.df.norm,by=list(cluster=clusters),median)
set.seed(42)
nc <- NbClust(pharm.df.norm, min.nc=2, max.nc=10, method="kmeans") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria") 
fit.km <- kmeans(pharm.df.norm, 2, nstart=25) 
fit.km$size 
fit.km$centers 
wssplot <- function(data, nc=10, seed=42){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(42) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")} 
wssplot(pharm.df.norm,nc=10,seed=42) 










