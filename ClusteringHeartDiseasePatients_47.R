# # install pre-requisite packages #
# install.packages("psych")
# install.packages("corrplot")
# install.packages("scales")
# install.packages("cluster")
# install.packages("stats")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("tidyverse")
# install.packages("cluster")
# install.packages("fpc")
# install.packages("factoextra")

#-- Load required libraries --#
library(psych)
library(corrplot)
library(scales)
library(cluster)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(tidyverse)
library(cluster)
library(fpc)
library(dbscan)
library(factoextra)

#-- import dataset --#
mydata<-read.table("Heartdiseas.txt",sep=",", header=TRUE)

#-- drop 'mydata$id' --#
mydata = mydata[,-1]

#-- show statistics --#
head(mydata)
summary(mydata)
describe(mydata)

-----------------------------------------------------------------

#-- correlation between columns --#
corrplot(cor(mydata) , method = "number",type = "upper")
corrplot(cor(mydata), method = "color", type = "upper")

-----------------------------------------------------------------

#-- boxplot for data  --#
boxplot(mydata)
boxplot(mydata$chol , main="chol")
boxplot(mydata$trestbps, main="trestbps")
boxplot(mydata$thalach, main="thalach")
boxplot(mydata$oldpeak, main="oldpeak")
boxplot(mydata$age, main="age")

-----------------------------------------------------------------

#-- Detect & Remove outliers from the dataset --#
#---------------------- chol ----------------------#
#Before
boxplot(mydata$chol, main="chol - before")
plot(density(mydata$chol))
# remove Outliers for chol
mydata <- subset(mydata, mydata$chol  > 180 & mydata$chol < 320)
#After
boxplot(mydata$chol, main="chol - after")
plot(density(mydata$chol))
#---------------------- trestbps ----------------------#
#Before
boxplot(mydata$trestbps, main="trestbps - before")
plot(density(mydata$trestbps))
# remove Outliers for chol
mydata <- subset(mydata, mydata$trestbps  > 100 & mydata$trestbps < 160)
#After
boxplot(mydata$trestbps, main="trestbps - after")
plot(density(mydata$trestbps))
#---------------------- thalach ----------------------#
#Before
boxplot(mydata$thalach, main="thalach - before")
plot(density(mydata$thalach))
# remove Outliers for chol
mydata <- subset(mydata, mydata$thalach  > 100 & mydata$thalach < 200)
#After
boxplot(mydata$thalach, main="thalach - after")
plot(density(mydata$thalach))
#---------------------- oldpeak ----------------------#
#Before
boxplot(mydata$oldpeak, main="oldpeak - before")
plot(density(mydata$oldpeak))
# remove Outliers for chol
mydata <- subset(mydata,  mydata$oldpeak < 3.9)
#After
boxplot(mydata$oldpeak, main="oldpeak - after")
plot(density(mydata$oldpeak))

#---------------------- age ----------------------#
#Before
boxplot(mydata$age, main="age - before")
plot(density(mydata$age))
# remove Outliers for chol
mydata <- subset(mydata, mydata$age  > 40 & mydata$age < 70)
#After
boxplot(mydata$age, main="age - after")
plot(density(mydata$age))
#---------------------- restecg ----------------------#
#Before
plot(mydata$restecg, main="restecg - before")
# remove Outliers for restecg
mydata <- subset(mydata,  mydata$restecg != 1)
#After
plot(mydata$restecg, main="restecg - after")

-----------------------------------------------------------------

#-- WSS Plot (number of clusters) --#
fviz_nbclust(mydata, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

#-- Scaling the dataset --#
scldata = mydata
scldata = scale(scldata)

#======================= K-means ======================= #
km <- kmeans(scldata,2)
#-- Visualize K-means Clusters
autoplot(km ,scldata,label = FALSE ,frame=TRUE, loadings = FALSE,loadings.label = FALSE, main="K-means Clusters")

#==================== Hierarchical ==================== #
hir <- hclust(dist(scldata))
#-- Visualize Hierarchical Clusters
fviz_dend(hir ,k = 2,show_labels = FALSE, main="Hierarchical Clusters")

#======================= DBScan ======================= #
kNNdistplot(scldata, k=3)
abline(3, 0 ,col="blue")
set.seed(123)
db <- dbscan::dbscan(scldata, eps = 3, MinPts =3)
db
#-- Visualize DBScan Clusters
fviz_cluster(db, scldata, geom = "point", main="DBScan Clusters")


#= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #
#= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = #