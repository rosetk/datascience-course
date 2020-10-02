install.packages("plyr")
library(plyr)
x<-runif(50)
x

y<-runif(50)
y

data<-cbind(x,y)
data

plot(data)

plot (data, type ="n")

text(data, rownames(data))

km<-kmeans(data,4)

str(km)

install.packages("animation")
library(animation)

km1<-kmeans.ani(data,4)
str(km1)
km$cluster
km$centers

#  assignment 1

input<-read.csv("D:/datascience/assignment/clustering/crime_data.csv",1)
normalised_data<-scale(input[,2:5])

#  assignment 2

install.packages("xlsx")
library(xlsx)
input<-read.xlsx("D:/datascience/assignment/clustering/EastWestAirlines.xlsx",2)
normalised_data<-scale(input[,2:11])

# k means clustering for assignment
#elbow curve and k ~ sqrt(n/2)  to decide the k value 

wss=(nrow(normalised_data)-1)*sum(apply(normalised_data,2,var))
for(i in 2:8)wss[i] = sum(kmeans(normalised_data,centers=i)$withinss)
plot(1:8 , wss, type="b",xlab="number of clusters", ylab = "within groups sum of squares")
title(sub="k-Means Clustering Scree Plot")



fit<-kmeans (normalised_data,5)
str(fit)
final2<-data.frame (input, fit$cluster)
final2
final3<-final2[,c(ncol(final2),1:(ncol(final2)-1))]
t=aggregate(input[,2:11],by=list(fit$cluster),FUN=mean)


# selecting k for kmeans clustering using kselection

install.packages("kselection")
library(kselection)
k<-kselection (iris[,-5],parallel=TRUE,k_threshold=0.9,max_centers=12)
?kselection
?iris


# using parallel processing
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=2)
k<-kselection (iris[,-5],parallel=TRUE,k_threshold=0.9,max_centers=12)


# k clustering alternative forlarge data set - Clustering Large Application (CLARA)

install.packages("cluster")
library(cluster)
xds<-rbind(cbind(rnorm(5000,0,8),rnorm(5000,0,8)), cbind(rnorm(5000, 50 ,8), rnorm(5000,50,8)))
xcl<-clara(xds,2,sample=100)
clusplot(xcl)


# Partitioning  around medioids

xpm<-pam(xds,2)
clusplot(xpm)



