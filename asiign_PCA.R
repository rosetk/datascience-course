install.packages("gdata")
install.packages("xlsx")



library(gdata)
library(xlsx)

input<-read.csv("D:/datascience/assignment/PCA/wine.csv",1)
#help(princomp)
data <- input[,-1]
attach(data)
cor(data)
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj)

biplot(pcaObj)
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$scores[,1:3]
input<-cbind(input,pcaObj$scores[,1:3])
View(input)
clus_data<-input[,15:17]
norm_clus<-scale(clus_data) 
dist1<-dist(norm_clus,method = "euclidean") 


fit1<-hclust(dist1,method="complete") 
plot(fit1) 

groups<-cutree(fit1,5) 

membership_1<-as.matrix(groups)

View(membership_1)

final1<-cbind(membership_1,input)
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) 

write.csv(final1,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()

# k means clustering for assignment
#elbow curve and k ~ sqrt(n/2)  to decide the k value 


normalised_data<-scale(pcaObj$scores[,1:3])
wss=(nrow(normalised_data)-1)*sum(apply(normalised_data,2,var))
for(i in 2:8)wss[i] = sum(kmeans(normalised_data,centers=i)$withinss)
plot(1:8 , wss, type="b",xlab="number of clusters", ylab = "within groups sum of squares")
title(sub="k-Means Clustering Scree Plot")



